#!/usr/bin/env bash
# install.sh — One-command setup for the llm-arc AI stack
#
# What it does:
#   1. Run host prerequisite checks
#   2. Detect GPU, set env vars
#   3. Build container images (ollama-ipex or ollama-vulkan, whisper-sycl)
#   4. Copy Quadlet files to ~/.config/containers/systemd/
#   5. Create model directories
#   6. systemctl --user daemon-reload
#   7. Start the stack
#   8. Pull recommended starter model
#   9. Run smoke test
#  10. Print Emacs setup instructions
#
# Usage: ./install.sh [--skip-build] [--skip-start]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/scripts/env.sh"

SKIP_BUILD=0
SKIP_START=0
for arg in "$@"; do
    case "$arg" in
        --skip-build) SKIP_BUILD=1 ;;
        --skip-start) SKIP_START=1 ;;
        --help|-h)
            echo "Usage: ./install.sh [--skip-build] [--skip-start]"
            exit 0
            ;;
    esac
done

echo ""
echo "╔══════════════════════════════════════════════════════╗"
echo "║          llm-arc — Local AI Stack Installer          ║"
echo "║     Intel Arc / NVIDIA GPU · Podman · Emacs          ║"
echo "╚══════════════════════════════════════════════════════╝"
echo ""

# ── Step 1: Host prerequisite checks ────────────────────────────
log_step "Step 1/10: Checking host prerequisites"

# Quick checks (don't abort on failure, just warn)
if ! command -v podman &>/dev/null; then
    log_error "Podman not installed. Install it first: sudo dnf install podman"
    exit 1
fi

# Vendor-aware GPU access check
if command -v nvidia-smi &>/dev/null && nvidia-smi &>/dev/null 2>&1; then
    log_info "NVIDIA GPU detected (render group and /dev/dri not required)"
else
    if ! id -nG | tr ' ' '\n' | grep -qx render; then
        log_warn "User not in 'render' group. Run: sudo usermod -aG render $USER"
    fi
    if ! compgen -G "/dev/dri/renderD*" >/dev/null; then
        log_error "No GPU render nodes found at /dev/dri/renderD*"
        exit 1
    fi
fi

require_cmd curl
log_ok "Prerequisites OK"

# ── Step 2: Detect GPU ───────────────────────────────────────────
log_step "Step 2/10: Detecting GPU"

bash "${LLM_ARC_SCRIPTS}/detect-gpu.sh" --save
source "${LLM_ARC_CONFIG}/gpu.env"

log_ok "GPU: $LLM_ARC_GPU_NAME ($LLM_ARC_GPU_CLASS)"
log_info "Backend: $LLM_ARC_GPU_BACKEND | VRAM: ~${LLM_ARC_GPU_VRAM_MB}MB"

# ── Step 3: Build container images ───────────────────────────────
if [[ $SKIP_BUILD -eq 0 ]]; then
    log_step "Step 3/10: Building container images"

    case "${LLM_ARC_GPU_VENDOR:-intel}" in
        nvidia)
            log_info "NVIDIA GPU detected — using stock Ollama image (has CUDA)"
            log_info "Pulling Ollama CUDA image..."
            podman pull "$LLM_ARC_OLLAMA_CUDA_IMAGE"
            log_ok "Ollama CUDA image pulled"

            log_info "Building Whisper CUDA image..."
            podman build -t whisper-cuda:latest \
                -f "${LLM_ARC_CONTAINERS}/Containerfile.whisper-cuda" \
                "${LLM_ARC_DIR}"
            log_ok "whisper-cuda:latest built"
            ;;
        *)
            if [[ "$LLM_ARC_GPU_BACKEND" == "vulkan" ]]; then
                log_info "Building Vulkan Ollama image..."
                podman build -t ollama-vulkan:latest \
                    -f "${LLM_ARC_CONTAINERS}/Containerfile.ollama-vulkan" \
                    "${LLM_ARC_DIR}"
                log_ok "ollama-vulkan:latest built"
            else
                log_info "Building IPEX-LLM Ollama image..."
                podman build -t ollama-ipex:latest \
                    -f "${LLM_ARC_CONTAINERS}/Containerfile.ollama-ipex" \
                    "${LLM_ARC_DIR}"
                log_ok "ollama-ipex:latest built"
            fi

            log_info "Building Whisper SYCL image..."
            podman build -t whisper-sycl:latest \
                -f "${LLM_ARC_CONTAINERS}/Containerfile.whisper-sycl" \
                "${LLM_ARC_DIR}"
            log_ok "whisper-sycl:latest built"
            ;;
    esac

    log_info "Pulling Piper TTS image..."
    podman pull "$LLM_ARC_PIPER_IMAGE"
    log_ok "Piper TTS image pulled"

    log_info "Pulling Open WebUI image..."
    podman pull "$LLM_ARC_WEBUI_IMAGE"
    log_ok "Open WebUI image pulled"
else
    log_step "Step 3/10: Skipping image build (--skip-build)"
fi

# ── Step 4: Install Quadlet files ────────────────────────────────
log_step "Step 4/10: Installing Quadlet unit files"

mkdir -p "$LLM_ARC_QUADLET_DIR"

for f in "${LLM_ARC_QUADLET}"/*.{pod,container}; do
    [[ -f "$f" ]] || continue
    local_basename=$(basename "$f")

    if [[ "${LLM_ARC_GPU_VENDOR:-intel}" == "nvidia" ]]; then
        # NVIDIA: rewrite ollama and whisper container units
        case "$local_basename" in
            ollama.container)
                sed -e 's|Image=localhost/ollama-ipex:latest|Image=docker.io/ollama/ollama:latest|' \
                    -e 's|Description=.*|Description=Ollama LLM Server (NVIDIA CUDA GPU)|' \
                    -e 's|AddDevice=/dev/dri.*|AddDevice=nvidia.com/gpu=all|' \
                    -e '/GroupAdd=keep-groups/d' \
                    -e '/SYCL_CACHE_PERSISTENT/d' \
                    -e '/SYCL_PI_LEVEL_ZERO/d' \
                    -e '/ZES_ENABLE_SYSMAN/d' \
                    -e '/BIGDL_LLM_XMX_DISABLED/d' \
                    -e '/Volume=.*sycl-cache/d' \
                    "$f" > "${LLM_ARC_QUADLET_DIR}/${local_basename}"
                # Add NVIDIA env vars
                sed -i '/OLLAMA_NUM_GPU/a Environment=NVIDIA_VISIBLE_DEVICES=all\nEnvironment=NVIDIA_DRIVER_CAPABILITIES=compute,utility' \
                    "${LLM_ARC_QUADLET_DIR}/${local_basename}"
                ;;
            whisper.container)
                sed -e 's|Image=localhost/whisper-sycl:latest|Image=localhost/whisper-cuda:latest|' \
                    -e 's|Description=.*|Description=Whisper Speech-to-Text Server (NVIDIA CUDA GPU)|' \
                    -e 's|AddDevice=/dev/dri.*|AddDevice=nvidia.com/gpu=all|' \
                    -e '/GroupAdd=keep-groups/d' \
                    -e '/ONEAPI_DEVICE_SELECTOR/d' \
                    -e '/ZES_ENABLE_SYSMAN/d' \
                    -e '/SYCL_CACHE_PERSISTENT/d' \
                    "$f" > "${LLM_ARC_QUADLET_DIR}/${local_basename}"
                # Add NVIDIA env vars
                sed -i '/^\[Container\]/a Environment=NVIDIA_VISIBLE_DEVICES=all\nEnvironment=NVIDIA_DRIVER_CAPABILITIES=compute,utility' \
                    "${LLM_ARC_QUADLET_DIR}/${local_basename}"
                ;;
            *)
                cp "$f" "${LLM_ARC_QUADLET_DIR}/${local_basename}"
                ;;
        esac
    elif [[ "$local_basename" == "ollama.container" && "$LLM_ARC_GPU_BACKEND" == "vulkan" ]]; then
        # Intel Vulkan: existing behavior
        sed 's|Image=localhost/ollama-ipex:latest|Image=localhost/ollama-vulkan:latest|' \
            "$f" > "${LLM_ARC_QUADLET_DIR}/${local_basename}"
    else
        cp "$f" "${LLM_ARC_QUADLET_DIR}/${local_basename}"
    fi
    log_ok "  → ${local_basename}"
done

# ── Step 5: Create model directories ────────────────────────────
log_step "Step 5/10: Creating model directories"

mkdir -p "$LLM_ARC_OLLAMA_MODELS" \
         "$LLM_ARC_WHISPER_MODELS" \
         "$LLM_ARC_PIPER_MODELS" \
         "${LLM_ARC_MODELS}/sycl-cache" \
         "${LLM_ARC_MODELS}/openwebui"

log_ok "Model directories created at $LLM_ARC_MODELS"

# ── Step 6: Reload systemd ──────────────────────────────────────
log_step "Step 6/10: Reloading systemd"

systemctl --user daemon-reload

# Enable linger if not already
if [[ ! -f "/var/lib/systemd/linger/$USER" ]]; then
    log_info "Enabling loginctl linger for boot persistence..."
    loginctl enable-linger "$USER" 2>/dev/null || \
        log_warn "Could not enable linger. Services won't auto-start on boot."
fi

log_ok "systemd reloaded"

# ── Step 7: Start the stack ──────────────────────────────────────
if [[ $SKIP_START -eq 0 ]]; then
    log_step "Step 7/10: Starting AI stack"

    systemctl --user start ai-stack-pod.service
    log_info "Waiting for services to initialize..."
    sleep 5

    # Show status
    bash "${LLM_ARC_SCRIPTS}/ai-stack.sh" status
else
    log_step "Step 7/10: Skipping start (--skip-start)"
fi

# ── Step 8: Pull starter model ───────────────────────────────────
if [[ $SKIP_START -eq 0 ]]; then
    log_step "Step 8/10: Pulling starter model"

    # Wait for Ollama to be ready
    local_url="http://127.0.0.1:${LLM_ARC_OLLAMA_PORT}"
    retries=30
    while (( retries > 0 )); do
        if curl -sf "${local_url}/api/tags" >/dev/null 2>&1; then
            break
        fi
        sleep 2
        (( retries-- ))
    done

    if curl -sf "${local_url}/api/tags" >/dev/null 2>&1; then
        # Pick model based on VRAM
        if (( LLM_ARC_GPU_VRAM_MB >= 6144 )); then
            starter_model="llama3.2:3b"
        else
            starter_model="llama3.2:1b"
        fi
        log_info "Pulling ${starter_model}..."
        ollama_cli pull "$starter_model"
        log_ok "Model $starter_model ready"
    else
        log_warn "Ollama not responding yet. Pull a model later:"
        log_info "  ./scripts/models.sh pull llama3.2:3b"
    fi
else
    log_step "Step 8/10: Skipping model pull (--skip-start)"
fi

# ── Step 9: Smoke test ───────────────────────────────────────────
if [[ $SKIP_START -eq 0 ]]; then
    log_step "Step 9/10: Running smoke tests"

    smoke_ok=0
    smoke_total=0

    # Test Ollama API
    (( ++smoke_total ))
    if curl -sf "http://127.0.0.1:${LLM_ARC_OLLAMA_PORT}/api/tags" >/dev/null 2>&1; then
        log_ok "Ollama API responding"
        (( ++smoke_ok ))

        # Test generation
        (( ++smoke_total ))
        response=$(curl -sf "http://127.0.0.1:${LLM_ARC_OLLAMA_PORT}/api/generate" \
            -d '{"model":"'"${starter_model:-llama3.2:3b}"'","prompt":"Say hello in one word.","stream":false}' 2>/dev/null || echo "")
        if [[ -n "$response" ]] && echo "$response" | jq -e '.response' >/dev/null 2>&1; then
            log_ok "LLM generation working"
            (( ++smoke_ok ))
        else
            log_warn "LLM generation test inconclusive"
        fi
    else
        log_warn "Ollama API not responding (may still be loading)"
    fi

    # Test Whisper
    (( ++smoke_total ))
    if curl -sf "http://127.0.0.1:${LLM_ARC_WHISPER_PORT}/health" >/dev/null 2>&1; then
        log_ok "Whisper STT responding"
        (( ++smoke_ok ))
    else
        log_warn "Whisper STT not responding (may still be loading)"
    fi

    # Test Open WebUI
    (( ++smoke_total ))
    if curl -sf "http://127.0.0.1:${LLM_ARC_WEBUI_PORT}" >/dev/null 2>&1; then
        log_ok "Open WebUI responding"
        (( ++smoke_ok ))
    else
        log_warn "Open WebUI not responding (may still be loading)"
    fi

    log_info "Smoke tests: ${smoke_ok}/${smoke_total} passed"
else
    log_step "Step 9/10: Skipping smoke tests (--skip-start)"
fi

# ── Step 10: Print Emacs setup instructions ──────────────────────
log_step "Step 10/10: Setup complete!"

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "  ${BOLD}Emacs Setup (Doom Emacs)${RESET}"
echo "  1. Add to packages.el:"
echo "       (package! gptel)"
echo "       (package! ellama)  ; pulls in llm-ollama automatically"
echo ""
echo "  2. Add to config.el:"
echo "       (add-to-list 'load-path \"${LLM_ARC_DIR}/emacs\")"
echo "       (load! \"llm-arc\" \"${LLM_ARC_DIR}/emacs\")"
echo ""
echo "  3. Run:  doom sync"
echo ""
echo "  ${BOLD}Emacs Setup (Vanilla)${RESET}"
echo "  Add to init.el:"
echo "    (add-to-list 'load-path \"${LLM_ARC_DIR}/emacs\")"
echo "    (require 'llm-arc)"
echo ""
echo "  ${BOLD}Key Bindings (Doom)${RESET}"
echo "    SPC l l   Open gptel chat buffer"
echo "    SPC l s   Send region/buffer to LLM"
echo "    SPC l m   gptel menu (model, params)"
echo "    SPC l r   Rewrite region"
echo "    SPC l c   Switch to code model"
echo "    SPC l e   Ellama actions (s=summarize, g=grammar, t=translate)"
echo "    SPC l w   Toggle dictation"
echo "    SPC l o   Org LLM (d=daily plan, c=smart capture, s=summary)"
echo "    , l       mu4e LLM actions (in email buffers)"
echo "    SPC TAB a Switch to AI workspace"
echo ""
echo "  ${BOLD}Management${RESET}"
echo "    ./scripts/ai-stack.sh status     Service health"
echo "    ./scripts/ai-stack.sh logs       View logs"
echo "    ./scripts/models.sh list         List models"
echo "    ./scripts/models.sh recommend    Model suggestions"
echo "    ./scripts/models.sh pull <model> Download a model"
echo ""
echo "  ${BOLD}Web UI${RESET}"
echo "    http://localhost:${LLM_ARC_WEBUI_PORT}"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
