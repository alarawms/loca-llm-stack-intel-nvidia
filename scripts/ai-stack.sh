#!/usr/bin/env bash
# ai-stack.sh — Lifecycle management for the llm-arc AI stack
#
# Usage:
#   ./ai-stack.sh install    Copy Quadlet files, build containers, daemon-reload
#   ./ai-stack.sh start      Start the pod (all services)
#   ./ai-stack.sh stop       Stop all services
#   ./ai-stack.sh status     Show health of all services
#   ./ai-stack.sh logs [svc] Tail logs (ollama, whisper, piper, openwebui)
#   ./ai-stack.sh restart    Restart all services
#   ./ai-stack.sh update     Pull latest images, rebuild, restart
#   ./ai-stack.sh benchmark  Run a quick speed test on the LLM
#   ./ai-stack.sh uninstall  Remove Quadlet files and stop services

source "$(dirname "${BASH_SOURCE[0]}")/env.sh"

# ── Service names (Quadlet generates these systemd unit names) ───
# NOTE: Podman 4.9.x (Ubuntu 24.04 LTS) does not support Pod= in .container
# quadlets, so the stack uses a shared .network instead. The container
# services Require= the network service automatically.
NETWORK_SERVICE="ai-stack-network.service"
SERVICES=(
    "ollama.service"
    "whisper.service"
    "piper.service"
    "searxng.service"
    "openwebui.service"
    "openclaw.service"
)

# ── Load GPU config if available ─────────────────────────────────
gpu_config="${LLM_ARC_CONFIG}/gpu.env"
[[ -f "$gpu_config" ]] && source "$gpu_config"

# ── Commands ─────────────────────────────────────────────────────

cmd_install() {
    log_step "Installing llm-arc AI stack"

    # 1. Run GPU detection and save config
    log_info "Detecting GPU..."
    bash "${LLM_ARC_SCRIPTS}/detect-gpu.sh" --save
    source "${LLM_ARC_CONFIG}/gpu.env"

    # 2. Create model directories
    log_info "Creating model directories..."
    mkdir -p "$LLM_ARC_OLLAMA_MODELS" \
             "$LLM_ARC_WHISPER_MODELS" \
             "$LLM_ARC_PIPER_MODELS" \
             "${LLM_ARC_MODELS}/sycl-cache" \
             "${LLM_ARC_MODELS}/searxng"

    if [[ ! -f "${LLM_ARC_MODELS}/searxng/settings.yml" ]]; then
        cp "${LLM_ARC_CONFIG}/searxng/settings.yml" "${LLM_ARC_MODELS}/searxng/settings.yml"
        log_ok "SearXNG settings.yml deployed"
    fi

    # 3. Build container images
    log_info "Building container images..."

    case "${LLM_ARC_GPU_VENDOR:-intel}" in
        nvidia)
            log_info "NVIDIA GPU detected — using stock Ollama image (has CUDA)"
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
                log_info "Building Vulkan Ollama image (recommended for $LLM_ARC_GPU_CLASS)..."
                podman build -t ollama-vulkan:latest \
                    -f "${LLM_ARC_CONTAINERS}/Containerfile.ollama-vulkan" \
                    "${LLM_ARC_DIR}"
                log_info "Using Vulkan backend for Ollama"
            else
                log_info "Building IPEX-LLM Ollama image (recommended for $LLM_ARC_GPU_CLASS)..."
                podman build -t ollama-ipex:latest \
                    -f "${LLM_ARC_CONTAINERS}/Containerfile.ollama-ipex" \
                    "${LLM_ARC_DIR}"
            fi

            if [[ -f "${LLM_ARC_CONTAINERS}/Containerfile.whisper-sycl" ]]; then
                log_info "Building Whisper SYCL image..."
                podman build -t whisper-sycl:latest \
                    -f "${LLM_ARC_CONTAINERS}/Containerfile.whisper-sycl" \
                    "${LLM_ARC_DIR}"
            fi
            ;;
    esac

    # 4. Copy Quadlet files to systemd directory
    log_info "Installing Quadlet unit files to ${LLM_ARC_QUADLET_DIR}/"
    mkdir -p "$LLM_ARC_QUADLET_DIR"

    for f in "${LLM_ARC_QUADLET}"/*.{network,pod,container}; do
        [[ -f "$f" ]] || continue
        local basename
        basename=$(basename "$f")

        if [[ "${LLM_ARC_GPU_VENDOR:-intel}" == "nvidia" ]]; then
            # NVIDIA: rewrite ollama and whisper container units
            case "$basename" in
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
                        "$f" > "${LLM_ARC_QUADLET_DIR}/${basename}"
                    # Add NVIDIA env vars and disable SELinux labeling for GPU access
                    sed -i '/OLLAMA_NUM_GPU/a Environment=NVIDIA_VISIBLE_DEVICES=all\nEnvironment=NVIDIA_DRIVER_CAPABILITIES=compute,utility\nSecurityLabelDisable=true' \
                        "${LLM_ARC_QUADLET_DIR}/${basename}"
                    ;;
                whisper.container)
                    sed -e 's|Image=localhost/whisper-sycl:latest|Image=localhost/whisper-cuda:latest|' \
                        -e 's|Description=.*|Description=Whisper Speech-to-Text Server (NVIDIA CUDA GPU)|' \
                        -e 's|AddDevice=/dev/dri.*|AddDevice=nvidia.com/gpu=all|' \
                        -e '/GroupAdd=keep-groups/d' \
                        -e '/ONEAPI_DEVICE_SELECTOR/d' \
                        -e '/ZES_ENABLE_SYSMAN/d' \
                        -e '/SYCL_CACHE_PERSISTENT/d' \
                        "$f" > "${LLM_ARC_QUADLET_DIR}/${basename}"
                    # Add NVIDIA env vars and disable SELinux labeling for GPU access
                    sed -i '/^\[Container\]/a Environment=NVIDIA_VISIBLE_DEVICES=all\nEnvironment=NVIDIA_DRIVER_CAPABILITIES=compute,utility\nSecurityLabelDisable=true' \
                        "${LLM_ARC_QUADLET_DIR}/${basename}"
                    ;;
                *)
                    cp "$f" "${LLM_ARC_QUADLET_DIR}/${basename}"
                    ;;
            esac
        elif [[ "$basename" == "ollama.container" && "$LLM_ARC_GPU_BACKEND" == "vulkan" ]]; then
            sed 's|Image=localhost/ollama-ipex:latest|Image=localhost/ollama-vulkan:latest|' \
                "$f" > "${LLM_ARC_QUADLET_DIR}/${basename}"
        else
            cp "$f" "${LLM_ARC_QUADLET_DIR}/${basename}"
        fi
        log_ok "  Installed ${basename}"
    done

    # 5. Reload systemd
    log_info "Reloading systemd user daemon..."
    systemctl --user daemon-reload

    log_ok "Installation complete!"
    echo ""
    log_info "Next steps:"
    log_info "  1. Start the stack:    ./scripts/ai-stack.sh start"
    log_info "  2. Pull a model:       ./scripts/models.sh pull llama3.2:3b"
    log_info "  3. Test it:            ./scripts/models.sh test"
}

cmd_start() {
    log_step "Starting AI stack..."
    systemctl --user start "$NETWORK_SERVICE"
    systemctl --user start "${SERVICES[@]}"

    # Wait briefly for services to start
    sleep 2
    cmd_status
}

cmd_stop() {
    log_step "Stopping AI stack..."
    systemctl --user stop "${SERVICES[@]}" 2>/dev/null || true
    log_ok "All services stopped"
}

cmd_restart() {
    log_step "Restarting AI stack..."
    systemctl --user restart "${SERVICES[@]}"
    sleep 2
    cmd_status
}

cmd_status() {
    log_step "AI Stack Status"
    echo ""

    # Network status
    printf "  %-20s " "Network:"
    if systemctl --user is-active "$NETWORK_SERVICE" &>/dev/null; then
        printf "${GREEN}running${RESET}\n"
    else
        printf "${RED}stopped${RESET}\n"
    fi

    # Individual services
    for svc in "${SERVICES[@]}"; do
        local name="${svc%.service}"
        printf "  %-20s " "${name}:"

        if ! systemctl --user cat "$svc" &>/dev/null 2>&1; then
            printf "${YELLOW}not installed${RESET}\n"
            continue
        fi

        local state
        state=$(systemctl --user is-active "$svc" 2>/dev/null || true)
        case "$state" in
            active)   printf "${GREEN}running${RESET}" ;;
            inactive) printf "${YELLOW}stopped${RESET}" ;;
            failed)   printf "${RED}failed${RESET}" ;;
            *)        printf "${YELLOW}${state}${RESET}" ;;
        esac

        # Health check status
        if [[ "$state" == "active" ]]; then
            local container_name="$name"
            local health
            health=$(podman inspect --format='{{.State.Health.Status}}' "$container_name" 2>/dev/null || echo "")
            if [[ -n "$health" ]]; then
                case "$health" in
                    healthy)  printf "  ${GREEN}(healthy)${RESET}" ;;
                    starting) printf "  ${YELLOW}(starting)${RESET}" ;;
                    *)        printf "  ${RED}(${health})${RESET}" ;;
                esac
            fi
        fi
        echo ""
    done

    echo ""

    # Quick API checks
    if curl -sf "http://127.0.0.1:${LLM_ARC_OLLAMA_PORT}/api/tags" >/dev/null 2>&1; then
        local model_count
        model_count=$(curl -sf "http://127.0.0.1:${LLM_ARC_OLLAMA_PORT}/api/tags" | \
            jq '.models | length' 2>/dev/null || echo "?")
        log_ok "Ollama API responding ($model_count models loaded)"
    fi

    if curl -sf "http://127.0.0.1:${LLM_ARC_WHISPER_PORT}/" >/dev/null 2>&1; then
        log_ok "Whisper STT responding"
    fi

    if curl -sf "http://127.0.0.1:${LLM_ARC_SEARXNG_PORT}/healthz" >/dev/null 2>&1; then
        log_ok "SearXNG search responding"
    fi

    if curl -sf "http://127.0.0.1:${LLM_ARC_OPENCLAW_PORT}/" >/dev/null 2>&1; then
        log_ok "OpenClaw agent responding at http://localhost:${LLM_ARC_OPENCLAW_PORT}"
    fi
}

cmd_logs() {
    local svc="${1:-}"

    if [[ -z "$svc" ]]; then
        # Show all stack service logs
        journalctl --user -u "ollama.service" -u "whisper.service" -u "piper.service" \
            -u "searxng.service" -u "openwebui.service" -u "openclaw.service" \
            -f --no-hostname
    else
        # Map short names to service names
        case "$svc" in
            ollama|whisper|piper|searxng|openwebui|openclaw)
                journalctl --user -u "${svc}.service" -f --no-hostname
                ;;
            *)
                log_error "Unknown service: $svc"
                log_info  "Available: ollama, whisper, piper, searxng, openwebui, openclaw"
                exit 1
                ;;
        esac
    fi
}

cmd_update() {
    log_step "Updating AI stack..."

    # Load GPU config
    [[ -f "$gpu_config" ]] && source "$gpu_config"

    # Pull latest base images (vendor-aware)
    log_info "Pulling latest base images..."
    case "${LLM_ARC_GPU_VENDOR:-intel}" in
        nvidia)
            podman pull "$LLM_ARC_OLLAMA_CUDA_IMAGE" 2>/dev/null || true
            podman pull docker.io/nvidia/cuda:12.6.3-runtime-ubuntu22.04 2>/dev/null || true
            ;;
        *)
            podman pull docker.io/intelanalytics/ipex-llm-inference-cpp-xpu:latest 2>/dev/null || true
            podman pull docker.io/ollama/ollama:latest 2>/dev/null || true
            ;;
    esac
    podman pull docker.io/rhasspy/wyoming-piper:latest 2>/dev/null || true
    podman pull docker.io/searxng/searxng:latest 2>/dev/null || true
    podman pull ghcr.io/open-webui/open-webui:main 2>/dev/null || true

    # Rebuild custom images
    log_info "Rebuilding custom images..."
    cmd_install

    # Restart
    cmd_restart
}

cmd_benchmark() {
    log_step "Running LLM benchmark..."

    local url="http://127.0.0.1:${LLM_ARC_OLLAMA_PORT}"
    if ! curl -sf "${url}/api/tags" >/dev/null 2>&1; then
        log_error "Ollama is not running"
        exit 1
    fi

    # Get first available model
    local model
    model=$(curl -sf "${url}/api/tags" | jq -r '.models[0].name // empty' 2>/dev/null)
    if [[ -z "$model" ]]; then
        log_error "No models installed. Pull one first: ./scripts/models.sh pull llama3.2:3b"
        exit 1
    fi

    log_info "Model: $model"
    echo ""

    # Warmup
    log_info "Warmup run..."
    curl -sf "${url}/api/generate" \
        -d "{\"model\":\"$model\",\"prompt\":\"Hello\",\"stream\":false}" >/dev/null

    # Benchmark: short prompt
    log_info "Test 1: Short prompt generation"
    local response
    response=$(curl -sf "${url}/api/generate" \
        -d "{\"model\":\"$model\",\"prompt\":\"Explain quantum computing in 3 sentences.\",\"stream\":false}")

    if command -v jq &>/dev/null; then
        local prompt_eval_count prompt_eval_duration eval_count eval_duration
        prompt_eval_count=$(echo "$response" | jq -r '.prompt_eval_count // 0')
        prompt_eval_duration=$(echo "$response" | jq -r '.prompt_eval_duration // 1')
        eval_count=$(echo "$response" | jq -r '.eval_count // 0')
        eval_duration=$(echo "$response" | jq -r '.eval_duration // 1')

        local prefill_tps gen_tps
        prefill_tps=$(echo "$prompt_eval_count $prompt_eval_duration" | \
            awk '{if($2>0) printf "%.1f", $1/($2/1000000000); else print "N/A"}')
        gen_tps=$(echo "$eval_count $eval_duration" | \
            awk '{if($2>0) printf "%.1f", $1/($2/1000000000); else print "N/A"}')

        echo ""
        printf "  %-24s %s tokens/sec\n" "Prompt processing:" "$prefill_tps"
        printf "  %-24s %s tokens/sec\n" "Generation speed:" "$gen_tps"
        printf "  %-24s %s tokens\n" "Tokens generated:" "$eval_count"
    else
        echo "$response"
    fi

    echo ""
    log_ok "Benchmark complete"
}

cmd_uninstall() {
    log_step "Uninstalling AI stack..."

    # Stop services
    systemctl --user stop "${SERVICES[@]}" 2>/dev/null || true
    systemctl --user stop "$NETWORK_SERVICE" 2>/dev/null || true

    # Remove quadlet files
    for f in "${LLM_ARC_QUADLET_DIR}"/ai-stack.network "${LLM_ARC_QUADLET_DIR}"/*.container; do
        [[ -f "$f" ]] || continue
        local basename
        basename=$(basename "$f")
        # Only remove files we installed
        case "$basename" in
            ai-stack.network|ollama.container|whisper.container|piper.container|searxng.container|openwebui.container|openclaw.container)
                rm "$f"
                log_ok "Removed $basename"
                ;;
        esac
    done

    systemctl --user daemon-reload
    log_ok "Quadlet files removed and systemd reloaded"
    log_info "Model data preserved in ${LLM_ARC_MODELS}/"
    log_info "To remove models too: rm -rf ${LLM_ARC_MODELS}"
}

# ── Main ─────────────────────────────────────────────────────────
case "${1:-help}" in
    install)   cmd_install ;;
    start)     cmd_start ;;
    stop)      cmd_stop ;;
    restart)   cmd_restart ;;
    status)    cmd_status ;;
    logs)      cmd_logs "${2:-}" ;;
    update)    cmd_update ;;
    benchmark) cmd_benchmark ;;
    uninstall) cmd_uninstall ;;
    help|*)
        echo "Usage: $(basename "$0") <command>"
        echo ""
        echo "Commands:"
        echo "  install    Build containers, copy Quadlet files, reload systemd"
        echo "  start      Start the AI stack pod (all services)"
        echo "  stop       Stop all services"
        echo "  restart    Restart all services"
        echo "  status     Show health/status of all services"
        echo "  logs [svc] Tail logs (ollama, whisper, piper, searxng, openwebui)"
        echo "  update     Pull latest images, rebuild, restart"
        echo "  benchmark  Run a quick LLM speed test"
        echo "  uninstall  Remove Quadlet files (preserves models)"
        ;;
esac
