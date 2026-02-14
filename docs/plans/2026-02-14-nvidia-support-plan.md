# NVIDIA GPU Support — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add full NVIDIA GPU support (consumer + datacenter) alongside existing Intel Arc, with auto-detection, CUDA containers, CDI passthrough, and vendor-aware installation.

**Architecture:** Extend existing scripts with a `LLM_ARC_GPU_VENDOR` variable (`intel`|`nvidia`). Detection scans both vendors via `/sys/class/drm/`. NVIDIA uses stock Ollama image (has CUDA), new `whisper-cuda` container, and CDI device passthrough via nvidia-container-toolkit.

**Tech Stack:** Bash, Podman Quadlet, NVIDIA Container Toolkit (CDI), CUDA 12.x, whisper.cpp

---

### Task 1: Add NVIDIA variables to `env.sh`

**Files:**
- Modify: `scripts/env.sh`

**Step 1: Add NVIDIA image names after the existing Intel image vars (line 37)**

After the line `export LLM_ARC_WEBUI_IMAGE=...`, add:

```bash
# ── NVIDIA container images ───────────────────────────────────────
export LLM_ARC_OLLAMA_CUDA_IMAGE="docker.io/ollama/ollama:latest"
export LLM_ARC_WHISPER_CUDA_IMAGE="localhost/whisper-cuda:latest"
```

**Step 2: Add NVIDIA env vars array after the existing Intel `LLM_ARC_GPU_ENV_VARS` block (line 52)**

After the closing `)` of `LLM_ARC_GPU_ENV_VARS`, add:

```bash
# ── NVIDIA GPU environment (for CUDA containers) ──────────────────
export LLM_ARC_NVIDIA_ENV_VARS=(
    "NVIDIA_VISIBLE_DEVICES=all"
    "NVIDIA_DRIVER_CAPABILITIES=compute,utility"
    "OLLAMA_HOST=0.0.0.0"
    "OLLAMA_ORIGINS=*"
    "OLLAMA_NUM_GPU=999"
)
```

**Step 3: Add vendor default to the GPU detection cache block (line 96)**

After `export LLM_ARC_GPU_BACKEND=...`, add:

```bash
export LLM_ARC_GPU_VENDOR="${LLM_ARC_GPU_VENDOR:-unknown}"
```

**Step 4: Verify syntax**

Run: `bash -n scripts/env.sh`
Expected: no output (clean parse)

**Step 5: Commit**

```bash
git add scripts/env.sh
git commit -m "feat: add NVIDIA image names, env vars, and vendor variable to env.sh"
```

---

### Task 2: Extend GPU detection for NVIDIA (`detect-gpu.sh`)

**Files:**
- Modify: `scripts/detect-gpu.sh`

This is the largest change. The detection loop must scan for NVIDIA (vendor `0x10de`) in addition to Intel (`0x8086`).

**Step 1: Add NVIDIA classification function after `classify_gpu()`**

Add a new function that classifies NVIDIA GPUs by querying `nvidia-smi`:

```bash
# ── NVIDIA GPU classification ─────────────────────────────────────
# Uses nvidia-smi to get accurate GPU name and VRAM.
# Returns: "dgpu:<class>" where class is datacenter, consumer-high, consumer-mid, consumer-entry
classify_nvidia_gpu() {
    local gpu_name="$1"
    case "$gpu_name" in
        *A100*|*H100*|*H200*|*L40*|*L4\ *|*Tesla*|*V100*)
            echo "dgpu:nvidia-datacenter"
            ;;
        *RTX\ 3090*|*RTX\ 4090*|*RTX\ 5090*|*RTX\ 4080*|*RTX\ 5080*|*RTX\ 3080\ Ti*)
            echo "dgpu:nvidia-high"
            ;;
        *RTX\ 30[67]0*|*RTX\ 40[67]0*|*RTX\ 50[67]0*|*RTX\ 20[78]0*|*RTX\ 3080*)
            echo "dgpu:nvidia-mid"
            ;;
        *)
            echo "dgpu:nvidia-entry"
            ;;
    esac
}
```

**Step 2: Add NVIDIA detection function**

Add after `classify_nvidia_gpu()`:

```bash
# ── Detect NVIDIA GPU via nvidia-smi ──────────────────────────────
detect_nvidia_gpu() {
    command -v nvidia-smi &>/dev/null || return 1

    local csv
    csv=$(nvidia-smi --query-gpu=name,memory.total --format=csv,noheader,nounits 2>/dev/null) || return 1
    [[ -n "$csv" ]] || return 1

    # Take the first GPU (strongest is usually index 0)
    local line
    line=$(echo "$csv" | head -1)
    local gpu_name vram_mb
    gpu_name=$(echo "$line" | cut -d, -f1 | xargs)
    vram_mb=$(echo "$line" | cut -d, -f2 | xargs)

    # Find the render node for NVIDIA
    local nvidia_render=""
    for card_dir in /sys/class/drm/card[0-9]*; do
        [[ -d "$card_dir/device" ]] || continue
        local vendor
        vendor=$(cat "$card_dir/device/vendor" 2>/dev/null)
        if [[ "$vendor" == "0x10de" ]]; then
            local card_name card_num
            card_name=$(basename "$card_dir")
            card_num="${card_name#card}"
            nvidia_render="/dev/dri/renderD$((128 + card_num))"
            break
        fi
    done

    local class
    class=$(classify_nvidia_gpu "$gpu_name")

    # Export results
    export LLM_ARC_GPU_VENDOR="nvidia"
    export LLM_ARC_GPU_TYPE="dgpu"
    export LLM_ARC_GPU_CLASS="$class"
    export LLM_ARC_GPU_DEVICE="nvidia"
    export LLM_ARC_GPU_RENDER_NODE="${nvidia_render:-/dev/nvidia0}"
    export LLM_ARC_GPU_CARD_NODE="${nvidia_render:-/dev/nvidia0}"
    export LLM_ARC_GPU_VRAM_MB="$vram_mb"
    export LLM_ARC_GPU_NAME="NVIDIA $gpu_name"
    export LLM_ARC_GPU_BACKEND="cuda"
    export LLM_ARC_GPU_RECOMMENDED_MODELS=$(recommend_models "$vram_mb")

    return 0
}
```

**Step 3: Update `recommend_backend()` to handle NVIDIA**

Add a case before the catch-all `*`:

```bash
        dgpu:nvidia-*)     echo "cuda"   ;; # NVIDIA always uses CUDA
```

**Step 4: Modify `detect_gpu()` to set vendor for Intel**

At the end of the existing `detect_gpu()` function, before `return 0`, add:

```bash
    export LLM_ARC_GPU_VENDOR="intel"
```

**Step 5: Update the main detection entry point**

Replace the entry-point block (lines 262-283) with logic that tries NVIDIA first (preferred when present), then falls back to Intel:

```bash
# ── Entry point ──────────────────────────────────────────────────
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # Running as a script (not sourced)
    # Try NVIDIA first (preferred over Intel iGPU if both present)
    if ! detect_nvidia_gpu 2>/dev/null; then
        if ! detect_gpu; then
            log_error "No supported GPU detected (checked Intel and NVIDIA)"
            exit 1
        fi
    fi

    case "${1:-}" in
        --env)
            print_env
            ;;
        --save)
            save_config
            print_results
            ;;
        *)
            print_results
            ;;
    esac
else
    # Being sourced — detect silently and export vars
    if ! detect_nvidia_gpu 2>/dev/null; then
        detect_gpu || log_warn "GPU detection failed"
    fi
fi
```

**Step 6: Update `print_env()` to include vendor**

Add to `print_env()`:

```bash
    echo "export LLM_ARC_GPU_VENDOR=\"$LLM_ARC_GPU_VENDOR\""
```

**Step 7: Update `print_results()` banner**

Change `log_step "Intel GPU Detection Results"` to:

```bash
    log_step "GPU Detection Results"
```

And add a vendor line to the printf block:

```bash
    printf "  %-22s %s\n" "Vendor:"       "$LLM_ARC_GPU_VENDOR"
```

Also update the render node check for NVIDIA (CDI doesn't use render nodes the same way):

```bash
    # Check device accessibility
    if [[ "$LLM_ARC_GPU_VENDOR" == "nvidia" ]]; then
        if command -v nvidia-smi &>/dev/null && nvidia-smi &>/dev/null; then
            log_ok "NVIDIA GPU accessible via nvidia-smi"
        else
            log_warn "nvidia-smi not working — check NVIDIA driver"
        fi
    elif [[ -c "$LLM_ARC_GPU_RENDER_NODE" ]] && [[ -r "$LLM_ARC_GPU_RENDER_NODE" ]]; then
        log_ok "Render node $LLM_ARC_GPU_RENDER_NODE is accessible"
    else
        log_warn "Render node $LLM_ARC_GPU_RENDER_NODE not accessible (check render/video group)"
    fi
```

**Step 8: Verify syntax**

Run: `bash -n scripts/detect-gpu.sh`
Expected: no output (clean parse)

**Step 9: Commit**

```bash
git add scripts/detect-gpu.sh
git commit -m "feat: add NVIDIA GPU detection alongside Intel in detect-gpu.sh"
```

---

### Task 3: Create `Containerfile.whisper-cuda`

**Files:**
- Create: `containers/Containerfile.whisper-cuda`

**Step 1: Create the Containerfile**

Mirror the structure of `Containerfile.whisper-sycl` but use NVIDIA CUDA base images:

```dockerfile
# Containerfile.whisper-cuda — whisper.cpp with CUDA backend for NVIDIA GPUs
#
# Multi-stage build:
#   1. Build whisper.cpp with CUDA support
#   2. Runtime image with just the server binary and CUDA libs
#
# Build:
#   podman build -t whisper-cuda:latest -f containers/Containerfile.whisper-cuda .

# ── Stage 1: Build ───────────────────────────────────────────────
FROM docker.io/nvidia/cuda:12.6.3-devel-ubuntu22.04 AS builder

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        git cmake build-essential curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Clone whisper.cpp and build with CUDA
RUN git clone --depth 1 https://github.com/ggml-org/whisper.cpp.git && \
    cd whisper.cpp && \
    cmake -B build \
        -DGGML_CUDA=ON \
        -DCMAKE_BUILD_TYPE=Release && \
    cmake --build build --config Release -j"$(nproc)"

# Download default model (base.en — good speed/quality balance)
RUN cd whisper.cpp && \
    bash models/download-ggml-model.sh base.en

# ── Stage 2: Runtime ─────────────────────────────────────────────
FROM docker.io/nvidia/cuda:12.6.3-runtime-ubuntu22.04

RUN apt-get update && \
    apt-get install -y --no-install-recommends curl && \
    rm -rf /var/lib/apt/lists/*

# Copy built binaries
COPY --from=builder /build/whisper.cpp/build/bin/whisper-server /usr/local/bin/
COPY --from=builder /build/whisper.cpp/build/bin/whisper-cli /usr/local/bin/

# Copy default model
COPY --from=builder /build/whisper.cpp/models/ggml-base.en.bin /models/ggml-base.en.bin

# NVIDIA environment
ENV NVIDIA_VISIBLE_DEVICES=all \
    NVIDIA_DRIVER_CAPABILITIES=compute,utility

EXPOSE 9000

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -sf http://localhost:9000/health || exit 1

# Default: run server with base.en model on port 9000
ENTRYPOINT ["whisper-server", \
    "--model", "/models/ggml-base.en.bin", \
    "--host", "0.0.0.0", \
    "--port", "9000"]
```

**Step 2: Commit**

```bash
git add containers/Containerfile.whisper-cuda
git commit -m "feat: add Containerfile.whisper-cuda for NVIDIA GPU Whisper"
```

---

### Task 4: Update prerequisite checks (`00-setup-host.sh`)

**Files:**
- Modify: `scripts/00-setup-host.sh`

**Step 1: Add early vendor detection**

After sourcing `env.sh` and parsing `--fix`, add a quick vendor sniff so checks can branch:

```bash
# ── Quick vendor detection (for conditional checks) ───────────────
GPU_VENDOR="unknown"
if command -v nvidia-smi &>/dev/null && nvidia-smi &>/dev/null 2>&1; then
    GPU_VENDOR="nvidia"
elif compgen -G "/sys/class/drm/card*/device/vendor" >/dev/null 2>&1; then
    for vf in /sys/class/drm/card*/device/vendor; do
        if [[ "$(cat "$vf" 2>/dev/null)" == "0x8086" ]]; then
            GPU_VENDOR="intel"
            break
        fi
    done
fi
log_info "Detected GPU vendor hint: $GPU_VENDOR"
```

**Step 2: Make user group checks vendor-conditional**

Wrap the render/video group checks (lines 62-82) in a vendor conditional:

```bash
# ── User groups (Intel only — NVIDIA uses CDI, no group needed) ──
log_step "Checking user groups"

if [[ "$GPU_VENDOR" != "nvidia" ]]; then
    # ... existing render/video group checks ...
else
    log_info "NVIDIA GPU: render/video groups not required (using CDI)"
fi
```

**Step 3: Add NVIDIA-specific checks after the GPU device node section**

```bash
# ── NVIDIA Container Toolkit (NVIDIA only) ────────────────────────
if [[ "$GPU_VENDOR" == "nvidia" ]]; then
    log_step "Checking NVIDIA Container Toolkit"

    check "nvidia-smi responds" nvidia-smi

    if ! check "nvidia-ctk installed" command -v nvidia-ctk; then
        log_info "Fix: Install nvidia-container-toolkit"
        log_info "  See: https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/latest/install-guide.html"
    fi

    # Check CDI spec exists
    check_cdi_spec() {
        nvidia-ctk cdi list 2>/dev/null | grep -q "nvidia.com/gpu"
    }
    if ! check "CDI spec generated for Podman" check_cdi_spec; then
        if [[ $FIX_MODE -eq 1 ]]; then
            log_info "Attempting: sudo nvidia-ctk cdi generate --output=/etc/cdi/nvidia.yaml"
            sudo nvidia-ctk cdi generate --output=/etc/cdi/nvidia.yaml && log_ok "CDI spec generated"
        else
            log_info "Fix: sudo nvidia-ctk cdi generate --output=/etc/cdi/nvidia.yaml"
        fi
    fi
fi
```

**Step 4: Make GPU device node checks vendor-conditional**

Wrap the existing render node checks (lines 84-98) in:

```bash
if [[ "$GPU_VENDOR" != "nvidia" ]]; then
    # ... existing /dev/dri/renderD* checks ...
fi
```

**Step 5: Update the GPU detection section to use multi-vendor detection**

Change line 161 from:
```bash
if source "${LLM_ARC_SCRIPTS}/detect-gpu.sh" 2>/dev/null; then
```
to just keep it as-is — `detect-gpu.sh` now handles both vendors when sourced.

**Step 6: Verify syntax**

Run: `bash -n scripts/00-setup-host.sh`
Expected: no output

**Step 7: Commit**

```bash
git add scripts/00-setup-host.sh
git commit -m "feat: add NVIDIA prereq checks (nvidia-smi, nvidia-ctk, CDI) to setup-host"
```

---

### Task 5: Update installer (`install.sh`) for vendor-aware build and Quadlet

**Files:**
- Modify: `install.sh`

**Step 1: Update the banner (line 39)**

Change:
```bash
echo "║       Intel Arc GPU · Podman · Emacs Integration     ║"
```
to:
```bash
echo "║     Intel Arc / NVIDIA GPU · Podman · Emacs          ║"
```

**Step 2: Update Step 2 header and GPU detection**

Change line 68 from `"Detecting Intel GPU"` to `"Detecting GPU"`.

The rest of Step 2 stays the same — `detect-gpu.sh --save` now handles both vendors, and `gpu.env` will contain the vendor.

**Step 3: Update Step 3 (container builds) for NVIDIA**

Replace the build block (lines 77-109) with vendor-aware logic:

```bash
if [[ $SKIP_BUILD -eq 0 ]]; then
    log_step "Step 3/10: Building container images"

    case "$LLM_ARC_GPU_VENDOR" in
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
        intel|*)
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
```

**Step 4: Update Step 4 (Quadlet installation) for vendor-aware rewriting**

Replace the Quadlet copy loop (lines 116-128) with:

```bash
for f in "${LLM_ARC_QUADLET}"/*.{pod,container}; do
    [[ -f "$f" ]] || continue
    local_basename=$(basename "$f")

    if [[ "$LLM_ARC_GPU_VENDOR" == "nvidia" ]]; then
        # NVIDIA: rewrite ollama and whisper container units
        case "$local_basename" in
            ollama.container)
                sed -e 's|Image=localhost/ollama-ipex:latest|Image=docker.io/ollama/ollama:latest|' \
                    -e 's|Description=.*|Description=Ollama LLM Server (NVIDIA CUDA GPU)|' \
                    -e 's|AddDevice=/dev/dri|AddDevice=nvidia.com/gpu=all|' \
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
                    -e 's|AddDevice=/dev/dri|AddDevice=nvidia.com/gpu=all|' \
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
```

**Step 5: Update Step 5 — skip SYCL cache dir for NVIDIA**

After the existing `mkdir -p` block, add:

```bash
# NVIDIA doesn't need SYCL cache
if [[ "$LLM_ARC_GPU_VENDOR" != "nvidia" ]]; then
    mkdir -p "${LLM_ARC_MODELS}/sycl-cache"
fi
```

(And remove `"${LLM_ARC_MODELS}/sycl-cache"` from the main `mkdir -p` line, or just leave it — an empty dir is harmless.)

**Step 6: Verify syntax**

Run: `bash -n install.sh`
Expected: no output

**Step 7: Commit**

```bash
git add install.sh
git commit -m "feat: make installer vendor-aware for NVIDIA (builds, Quadlet, images)"
```

---

### Task 6: Update README.md

**Files:**
- Modify: `README.md`

**Step 1: Update the description line (line 3)**

Change to:
```
Local AI stack for Intel Arc and NVIDIA GPUs, managed with Podman Quadlet and integrated with Emacs.
```

**Step 2: Add NVIDIA rows to the GPU Support table (after line 41)**

```markdown
| GeForce GTX 10/16-series | dGPU | CUDA | Good |
| GeForce RTX 20/30-series | dGPU | CUDA | Great |
| GeForce RTX 40/50-series | dGPU | CUDA | Best |
| Tesla, A100, H100, L40 | dGPU | CUDA | Best |
```

**Step 3: Update the backend auto-select note (line 43)**

Change to:
```
Backend is auto-selected by `scripts/detect-gpu.sh`. Override with `LLM_ARC_GPU_BACKEND=vulkan` (Intel) or `LLM_ARC_GPU_BACKEND=cuda` (NVIDIA).
```

**Step 4: Add NVIDIA to Requirements (after line 31)**

Add after the existing requirements:
```markdown
- **NVIDIA GPU** (optional): Requires `nvidia-container-toolkit` for CDI passthrough
```

**Step 5: Add NVIDIA troubleshooting entries**

Add before the closing of the Troubleshooting section:

```markdown
**NVIDIA GPU not detected**: Ensure `nvidia-smi` works and shows your GPU. Install the NVIDIA driver if missing.

**NVIDIA container fails**: Install nvidia-container-toolkit and generate CDI spec:
\`\`\`bash
sudo nvidia-ctk cdi generate --output=/etc/cdi/nvidia.yaml
\`\`\`

**NVIDIA + Intel both present**: The installer prefers NVIDIA. Override with `LLM_ARC_GPU_VENDOR=intel`.
```

**Step 6: Add `Containerfile.whisper-cuda` to the Project Structure tree**

Add after `Containerfile.whisper-sycl`:
```
    Containerfile.whisper-cuda      # Whisper with CUDA backend (for NVIDIA)
```

**Step 7: Commit**

```bash
git add README.md
git commit -m "docs: add NVIDIA GPU support to README"
```

---

### Task 7: Smoke test the full change set

**Files:** None (verification only)

**Step 1: Run shellcheck on all modified scripts**

```bash
shellcheck scripts/env.sh scripts/detect-gpu.sh scripts/00-setup-host.sh install.sh
```
Expected: no errors (warnings about `source` are fine)

**Step 2: Verify syntax parse on all scripts**

```bash
bash -n scripts/env.sh && bash -n scripts/detect-gpu.sh && bash -n scripts/00-setup-host.sh && bash -n install.sh
```
Expected: no output (all clean)

**Step 3: Test detection on current machine (Intel iGPU)**

```bash
bash scripts/detect-gpu.sh
```
Expected: should still detect Intel Arrow Lake iGPU as before. `LLM_ARC_GPU_VENDOR` should be `intel`.

**Step 4: Test detection env output**

```bash
bash scripts/detect-gpu.sh --env
```
Expected: includes `export LLM_ARC_GPU_VENDOR="intel"` line.

**Step 5: Verify Containerfile syntax**

```bash
podman build --dry-run -f containers/Containerfile.whisper-cuda . 2>&1 || echo "dry-run not supported, check file manually"
```

**Step 6: Final commit (if any fixups needed)**

```bash
git add -A
git commit -m "fix: address shellcheck/lint issues from NVIDIA support"
```
(Only if Step 1-5 revealed issues that needed fixing.)
