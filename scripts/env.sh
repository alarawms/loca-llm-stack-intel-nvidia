#!/usr/bin/env bash
# env.sh — Shared environment variables for the llm-arc AI stack
# Source this file from other scripts: source "$(dirname "$0")/env.sh"

set -euo pipefail

# ── Project paths ────────────────────────────────────────────────
export LLM_ARC_DIR="${LLM_ARC_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
export LLM_ARC_SCRIPTS="${LLM_ARC_DIR}/scripts"
export LLM_ARC_CONTAINERS="${LLM_ARC_DIR}/containers"
export LLM_ARC_QUADLET="${LLM_ARC_DIR}/quadlet"
export LLM_ARC_CONFIG="${LLM_ARC_DIR}/config"

# ── Model storage ────────────────────────────────────────────────
export LLM_ARC_MODELS="${LLM_ARC_MODELS:-${HOME}/.local/share/ai-models}"
export LLM_ARC_OLLAMA_MODELS="${LLM_ARC_MODELS}/ollama"
export LLM_ARC_WHISPER_MODELS="${LLM_ARC_MODELS}/whisper"
export LLM_ARC_PIPER_MODELS="${LLM_ARC_MODELS}/piper"

# ── Quadlet install path (rootless) ─────────────────────────────
export LLM_ARC_QUADLET_DIR="${HOME}/.config/containers/systemd"

# ── Service ports (localhost only) ───────────────────────────────
export LLM_ARC_OLLAMA_PORT="${LLM_ARC_OLLAMA_PORT:-11434}"
export LLM_ARC_WHISPER_PORT="${LLM_ARC_WHISPER_PORT:-9000}"
export LLM_ARC_PIPER_PORT="${LLM_ARC_PIPER_PORT:-5002}"
export LLM_ARC_WEBUI_PORT="${LLM_ARC_WEBUI_PORT:-8080}"

# ── Pod name ─────────────────────────────────────────────────────
export LLM_ARC_POD="ai-stack"

# ── Container image names ────────────────────────────────────────
export LLM_ARC_OLLAMA_IPEX_IMAGE="localhost/ollama-ipex:latest"
export LLM_ARC_OLLAMA_VULKAN_IMAGE="docker.io/ollama/ollama:latest"
export LLM_ARC_WHISPER_IMAGE="localhost/whisper-sycl:latest"
export LLM_ARC_PIPER_IMAGE="docker.io/rhasspy/wyoming-piper:latest"
export LLM_ARC_WEBUI_IMAGE="ghcr.io/open-webui/open-webui:main"

# ── NVIDIA container images ───────────────────────────────────────
export LLM_ARC_OLLAMA_CUDA_IMAGE="docker.io/ollama/ollama:latest"
export LLM_ARC_WHISPER_CUDA_IMAGE="localhost/whisper-cuda:latest"

# ── GPU backend selection (ipex or vulkan) ───────────────────────
# Set by detect-gpu.sh or override manually
export LLM_ARC_GPU_BACKEND="${LLM_ARC_GPU_BACKEND:-ipex}"

# ── Intel GPU environment (for SYCL/IPEX containers) ────────────
export LLM_ARC_GPU_ENV_VARS=(
    "ZES_ENABLE_SYSMAN=1"
    "SYCL_CACHE_PERSISTENT=1"
    "SYCL_PI_LEVEL_ZERO_USE_IMMEDIATE_COMMANDLISTS=1"
    "BIGDL_LLM_XMX_DISABLED=0"
    "OLLAMA_HOST=0.0.0.0"
    "OLLAMA_ORIGINS=*"
    "OLLAMA_NUM_GPU=999"
)

# ── NVIDIA GPU environment (for CUDA containers) ──────────────────
export LLM_ARC_NVIDIA_ENV_VARS=(
    "NVIDIA_VISIBLE_DEVICES=all"
    "NVIDIA_DRIVER_CAPABILITIES=compute,utility"
    "OLLAMA_HOST=0.0.0.0"
    "OLLAMA_ORIGINS=*"
    "OLLAMA_NUM_GPU=999"
)

# ── Piper TTS defaults ──────────────────────────────────────────
export LLM_ARC_PIPER_VOICE="${LLM_ARC_PIPER_VOICE:-en_US-amy-medium}"

# ── Colors for script output ────────────────────────────────────
if [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    BLUE='\033[0;34m'
    BOLD='\033[1m'
    RESET='\033[0m'
else
    RED='' GREEN='' YELLOW='' BLUE='' BOLD='' RESET=''
fi

# ── Helper functions ─────────────────────────────────────────────
log_info()  { printf "${BLUE}[INFO]${RESET}  %s\n" "$*"; }
log_ok()    { printf "${GREEN}[OK]${RESET}    %s\n" "$*"; }
log_warn()  { printf "${YELLOW}[WARN]${RESET}  %s\n" "$*"; }
log_error() { printf "${RED}[ERROR]${RESET} %s\n" "$*" >&2; }
log_step()  { printf "${BOLD}▸ %s${RESET}\n" "$*"; }

require_cmd() {
    local cmd="$1"
    if ! command -v "$cmd" &>/dev/null; then
        log_error "Required command not found: $cmd"
        return 1
    fi
}

# Execute ollama CLI inside the container
# Works with both IPEX (binary at /llm/ollama/) and Vulkan (binary at /usr/bin/)
# Usage: ollama_cli pull model   OR   ollama_cli -it run model
ollama_cli() {
    local flags=()
    while [[ "${1:-}" == -[a-z]* ]]; do flags+=("$1"); shift; done
    podman exec "${flags[@]}" ollama \
        sh -c 'PATH="/llm/ollama:$PATH" exec ollama "$@"' _ "$@"
}

# ── GPU detection cache (populated by detect-gpu.sh) ────────────
# These are overwritten when detect-gpu.sh runs
export LLM_ARC_GPU_VENDOR="${LLM_ARC_GPU_VENDOR:-unknown}"
export LLM_ARC_GPU_TYPE="${LLM_ARC_GPU_TYPE:-unknown}"
export LLM_ARC_GPU_DEVICE="${LLM_ARC_GPU_DEVICE:-}"
export LLM_ARC_GPU_RENDER_NODE="${LLM_ARC_GPU_RENDER_NODE:-/dev/dri/renderD128}"
export LLM_ARC_GPU_CARD_NODE="${LLM_ARC_GPU_CARD_NODE:-/dev/dri/card1}"
export LLM_ARC_GPU_VRAM_MB="${LLM_ARC_GPU_VRAM_MB:-0}"
export LLM_ARC_GPU_NAME="${LLM_ARC_GPU_NAME:-}"
