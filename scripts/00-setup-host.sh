#!/usr/bin/env bash
# 00-setup-host.sh — Verify host prerequisites for the llm-arc AI stack
#
# Checks:
#   - User is in render and video groups
#   - Kernel version >= 6.2
#   - /dev/dri/renderD* exists and is accessible
#   - Podman is installed
#   - loginctl linger is enabled
#   - Directory structure exists
#
# Usage: ./scripts/00-setup-host.sh [--fix]
#   --fix  Attempt to fix issues automatically (may prompt for sudo)

source "$(dirname "${BASH_SOURCE[0]}")/env.sh"

FIX_MODE=0
[[ "${1:-}" == "--fix" ]] && FIX_MODE=1

ERRORS=0
WARNINGS=0

check() {
    local desc="$1"
    shift
    if "$@" 2>/dev/null; then
        log_ok "$desc"
        return 0
    else
        log_error "FAIL: $desc"
        (( ERRORS++ )) || true
        return 1
    fi
}

warn_check() {
    local desc="$1"
    shift
    if "$@" 2>/dev/null; then
        log_ok "$desc"
        return 0
    else
        log_warn "$desc"
        (( WARNINGS++ )) || true
        return 1
    fi
}

# ── Kernel version ───────────────────────────────────────────────
log_step "Checking kernel version"
check_kernel() {
    local kver
    kver=$(uname -r)
    local major minor
    major=$(echo "$kver" | cut -d. -f1)
    minor=$(echo "$kver" | cut -d. -f2)
    (( major > 6 || (major == 6 && minor >= 2) ))
}
check "Kernel >= 6.2 (have: $(uname -r))" check_kernel

# ── User groups ──────────────────────────────────────────────────
log_step "Checking user groups"

check_group() { id -nG | tr ' ' '\n' | grep -qx "$1"; }

if ! check "User $(whoami) in 'render' group" check_group render; then
    if [[ $FIX_MODE -eq 1 ]]; then
        log_info "Attempting: sudo usermod -aG render $USER"
        sudo usermod -aG render "$USER" && log_ok "Added to render group (re-login required)"
    else
        log_info "Fix: sudo usermod -aG render $USER"
    fi
fi

if ! check "User $(whoami) in 'video' group" check_group video; then
    if [[ $FIX_MODE -eq 1 ]]; then
        log_info "Attempting: sudo usermod -aG video $USER"
        sudo usermod -aG video "$USER" && log_ok "Added to video group (re-login required)"
    else
        log_info "Fix: sudo usermod -aG video $USER"
    fi
fi

# ── GPU device nodes ─────────────────────────────────────────────
log_step "Checking GPU device nodes"

check_render_node() {
    compgen -G "/dev/dri/renderD*" > /dev/null
}
check "Render node exists (/dev/dri/renderD*)" check_render_node

check_render_access() {
    for node in /dev/dri/renderD*; do
        [[ -r "$node" && -w "$node" ]] && return 0
    done
    return 1
}
check "Render node is read/write accessible" check_render_access

# ── Required commands ────────────────────────────────────────────
log_step "Checking required tools"

check "Podman installed" command -v podman
check "systemctl available" command -v systemctl
check "curl installed" command -v curl
warn_check "jq installed (optional, for JSON parsing)" command -v jq
warn_check "ffmpeg installed (needed for dictation)" command -v ffmpeg

# ── Podman version ───────────────────────────────────────────────
if command -v podman &>/dev/null; then
    podman_ver=$(podman --version | awk '{print $NF}')
    check_podman_ver() {
        local major minor
        major=$(echo "$podman_ver" | cut -d. -f1)
        minor=$(echo "$podman_ver" | cut -d. -f2)
        (( major > 4 || (major == 4 && minor >= 4) ))
    }
    check "Podman >= 4.4 for Quadlet support (have: $podman_ver)" check_podman_ver
fi

# ── loginctl linger ──────────────────────────────────────────────
log_step "Checking session persistence"

check_linger() {
    local linger_file="/var/lib/systemd/linger/$USER"
    [[ -f "$linger_file" ]]
}

if ! check "loginctl linger enabled for $USER" check_linger; then
    if [[ $FIX_MODE -eq 1 ]]; then
        log_info "Attempting: loginctl enable-linger $USER"
        loginctl enable-linger "$USER" && log_ok "Linger enabled"
    else
        log_info "Fix: loginctl enable-linger $USER"
    fi
fi

# ── Directory structure ──────────────────────────────────────────
log_step "Checking directory structure"

ensure_dir() {
    if [[ -d "$1" ]]; then
        return 0
    elif [[ $FIX_MODE -eq 1 ]]; then
        mkdir -p "$1"
        log_ok "Created $1"
        return 0
    else
        return 1
    fi
}

check "Model dir: $LLM_ARC_OLLAMA_MODELS" ensure_dir "$LLM_ARC_OLLAMA_MODELS"
check "Model dir: $LLM_ARC_WHISPER_MODELS" ensure_dir "$LLM_ARC_WHISPER_MODELS"
check "Model dir: $LLM_ARC_PIPER_MODELS" ensure_dir "$LLM_ARC_PIPER_MODELS"
check "Quadlet dir: $LLM_ARC_QUADLET_DIR" ensure_dir "$LLM_ARC_QUADLET_DIR"

# ── GPU detection ────────────────────────────────────────────────
log_step "Running GPU detection"

if source "${LLM_ARC_SCRIPTS}/detect-gpu.sh" 2>/dev/null; then
    log_ok "Detected: $LLM_ARC_GPU_NAME"
    log_info "Type: $LLM_ARC_GPU_TYPE | Backend: $LLM_ARC_GPU_BACKEND | VRAM: ~${LLM_ARC_GPU_VRAM_MB}MB"
else
    log_error "GPU detection failed"
    (( ERRORS++ )) || true
fi

# ── Summary ──────────────────────────────────────────────────────
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [[ $ERRORS -eq 0 ]]; then
    log_ok "All checks passed! ($WARNINGS warnings)"
    echo ""
    log_info "Next: ./scripts/ai-stack.sh install"
else
    log_error "$ERRORS check(s) failed ($WARNINGS warnings)"
    if [[ $FIX_MODE -eq 0 ]]; then
        echo ""
        log_info "Re-run with --fix to attempt automatic fixes:"
        log_info "  ./scripts/00-setup-host.sh --fix"
    fi
    exit 1
fi
