#!/usr/bin/env bash
# 00-setup-host.sh — Host prerequisite checker for the llm-arc AI stack
# Run this before install.sh to verify your system is ready.
#
# Exit codes: 0 = all good (or warnings only), 1 = blocking errors found

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/env.sh"

ERRORS=0
WARNINGS=0

check_fail() { log_error "$*"; ERRORS=$(( ERRORS + 1 )); }
check_warn() { log_warn  "$*"; WARNINGS=$(( WARNINGS + 1 )); }

echo ""
echo "╔══════════════════════════════════════════════════════╗"
echo "║         llm-arc — Host Prerequisite Checker          ║"
echo "╚══════════════════════════════════════════════════════╝"
echo ""

# ── 1. Kernel version ────────────────────────────────────────
log_step "1. Kernel version"

kernel=$(uname -r)
k_major=$(echo "$kernel" | cut -d. -f1)
k_minor=$(echo "$kernel" | cut -d. -f2 | grep -oP '^\d+')

if [[ "$k_major" -gt 6 ]] || { [[ "$k_major" -eq 6 ]] && [[ "$k_minor" -ge 2 ]]; }; then
    log_ok "Kernel $kernel (>= 6.2 required)"
else
    check_fail "Kernel $kernel is too old — need >= 6.2 for Intel GPU support"
    log_info "  Upgrade: sudo dnf upgrade kernel"
fi

# ── 2. Required tools ────────────────────────────────────────
log_step "2. Required tools"

require_tool() {
    local cmd="$1" pkg="${2:-$1}"
    if command -v "$cmd" &>/dev/null; then
        log_ok "$cmd  ($(command -v "$cmd"))"
    else
        check_fail "$cmd not found — install: sudo dnf install $pkg"
    fi
}

optional_tool() {
    local cmd="$1" pkg="${2:-$1}" reason="$3"
    if command -v "$cmd" &>/dev/null; then
        log_ok "$cmd  ($(command -v "$cmd"))"
    else
        check_warn "$cmd not found — $reason"
        log_info "  Install: sudo dnf install $pkg"
    fi
}

require_tool podman podman
require_tool curl    curl

optional_tool jq  jq  "used by status/benchmark commands"
optional_tool wget wget "used by SearXNG health check inside container (Alpine)"

# Podman version: need >= 4.4 for Quadlet
if command -v podman &>/dev/null; then
    podman_ver=$(podman --version 2>/dev/null | grep -oP '\d+\.\d+' | head -1)
    p_major=$(echo "$podman_ver" | cut -d. -f1)
    p_minor=$(echo "$podman_ver" | cut -d. -f2)
    if [[ "$p_major" -gt 4 ]] || { [[ "$p_major" -eq 4 ]] && [[ "$p_minor" -ge 4 ]]; }; then
        log_ok "Podman $podman_ver (>= 4.4 required for Quadlet)"
    else
        check_fail "Podman $podman_ver is too old — need >= 4.4 for Quadlet support"
        log_info "  Upgrade: sudo dnf upgrade podman"
    fi
fi

# ── 3. GPU detection ─────────────────────────────────────────
log_step "3. GPU detection"

GPU_VENDOR="none"

if command -v nvidia-smi &>/dev/null && nvidia-smi &>/dev/null 2>&1; then
    GPU_VENDOR="nvidia"
    gpu_name=$(nvidia-smi --query-gpu=name --format=csv,noheader 2>/dev/null | head -1 || echo "unknown")
    driver_ver=$(nvidia-smi --query-gpu=driver_version --format=csv,noheader 2>/dev/null | head -1 || echo "unknown")
    log_ok "NVIDIA GPU: $gpu_name (driver $driver_ver)"
elif compgen -G "/dev/dri/renderD*" >/dev/null 2>&1; then
    GPU_VENDOR="intel"
    for node in /dev/dri/renderD*; do
        log_ok "Intel/AMD render node: $node"
    done
else
    check_fail "No GPU detected — no /dev/dri/renderD* nodes found and nvidia-smi not available"
fi

# ── 4. GPU-specific checks ────────────────────────────────────
log_step "4. GPU access and drivers"

if [[ "$GPU_VENDOR" == "nvidia" ]]; then
    # nvidia-container-toolkit: required for CDI GPU passthrough in Podman
    if command -v nvidia-ctk &>/dev/null; then
        ctk_ver=$(nvidia-ctk --version 2>/dev/null | head -1 || echo "")
        log_ok "nvidia-container-toolkit found${ctk_ver:+ ($ctk_ver)}"
    else
        check_fail "nvidia-container-toolkit not found — required for GPU passthrough in Podman"
        log_info "  Install (Fedora/RHEL): sudo dnf install nvidia-container-toolkit"
        log_info "  Install (Ubuntu/Debian): sudo apt install nvidia-container-toolkit"
        log_info "  See: https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html"
    fi

    # CDI spec: generated from toolkit, consumed by Podman at runtime
    if [[ -f /etc/cdi/nvidia.yaml ]]; then
        log_ok "NVIDIA CDI spec: /etc/cdi/nvidia.yaml"
    else
        check_fail "NVIDIA CDI spec missing at /etc/cdi/nvidia.yaml"
        log_info "  Generate: sudo nvidia-ctk cdi generate --output=/etc/cdi/nvidia.yaml"
        log_info "  Regenerate after each driver upgrade"
    fi

    # nvidia-smi sanity: confirm driver stack is functional
    if nvidia-smi &>/dev/null 2>&1; then
        log_ok "nvidia-smi: driver stack functional"
    else
        check_fail "nvidia-smi found but failed to query GPU — driver may be misconfigured"
        log_info "  Check: nvidia-smi -q"
    fi

elif [[ "$GPU_VENDOR" == "intel" ]]; then
    # render group: required to open /dev/dri/renderD* without root
    if id -nG | tr ' ' '\n' | grep -qx render; then
        log_ok "User '$USER' is in the 'render' group"
    else
        check_fail "User '$USER' is not in the 'render' group — GPU access denied"
        log_info "  Fix: sudo usermod -aG render $USER"
        log_info "  Then log out and back in (or: newgrp render)"
    fi

    # video group: needed by some Intel GPU tools and Vulkan
    if id -nG | tr ' ' '\n' | grep -qx video; then
        log_ok "User '$USER' is in the 'video' group"
    else
        check_warn "User '$USER' is not in the 'video' group (may affect Vulkan / display access)"
        log_info "  Fix: sudo usermod -aG video $USER"
    fi

    # Check render node permissions
    for node in /dev/dri/renderD*; do
        if [[ -r "$node" ]]; then
            log_ok "$node is readable"
        else
            check_fail "$node exists but is not readable — group membership may not have taken effect"
            log_info "  Try: newgrp render  (or log out and back in)"
        fi
    done
fi

# ── 5. Systemd user session ──────────────────────────────────
log_step "5. Systemd user session"

if systemctl --user status &>/dev/null 2>&1; then
    log_ok "Systemd user session active"
else
    check_fail "Systemd user session not available — required for rootless Podman + Quadlet"
    log_info "  This usually means you're logged in via su/sudo rather than a real login session"
fi

if [[ -f "/var/lib/systemd/linger/$USER" ]]; then
    log_ok "Loginctl linger enabled (services survive logout)"
else
    check_warn "Loginctl linger not enabled — services will stop on logout and won't auto-start on boot"
    log_info "  Fix: loginctl enable-linger $USER"
fi

# ── 6. Summary ───────────────────────────────────────────────
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if (( ERRORS > 0 )); then
    log_error "$ERRORS error(s) and $WARNINGS warning(s) found — fix errors before running install.sh"
    echo ""
    exit 1
elif (( WARNINGS > 0 )); then
    log_warn "$WARNINGS warning(s) — you can proceed but check the items above"
    echo ""
    log_info "Ready to install:  ./install.sh"
    exit 0
else
    log_ok "All checks passed!"
    echo ""
    log_info "Ready to install:  ./install.sh"
    exit 0
fi
