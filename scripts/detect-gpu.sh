#!/usr/bin/env bash
# detect-gpu.sh — Auto-detect Intel GPU type, render node, and VRAM
#
# Enumerates /sys/class/drm/ for Intel devices (vendor 0x8086),
# classifies as iGPU or dGPU, and exports env vars for other scripts.
#
# Usage:
#   source scripts/detect-gpu.sh        # sets env vars in current shell
#   ./scripts/detect-gpu.sh             # prints detection results
#   ./scripts/detect-gpu.sh --env       # prints export statements (eval-able)

source "$(dirname "${BASH_SOURCE[0]}")/env.sh"

# ── Intel PCI device ID classification ───────────────────────────
# Map known device ID prefixes to GPU families.
# See: https://pci-ids.ucw.cz/read/PC/8086
classify_gpu() {
    local device_id="$1"

    case "$device_id" in
        # Arrow Lake / Arrow Lake-P iGPU
        0x7d4*|0x7d5*|0x7d6*)
            echo "igpu:arrow-lake"
            ;;
        # Meteor Lake iGPU
        0x7d0*|0x7d1*|0x7d2*)
            echo "igpu:meteor-lake"
            ;;
        # Lunar Lake iGPU
        0x64a*|0x64b*)
            echo "igpu:lunar-lake"
            ;;
        # Raptor Lake / Alder Lake iGPU
        0xa78*|0xa72*|0x468*|0x462*|0x46a*|0x46b*|0x46c*|0x46d*)
            echo "igpu:alder-lake"
            ;;
        # Tiger Lake iGPU
        0x9a4*)
            echo "igpu:tiger-lake"
            ;;
        # Arc A-series (Alchemist) discrete
        0x5690|0x5691|0x5692|0x5693|0x5694|0x5695|0x5696|0x5697)
            echo "dgpu:arc-a-series"
            ;;
        0x56a0|0x56a1|0x56a2|0x56a5|0x56a6)
            echo "dgpu:arc-a-series"
            ;;
        # Arc B-series (Battlemage) discrete
        0xe202|0xe20b|0xe20c|0xe20d|0xe212)
            echo "dgpu:arc-b-series"
            ;;
        # Arc Pro series
        0x56c0|0x56c1|0x56c2)
            echo "dgpu:arc-pro"
            ;;
        # Catch-all for other Intel GPUs
        *)
            echo "unknown:$device_id"
            ;;
    esac
}

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

# ── Estimate VRAM based on GPU class ─────────────────────────────
estimate_vram_mb() {
    local class="$1"
    local device_id="$2"

    case "$class" in
        igpu:*)
            # iGPU shares system RAM; estimate available portion
            local total_ram_mb
            total_ram_mb=$(awk '/MemTotal/ {printf "%d", $2/1024}' /proc/meminfo)
            # iGPU typically gets up to half of system RAM, but realistically 4-8GB usable
            local estimate=$(( total_ram_mb / 4 ))
            # Cap at reasonable max for iGPU
            if (( estimate > 8192 )); then estimate=8192; fi
            echo "$estimate"
            ;;
        dgpu:arc-a-series)
            case "$device_id" in
                0x5690|0x5691|0x5692) echo "16384" ;; # A770 16GB
                0x5693|0x5694)        echo "8192"  ;; # A750 8GB
                0x56a5|0x56a6)        echo "6144"  ;; # A380 6GB
                *)                    echo "8192"  ;; # default A-series
            esac
            ;;
        dgpu:arc-b-series)
            case "$device_id" in
                0xe202|0xe20b) echo "12288" ;; # B580 12GB
                0xe20c|0xe20d) echo "10240" ;; # B570 10GB
                *)             echo "12288" ;; # default B-series
            esac
            ;;
        dgpu:arc-pro)
            echo "8192"  # varies, conservative default
            ;;
        *)
            echo "0"
            ;;
    esac
}

# ── Recommend backend based on GPU type ──────────────────────────
recommend_backend() {
    local class="$1"
    case "$class" in
        igpu:*)            echo "ipex"   ;; # SYCL/IPEX better for iGPU
        dgpu:arc-a-series) echo "vulkan" ;; # Vulkan 40-100% faster on dGPU
        dgpu:arc-b-series) echo "vulkan" ;; # Vulkan preferred for Battlemage
        dgpu:arc-pro)      echo "ipex"   ;; # Pro series: IPEX is well-tested
        dgpu:nvidia-*)     echo "cuda"   ;; # NVIDIA always uses CUDA
        *)                 echo "ipex"   ;; # fallback
    esac
}

# ── Recommend models based on VRAM ───────────────────────────────
recommend_models() {
    local vram_mb="$1"

    if (( vram_mb >= 16384 )); then
        echo "llama3.1:8b, mistral:7b, qwen2.5-coder:14b-q4, deepseek-coder-v2:16b-q4"
    elif (( vram_mb >= 10240 )); then
        echo "llama3.1:8b, qwen2.5-coder:7b, gemma2:9b-q4"
    elif (( vram_mb >= 6144 )); then
        echo "llama3.1:8b-q4, qwen2.5-coder:7b-q4, phi3:3.8b"
    elif (( vram_mb >= 4096 )); then
        echo "llama3.2:3b, phi3:3.8b, qwen2.5-coder:3b"
    else
        echo "llama3.2:1b, phi3:mini, qwen2.5-coder:1.5b"
    fi
}

# ── Main detection logic ─────────────────────────────────────────
detect_gpu() {
    local found=0
    local best_class=""
    local best_device_id=""
    local best_render_node=""
    local best_card_node=""
    local best_name=""

    for card_dir in /sys/class/drm/card[0-9]*; do
        [[ -d "$card_dir/device" ]] || continue

        local vendor_file="$card_dir/device/vendor"
        [[ -f "$vendor_file" ]] || continue

        local vendor
        vendor=$(cat "$vendor_file" 2>/dev/null)
        [[ "$vendor" == "0x8086" ]] || continue

        local device_id
        device_id=$(cat "$card_dir/device/device" 2>/dev/null)

        local class
        class=$(classify_gpu "$device_id")

        local card_name
        card_name=$(basename "$card_dir")
        local card_num="${card_name#card}"
        local render_node="/dev/dri/renderD$((128 + card_num))"
        local card_node="/dev/dri/$card_name"

        # Read GPU name from driver if available
        local gpu_name=""
        if [[ -f "$card_dir/device/label" ]]; then
            gpu_name=$(cat "$card_dir/device/label" 2>/dev/null)
        fi
        if [[ -z "$gpu_name" ]] && command -v lspci &>/dev/null; then
            local pci_slot
            pci_slot=$(basename "$(readlink -f "$card_dir/device")" 2>/dev/null)
            if [[ -n "$pci_slot" ]]; then
                gpu_name=$(lspci -s "$pci_slot" -nn 2>/dev/null | sed 's/.*: //' || true)
            fi
        fi

        # Prefer dGPU over iGPU if both exist
        if [[ "$class" == dgpu:* ]] || [[ $found -eq 0 ]]; then
            best_class="$class"
            best_device_id="$device_id"
            best_render_node="$render_node"
            best_card_node="$card_node"
            best_name="${gpu_name:-Intel GPU ($device_id)}"
            found=1
            # If we found a dGPU, stop looking (prefer discrete)
            [[ "$class" == dgpu:* ]] && break
        fi
    done

    if [[ $found -eq 0 ]]; then
        log_error "No Intel GPU detected in /sys/class/drm/"
        return 1
    fi

    local vram_mb
    vram_mb=$(estimate_vram_mb "$best_class" "$best_device_id")

    local backend
    backend=$(recommend_backend "$best_class")

    local models
    models=$(recommend_models "$vram_mb")

    local gpu_type="${best_class%%:*}"  # igpu or dgpu

    # Export results
    export LLM_ARC_GPU_TYPE="$gpu_type"
    export LLM_ARC_GPU_CLASS="$best_class"
    export LLM_ARC_GPU_DEVICE="$best_device_id"
    export LLM_ARC_GPU_RENDER_NODE="$best_render_node"
    export LLM_ARC_GPU_CARD_NODE="$best_card_node"
    export LLM_ARC_GPU_VRAM_MB="$vram_mb"
    export LLM_ARC_GPU_NAME="$best_name"
    export LLM_ARC_GPU_BACKEND="$backend"
    export LLM_ARC_GPU_RECOMMENDED_MODELS="$models"
    export LLM_ARC_GPU_VENDOR="intel"

    return 0
}

# ── Output formatting ────────────────────────────────────────────
print_results() {
    log_step "GPU Detection Results"
    echo ""
    printf "  %-22s %s\n" "Vendor:"       "$LLM_ARC_GPU_VENDOR"
    printf "  %-22s %s\n" "GPU Name:"     "$LLM_ARC_GPU_NAME"
    printf "  %-22s %s\n" "Type:"          "$LLM_ARC_GPU_TYPE ($LLM_ARC_GPU_CLASS)"
    printf "  %-22s %s\n" "Device ID:"     "$LLM_ARC_GPU_DEVICE"
    printf "  %-22s %s\n" "Render Node:"   "$LLM_ARC_GPU_RENDER_NODE"
    printf "  %-22s %s\n" "Card Node:"     "$LLM_ARC_GPU_CARD_NODE"
    printf "  %-22s %s MB\n" "Est. VRAM:"  "$LLM_ARC_GPU_VRAM_MB"
    printf "  %-22s %s\n" "Rec. Backend:"  "$LLM_ARC_GPU_BACKEND"
    printf "  %-22s %s\n" "Rec. Models:"   "$LLM_ARC_GPU_RECOMMENDED_MODELS"
    echo ""

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
}

print_env() {
    echo "export LLM_ARC_GPU_VENDOR=\"$LLM_ARC_GPU_VENDOR\""
    echo "export LLM_ARC_GPU_TYPE=\"$LLM_ARC_GPU_TYPE\""
    echo "export LLM_ARC_GPU_CLASS=\"$LLM_ARC_GPU_CLASS\""
    echo "export LLM_ARC_GPU_DEVICE=\"$LLM_ARC_GPU_DEVICE\""
    echo "export LLM_ARC_GPU_RENDER_NODE=\"$LLM_ARC_GPU_RENDER_NODE\""
    echo "export LLM_ARC_GPU_CARD_NODE=\"$LLM_ARC_GPU_CARD_NODE\""
    echo "export LLM_ARC_GPU_VRAM_MB=\"$LLM_ARC_GPU_VRAM_MB\""
    echo "export LLM_ARC_GPU_NAME=\"$LLM_ARC_GPU_NAME\""
    echo "export LLM_ARC_GPU_BACKEND=\"$LLM_ARC_GPU_BACKEND\""
    echo "export LLM_ARC_GPU_RECOMMENDED_MODELS=\"$LLM_ARC_GPU_RECOMMENDED_MODELS\""
}

# ── Save detection to config cache ───────────────────────────────
save_config() {
    local config_file="${LLM_ARC_CONFIG}/gpu.env"
    mkdir -p "$(dirname "$config_file")"
    print_env > "$config_file"
    log_ok "GPU config saved to $config_file"
}

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
