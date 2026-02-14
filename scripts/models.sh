#!/usr/bin/env bash
# models.sh — Model management CLI for the llm-arc AI stack
#
# Usage:
#   ./models.sh list              List installed models
#   ./models.sh pull <model>      Pull a model from Ollama registry
#   ./models.sh remove <model>    Remove a model
#   ./models.sh recommend         Suggest models based on GPU VRAM
#   ./models.sh info <model>      Show model details
#   ./models.sh chat <model>      Start interactive chat
#   ./models.sh test [model]      Quick generation test

source "$(dirname "${BASH_SOURCE[0]}")/env.sh"

OLLAMA_URL="http://127.0.0.1:${LLM_ARC_OLLAMA_PORT}"

# ── Check Ollama is running ──────────────────────────────────────
check_ollama() {
    if ! curl -sf "${OLLAMA_URL}/api/tags" >/dev/null 2>&1; then
        log_error "Ollama is not responding at ${OLLAMA_URL}"
        log_info  "Start the stack: ./scripts/ai-stack.sh start"
        exit 1
    fi
}

# ── Commands ─────────────────────────────────────────────────────
cmd_list() {
    check_ollama
    log_step "Installed models"
    echo ""

    local response
    response=$(curl -sf "${OLLAMA_URL}/api/tags")

    if command -v jq &>/dev/null; then
        echo "$response" | jq -r '
            .models[] |
            "  \(.name)\t\(.size / 1073741824 | . * 100 | round / 100)GB\t\(.details.parameter_size // "?")\t\(.details.quantization_level // "?")"
        ' 2>/dev/null | column -t -s $'\t' || echo "$response"
    else
        echo "$response"
    fi
    echo ""
}

cmd_pull() {
    local model="$1"
    [[ -z "$model" ]] && { log_error "Usage: models.sh pull <model>"; exit 1; }

    check_ollama
    log_step "Pulling $model ..."
    ollama_cli pull "$model"
}

cmd_remove() {
    local model="$1"
    [[ -z "$model" ]] && { log_error "Usage: models.sh remove <model>"; exit 1; }

    check_ollama
    log_step "Removing $model ..."
    ollama_cli rm "$model"
}

cmd_info() {
    local model="$1"
    [[ -z "$model" ]] && { log_error "Usage: models.sh info <model>"; exit 1; }

    check_ollama
    local response
    response=$(curl -sf "${OLLAMA_URL}/api/show" -d "{\"name\":\"$model\"}")

    if command -v jq &>/dev/null; then
        echo "$response" | jq '{
            model: .modelinfo."general.architecture",
            parameters: .details.parameter_size,
            quantization: .details.quantization_level,
            family: .details.family,
            format: .details.format,
            context_length: .modelinfo."llama.context_length"
        }'
    else
        echo "$response"
    fi
}

cmd_recommend() {
    # Load GPU detection
    source "${LLM_ARC_SCRIPTS}/detect-gpu.sh" 2>/dev/null || true

    local vram="${LLM_ARC_GPU_VRAM_MB:-0}"

    log_step "Model Recommendations"
    echo ""
    printf "  GPU:  %s\n" "${LLM_ARC_GPU_NAME:-unknown}"
    printf "  VRAM: ~%s MB\n" "$vram"
    printf "  Backend: %s\n" "${LLM_ARC_GPU_BACKEND:-unknown}"
    echo ""

    echo "  ┌──────────────────┬──────────────────────┬────────────────────┐"
    echo "  │ Category         │ Model                │ Notes              │"
    echo "  ├──────────────────┼──────────────────────┼────────────────────┤"

    if (( vram >= 16384 )); then
        echo "  │ Chat             │ llama3.1:8b          │ Best all-round     │"
        echo "  │ Chat             │ mistral:7b           │ Fast, good quality │"
        echo "  │ Code             │ qwen2.5-coder:14b-q4 │ Large code model  │"
        echo "  │ Code             │ deepseek-coder-v2:16b-q4 │ Deep reasoning│"
        echo "  │ Small/Fast       │ phi3:3.8b            │ Quick tasks        │"
    elif (( vram >= 10240 )); then
        echo "  │ Chat             │ llama3.1:8b          │ Fits well in 12GB  │"
        echo "  │ Chat             │ gemma2:9b-q4         │ Good quality       │"
        echo "  │ Code             │ qwen2.5-coder:7b     │ Best code model    │"
        echo "  │ Small/Fast       │ phi3:3.8b            │ Quick tasks        │"
    elif (( vram >= 6144 )); then
        echo "  │ Chat             │ llama3.1:8b-q4       │ Tight fit, works   │"
        echo "  │ Code             │ qwen2.5-coder:7b-q4  │ Good code assist   │"
        echo "  │ Small/Fast       │ phi3:3.8b            │ Comfortable fit    │"
    elif (( vram >= 4096 )); then
        echo "  │ Chat             │ llama3.2:3b          │ Best for ~4GB      │"
        echo "  │ Chat             │ phi3:3.8b            │ Slightly larger    │"
        echo "  │ Code             │ qwen2.5-coder:3b     │ Compact code model │"
    else
        echo "  │ Chat             │ llama3.2:1b          │ Minimal VRAM       │"
        echo "  │ Code             │ qwen2.5-coder:1.5b   │ Smallest code      │"
        echo "  │ Chat             │ phi3:mini            │ Very small          │"
    fi

    echo "  └──────────────────┴──────────────────────┴────────────────────┘"
    echo ""

    log_info "Pull a model:  ./scripts/models.sh pull llama3.2:3b"
}

cmd_chat() {
    local model="${1:-}"
    [[ -z "$model" ]] && { log_error "Usage: models.sh chat <model>"; exit 1; }

    check_ollama
    ollama_cli -it run "$model"
}

cmd_test() {
    check_ollama

    local model="${1:-}"
    if [[ -z "$model" ]]; then
        # Use first available model
        model=$(curl -sf "${OLLAMA_URL}/api/tags" | \
            jq -r '.models[0].name // empty' 2>/dev/null)
        [[ -z "$model" ]] && { log_error "No models installed. Pull one first."; exit 1; }
    fi

    log_step "Testing generation with $model ..."
    local start_time
    start_time=$(date +%s%N)

    local response
    response=$(curl -sf "${OLLAMA_URL}/api/generate" \
        -d "{\"model\":\"$model\",\"prompt\":\"Write a single haiku about computing.\",\"stream\":false}")

    local end_time
    end_time=$(date +%s%N)
    local elapsed_ms=$(( (end_time - start_time) / 1000000 ))

    if command -v jq &>/dev/null; then
        local text eval_count eval_duration
        text=$(echo "$response" | jq -r '.response')
        eval_count=$(echo "$response" | jq -r '.eval_count // 0')
        eval_duration=$(echo "$response" | jq -r '.eval_duration // 0')

        echo ""
        echo "$text"
        echo ""

        if (( eval_duration > 0 )); then
            local tps
            tps=$(echo "$eval_count $eval_duration" | awk '{printf "%.1f", $1 / ($2 / 1000000000)}')
            log_ok "Generated $eval_count tokens at ${tps} tokens/sec (${elapsed_ms}ms total)"
        else
            log_ok "Response received in ${elapsed_ms}ms"
        fi
    else
        echo "$response"
    fi
}

# ── Main ─────────────────────────────────────────────────────────
case "${1:-help}" in
    list)      cmd_list ;;
    pull)      cmd_pull "${2:-}" ;;
    remove|rm) cmd_remove "${2:-}" ;;
    info)      cmd_info "${2:-}" ;;
    recommend) cmd_recommend ;;
    chat|run)  cmd_chat "${2:-}" ;;
    test)      cmd_test "${2:-}" ;;
    help|*)
        echo "Usage: $(basename "$0") <command> [args]"
        echo ""
        echo "Commands:"
        echo "  list              List installed models"
        echo "  pull <model>      Pull a model from Ollama registry"
        echo "  remove <model>    Remove a model"
        echo "  recommend         Suggest models based on your GPU"
        echo "  info <model>      Show model details"
        echo "  chat <model>      Start interactive chat"
        echo "  test [model]      Quick generation speed test"
        ;;
esac
