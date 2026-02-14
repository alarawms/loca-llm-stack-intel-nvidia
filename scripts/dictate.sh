#!/usr/bin/env bash
# dictate.sh — Audio capture and transcription helper
#
# Records from PipeWire/PulseAudio default source, sends to the
# containerized whisper server, and outputs transcribed text.
#
# Usage:
#   ./dictate.sh start           Start recording
#   ./dictate.sh stop            Stop recording and transcribe
#   ./dictate.sh clip            Record, transcribe, copy to clipboard
#   ./dictate.sh file <path>     Transcribe an existing audio file
#   ./dictate.sh toggle          Toggle recording on/off

source "$(dirname "${BASH_SOURCE[0]}")/env.sh"

WHISPER_URL="http://127.0.0.1:${LLM_ARC_WHISPER_PORT}/inference"
PID_FILE="/tmp/llm-arc-dictate.pid"
AUDIO_FILE="/tmp/llm-arc-dictate.wav"

# ── Recording ────────────────────────────────────────────────────

start_recording() {
    if [[ -f "$PID_FILE" ]]; then
        local pid
        pid=$(cat "$PID_FILE")
        if kill -0 "$pid" 2>/dev/null; then
            log_warn "Already recording (PID $pid)"
            return 0
        fi
        rm -f "$PID_FILE"
    fi

    rm -f "$AUDIO_FILE"

    # Prefer pw-record (PipeWire native), fall back to ffmpeg
    if command -v pw-record &>/dev/null; then
        pw-record --format=s16 --rate=16000 --channels=1 "$AUDIO_FILE" &
    elif command -v ffmpeg &>/dev/null; then
        ffmpeg -y -f pulse -i default -ar 16000 -ac 1 -f wav "$AUDIO_FILE" \
            -loglevel quiet &
    else
        log_error "Neither pw-record nor ffmpeg found. Install one."
        exit 1
    fi

    echo $! > "$PID_FILE"
    log_ok "Recording started (PID $(cat "$PID_FILE"))"
}

stop_recording() {
    if [[ ! -f "$PID_FILE" ]]; then
        log_error "Not currently recording"
        return 1
    fi

    local pid
    pid=$(cat "$PID_FILE")
    kill "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    rm -f "$PID_FILE"

    # Give the file a moment to finalize
    sleep 0.3

    if [[ ! -s "$AUDIO_FILE" ]]; then
        log_error "Recording file is empty"
        return 1
    fi

    log_ok "Recording stopped"
}

# ── Transcription ────────────────────────────────────────────────

transcribe_file() {
    local file="$1"

    if [[ ! -f "$file" ]]; then
        log_error "File not found: $file"
        return 1
    fi

    if ! curl -sf "http://127.0.0.1:${LLM_ARC_WHISPER_PORT}/health" >/dev/null 2>&1; then
        log_error "Whisper server not responding. Is the stack running?"
        return 1
    fi

    local response
    response=$(curl -sf -X POST "$WHISPER_URL" \
        -F "file=@${file}" \
        -F "response_format=json" \
        -F "temperature=0.0")

    if command -v jq &>/dev/null; then
        echo "$response" | jq -r '.text // .error // "No transcription"'
    else
        echo "$response"
    fi
}

# ── Commands ─────────────────────────────────────────────────────

cmd_start() {
    start_recording
}

cmd_stop() {
    stop_recording || exit 1
    log_info "Transcribing..."
    transcribe_file "$AUDIO_FILE"
}

cmd_clip() {
    # If already recording, stop and transcribe
    if [[ -f "$PID_FILE" ]]; then
        stop_recording || exit 1
    else
        # Quick record for 5 seconds
        log_info "Recording for 5 seconds..."
        rm -f "$AUDIO_FILE"
        if command -v ffmpeg &>/dev/null; then
            ffmpeg -y -f pulse -i default -t 5 -ar 16000 -ac 1 \
                -f wav "$AUDIO_FILE" -loglevel quiet
        elif command -v pw-record &>/dev/null; then
            timeout 5 pw-record --format=s16 --rate=16000 --channels=1 "$AUDIO_FILE" || true
        fi
    fi

    log_info "Transcribing..."
    local text
    text=$(transcribe_file "$AUDIO_FILE")

    echo "$text"

    # Copy to clipboard
    if command -v wl-copy &>/dev/null; then
        echo -n "$text" | wl-copy
        log_ok "Copied to clipboard (Wayland)"
    elif command -v xclip &>/dev/null; then
        echo -n "$text" | xclip -selection clipboard
        log_ok "Copied to clipboard (X11)"
    fi
}

cmd_file() {
    local file="$1"
    [[ -z "$file" ]] && { log_error "Usage: dictate.sh file <path>"; exit 1; }
    transcribe_file "$file"
}

cmd_toggle() {
    if [[ -f "$PID_FILE" ]] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null; then
        cmd_stop
    else
        cmd_start
    fi
}

# ── Main ─────────────────────────────────────────────────────────
case "${1:-help}" in
    start)  cmd_start ;;
    stop)   cmd_stop ;;
    clip)   cmd_clip ;;
    file)   cmd_file "${2:-}" ;;
    toggle) cmd_toggle ;;
    help|*)
        echo "Usage: $(basename "$0") <command>"
        echo ""
        echo "Commands:"
        echo "  start         Start recording from microphone"
        echo "  stop          Stop recording and transcribe"
        echo "  clip          Record briefly, transcribe, copy to clipboard"
        echo "  file <path>   Transcribe an existing audio file"
        echo "  toggle        Toggle recording on/off"
        ;;
esac
