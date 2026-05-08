#!/usr/bin/env bash
# openclaw-preconfigure.sh — pre-seed OpenClaw config so the only first-run
# step left for the user is the token paste in the browser.
#
# What it writes (via OpenClaw's `config patch` CLI, schema-validated):
#   - gateway.mode = local
#   - models.providers.ollama  → baseUrl=http://ollama:11434, api=ollama,
#                                model catalog filled from `ollama list`
#   - agents.list[0]           → default agent "Local Pi (Ollama)" using
#                                the first locally-pulled model as primary
#                                and the rest as fallbacks
#
# Why this exists: OpenClaw stores the gateway shared secret in the browser's
# localStorage (per docs.openclaw.ai/install/docker), which is unreachable from
# the server. We pre-configure everything else so once the user pastes the
# token in Settings, the model picker is already populated and the default
# agent already points at a local model.
#
# Idempotent — safe to re-run after pulling more Ollama models. Restarts
# openclaw.service so the gateway picks up the new config.
#
# Usage:
#   ./scripts/openclaw-preconfigure.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/env.sh"

# ── Wait for required containers ─────────────────────────────────
require_running() {
    local name="$1" deadline=$((SECONDS + 60))
    while (( SECONDS < deadline )); do
        if podman container inspect "$name" --format '{{.State.Running}}' 2>/dev/null \
            | grep -qx true; then
            return 0
        fi
        sleep 2
    done
    log_error "$name container is not running (waited 60s)"
    log_info "  Start the stack first: ./scripts/ai-stack.sh start"
    return 1
}

require_running ollama
require_running openclaw

# ── Discover Ollama models ───────────────────────────────────────
# `ollama list` columns: NAME  ID  SIZE  UNIT  MODIFIED.
# Sort largest-first so the primary defaults to the user's strongest model
# (smaller starter models like llama3.2:1b end up as fallbacks). User can
# reorder in the UI later.
mapfile -t MODELS < <(podman exec ollama ollama list 2>/dev/null \
    | awk 'NR>1 && $1 != "" {
        m = 1
        if      ($4 == "GB") m = 1024*1024*1024
        else if ($4 == "MB") m = 1024*1024
        else if ($4 == "KB") m = 1024
        printf "%015d\t%s\n", $3 * m, $1
    }' | sort -rn | cut -f2)

if (( ${#MODELS[@]} == 0 )); then
    log_warn "Ollama has no models pulled yet — nothing to pre-configure."
    log_info "  Pull at least one first: ./scripts/models.sh pull <model>"
    log_info "  Then re-run: ./scripts/openclaw-preconfigure.sh"
    exit 1
fi

PRIMARY="${MODELS[0]}"
FALLBACKS=("${MODELS[@]:1}")

# ── Build the JSON5 patch ────────────────────────────────────────
build_patch() {
    cat <<HEADER
{
  gateway: { mode: "local" },
  models: {
    mode: "merge",
    providers: {
      ollama: {
        baseUrl: "http://ollama:11434",
        api: "ollama",
        timeoutSeconds: 600,
        models: [
HEADER
    for m in "${MODELS[@]}"; do
        printf '          { id: "ollama/%s", name: "%s" },\n' "$m" "$m"
    done
    cat <<MIDDLE
        ],
      },
    },
  },
  agents: {
    list: [
      {
        id: "default",
        default: true,
        name: "Local Pi (Ollama)",
        model: {
          primary: "ollama/${PRIMARY}",
MIDDLE
    if (( ${#FALLBACKS[@]} > 0 )); then
        printf '          fallbacks: [\n'
        for m in "${FALLBACKS[@]}"; do
            printf '            "ollama/%s",\n' "$m"
        done
        printf '          ],\n'
    fi
    cat <<TAIL
        },
      },
    ],
  },
}
TAIL
}

PATCH="$(build_patch)"

# ── Validate then apply ──────────────────────────────────────────
log_info "Validating config patch (${#MODELS[@]} models, primary=${PRIMARY})..."
if ! printf '%s\n' "$PATCH" \
    | podman exec -i openclaw node /app/dist/index.js config patch --stdin --dry-run \
        >/dev/null; then
    log_error "Schema validation failed. Patch dump for debugging:"
    printf '%s\n' "$PATCH" >&2
    exit 1
fi

log_info "Applying patch..."
printf '%s\n' "$PATCH" \
    | podman exec -i openclaw node /app/dist/index.js config patch --stdin

log_info "Restarting openclaw.service..."
systemctl --user restart openclaw.service

log_ok "OpenClaw pre-configured."

# ── Print the final manual step ──────────────────────────────────
token_file="${LLM_ARC_MODELS}/openclaw/openclaw.env"
token=""
[[ -f "$token_file" ]] && token="$(grep '^OPENCLAW_GATEWAY_TOKEN=' "$token_file" | cut -d= -f2-)"

echo ""
log_info "Final step (browser, one time per browser profile):"
log_info "  1. Open http://localhost:${LLM_ARC_OPENCLAW_PORT}"
log_info "  2. Settings → paste the gateway token below"
if [[ -n "$token" ]]; then
    log_info "     Token: ${token}"
else
    log_warn "     Token file not found at ${token_file}"
fi
log_info "  3. Pick a model in the UI (qwen3.6/gemma4/nemotron/llama3.2 already in catalog)"
