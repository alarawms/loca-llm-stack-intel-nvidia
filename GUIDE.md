# llm-arc Development Guide

Technical reference for maintaining and extending the llm-arc stack.

## Architecture

```
                    ┌─────────────────────────────────────────┐
                    │           ai-stack pod                   │
                    │  (shared localhost network, no exposure) │
                    │                                          │
    127.0.0.1:11434 │  ┌─────────┐  GPU  ┌──────────┐        │
   ─────────────────┤  │ Ollama  │◄─────►│ /dev/dri │        │
                    │  └─────────┘       └──────────┘        │
     127.0.0.1:9000 │  ┌─────────┐  GPU      │               │
   ─────────────────┤  │ Whisper │◄───────────┘               │
                    │  └─────────┘                            │
     127.0.0.1:5002 │  ┌─────────┐                            │
   ─────────────────┤  │  Piper  │  (CPU only)                │
                    │  └─────────┘                            │
     127.0.0.1:8080 │  ┌─────────┐                            │
   ─────────────────┤  │ Open    │──► localhost:11434 (Ollama) │
                    │  │ WebUI   │                            │
                    │  └─────────┘                            │
                    └─────────────────────────────────────────┘
```

All containers share the pod network. Open WebUI connects to Ollama via `localhost:11434` inside the pod.

## Podman Quadlet

Quadlet turns declarative `.container` and `.pod` files into systemd units at daemon-reload time.

**Name mapping:**
- `ai-stack.pod` -> `ai-stack-pod.service`
- `ollama.container` -> `ollama.service`
- `whisper.container` -> `whisper.service`

**Install location:** `~/.config/containers/systemd/` (rootless user units)

**Key Quadlet directives used:**
- `Pod=ai-stack.pod` — joins the container to the pod
- `AddDevice=/dev/dri` — GPU passthrough
- `GroupAdd=keep-groups` — preserves host group memberships (render, video)
- `Volume=%h/.local/share/ai-models/...` — persistent model storage (`%h` = `$HOME`)
- `HealthCmd=...` — container health checks (shown in `ai-stack.sh status`)

## GPU Detection

`detect-gpu.sh` scans `/sys/class/drm/card*/device/{vendor,device}` for Intel devices (vendor `0x8086`), classifies them via PCI device ID lookup, and recommends a backend:

| Classification | Backend | Rationale |
|---------------|---------|-----------|
| `igpu:*` | ipex | SYCL/IPEX better optimized for integrated graphics |
| `dgpu:arc-a-series` | vulkan | Vulkan 40-100% faster on discrete Arc |
| `dgpu:arc-b-series` | vulkan | Vulkan preferred for Battlemage |
| `dgpu:arc-pro` | ipex | IPEX well-tested on Pro series |

VRAM estimation: iGPU gets `total_ram / 4` capped at 8GB. dGPU sizes are looked up by device ID.

Results are cached to `config/gpu.env` and sourced by other scripts.

## Container Images

### ollama-ipex (SYCL backend)

Built from `intelanalytics/ipex-llm-inference-cpp-xpu:latest`. Uses the IPEX-LLM fork of Ollama with SYCL acceleration.

- Binary location: `/llm/ollama/ollama` (not in `$PATH`)
- Entry: `./ollama serve` (relative to WORKDIR `/llm/ollama`)
- Key env: `SYCL_CACHE_PERSISTENT=1`, `OLLAMA_NUM_GPU=999`

### ollama-vulkan (Vulkan backend)

Built from `ollama/ollama:latest`. Stock Ollama with Vulkan GPU support.

- Binary location: `/usr/bin/ollama` (standard `$PATH`)
- Key env: `OLLAMA_INTEL_GPU=1`

### whisper-sycl

Multi-stage build using Intel oneAPI. Compiles whisper.cpp with SYCL backend.

- **Builder:** `intel/oneapi-basekit:2025.3.1-0-devel-ubuntu22.04`
- **Runtime:** `intel/oneapi-runtime:2025.3.1-0-devel-ubuntu22.04`
- The 2025.3+ runtime image bundles level-zero, OpenCL ICD, and IGC — no extra apt packages needed
- Default model: `base.en` (baked into image, overridable via volume mount at `/models`)

**Intel oneAPI tag format (2025+):** `{major}.{minor}.{patch}-{build}-devel-{os}`
e.g., `2025.3.1-0-devel-ubuntu22.04`. The old short-form (`2025.0-devel-*`) does not exist.

## Key Helper: ollama_cli()

Defined in `scripts/env.sh`. Handles the binary path difference between IPEX and Vulkan images:

```bash
ollama_cli() {
    local flags=()
    while [[ "${1:-}" == -[a-z]* ]]; do flags+=("$1"); shift; done
    podman exec "${flags[@]}" ollama \
        sh -c 'PATH="/llm/ollama:$PATH" exec ollama "$@"' _ "$@"
}
```

Usage:
```bash
ollama_cli list                 # List models
ollama_cli pull llama3.2:3b     # Pull a model
ollama_cli rm llama3.2:3b       # Remove a model
ollama_cli -it run llama3.2:3b  # Interactive chat (-it passed to podman exec)
```

The function prepends `/llm/ollama` to `$PATH` inside the container, so the `ollama` binary is found regardless of which image is running.

## Installer Flow (install.sh)

10-step process with `--skip-build` and `--skip-start` flags:

1. **Prerequisites** — render group, podman, GPU nodes, curl
2. **GPU detection** — `detect-gpu.sh --save` writes `config/gpu.env`
3. **Image builds** — IPEX or Vulkan Ollama, Whisper SYCL, pull Piper + WebUI
4. **Quadlet install** — copy `.pod`/`.container` to `~/.config/containers/systemd/`, rewriting Ollama image for Vulkan if needed
5. **Model directories** — create under `~/.local/share/ai-models/`
6. **systemd reload** — `systemctl --user daemon-reload`, enable linger
7. **Start stack** — `systemctl --user start ai-stack-pod.service`
8. **Starter model** — pull `llama3.2:3b` (or `1b` if VRAM < 6GB)
9. **Smoke tests** — API health checks for Ollama, Whisper, WebUI + LLM generation test
10. **Emacs instructions** — printed to terminal

## Known Gotchas

### Bash arithmetic under set -e

`(( var++ ))` with `var=0` returns exit code 1 (the expression evaluates to 0, which bash treats as false). Use `(( ++var ))` (pre-increment) instead — it evaluates to the new value, which is always >= 1.

### podman exec and PATH

`podman exec <container> <cmd>` does not inherit `WORKDIR` or `ENTRYPOINT` context. The IPEX image's `./ollama` entrypoint works because of WORKDIR, but `podman exec ollama ollama` fails because `/llm/ollama` is not in `$PATH`. The `ollama_cli()` helper solves this.

### Intel oneAPI image tag changes

Intel changed the naming scheme in 2025. Tags like `2025.0-devel-ubuntu22.04` never existed. The correct format includes patch and build numbers: `2025.3.1-0-devel-ubuntu22.04`.

### oneapi-runtime compute stack

The `intel/oneapi-runtime:2025.3+` images pre-install `intel-opencl-icd`, `libze1`, `libze-intel-gpu1`, and `libigc2`. Installing `intel-level-zero-gpu` via apt creates a version conflict (`libigc1` vs `libigc2`).

### SYCL first-run compilation

Whisper's first startup compiles SYCL kernels for the specific GPU, which can take 1-2 minutes. `SYCL_CACHE_PERSISTENT=1` caches the compiled kernels so subsequent starts are fast. The SYCL cache is persisted via volume mount at `~/.local/share/ai-models/sycl-cache`.
