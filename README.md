# llm-arc

Local AI stack for Intel Arc and NVIDIA GPUs, managed with Podman Quadlet and integrated with Emacs.

Runs **Ollama** (LLM), **Whisper** (speech-to-text), **Piper** (text-to-speech), **Open WebUI**, and **SearXNG** (web search) in a single rootless pod with GPU passthrough.

## Quick Start

```bash
# 1. Verify host prerequisites (kernel, groups, GPU drivers, tools)
./scripts/00-setup-host.sh

# 2. Install and start everything
./install.sh
```

The installer will:
1. Verify host prerequisites (kernel, groups, GPU nodes, NVIDIA toolkit)
2. Auto-detect GPU and select the optimal backend (IPEX / Vulkan / CUDA)
3. Build container images (IPEX-LLM or Vulkan for Ollama, SYCL/CUDA for Whisper)
4. Install Quadlet systemd units for boot persistence
5. Start all services and pull a starter model
6. Run smoke tests (Ollama, Whisper, SearXNG, Open WebUI)

## Requirements

Run `./scripts/00-setup-host.sh` to check all of these before installing.

**All systems**

| Requirement | Min version | Install |
|-------------|-------------|---------|
| Linux kernel | >= 6.2 | `sudo dnf upgrade kernel` |
| Podman | >= 4.4 | `sudo dnf install podman` |
| `curl` | any | `sudo dnf install curl` |
| `jq` | any | `sudo dnf install jq` *(optional — status/benchmark commands)* |

**Intel GPU (iGPU or Arc)**

| Requirement | Notes |
|-------------|-------|
| User in `render` group | `sudo usermod -aG render $USER` — needed to open `/dev/dri/renderD*` |
| User in `video` group | `sudo usermod -aG video $USER` — needed for Vulkan / display access |
| `/dev/dri/renderD*` present | Confirms GPU driver is loaded |

**NVIDIA GPU**

| Requirement | Notes |
|-------------|-------|
| NVIDIA driver + `nvidia-smi` | `sudo dnf install akmod-nvidia` (RPM Fusion) |
| `nvidia-container-toolkit` | `sudo dnf install nvidia-container-toolkit` — enables GPU passthrough in Podman |
| CDI spec at `/etc/cdi/nvidia.yaml` | `sudo nvidia-ctk cdi generate --output=/etc/cdi/nvidia.yaml` — regenerate after each driver upgrade |

> **Both Intel and NVIDIA present?** The installer prefers NVIDIA. Override with `LLM_ARC_GPU_VENDOR=intel`.

## GPU Support

| GPU Family | Type | Backend | Performance |
|------------|------|---------|-------------|
| Arrow Lake, Meteor Lake, Lunar Lake | iGPU | IPEX-LLM (SYCL) | Good |
| Alder Lake, Tiger Lake | iGPU | IPEX-LLM (SYCL) | Fair |
| Arc A-series (Alchemist) | dGPU | Vulkan | Best |
| Arc B-series (Battlemage) | dGPU | Vulkan | Best |
| Arc Pro | dGPU | IPEX-LLM (SYCL) | Good |
| GeForce GTX 10/16-series | dGPU | CUDA | Good |
| GeForce RTX 20/30-series | dGPU | CUDA | Great |
| GeForce RTX 40/50-series | dGPU | CUDA | Best |
| Tesla, A100, H100, L40 | dGPU | CUDA | Best |

Backend is auto-selected by `scripts/detect-gpu.sh`. Override with `LLM_ARC_GPU_BACKEND=vulkan` (Intel) or `LLM_ARC_GPU_BACKEND=cuda` (NVIDIA).

## Services

| Service | Port | Description |
|---------|------|-------------|
| Ollama | `:11434` | LLM inference (GPU-accelerated) |
| Whisper | `:9100` | Speech-to-text (GPU-accelerated) |
| Piper | `:5002` | Text-to-speech (CPU, Wyoming protocol) |
| Open WebUI | `:8080` | Web chat interface |
| SearXNG | `:8888` | Metasearch engine (web search for AI RAG) |

Services run in a shared Podman pod (`ai-stack`). Ports bind to all interfaces (`0.0.0.0`) for Tailscale access. Use firewall rules (firewalld/nftables) to restrict LAN access if needed.

## Management

```bash
./scripts/ai-stack.sh status       # Service health
./scripts/ai-stack.sh logs         # Pod logs (or: logs ollama)
./scripts/ai-stack.sh stop         # Stop everything
./scripts/ai-stack.sh start        # Start everything
./scripts/ai-stack.sh restart      # Restart all services
./scripts/ai-stack.sh benchmark    # LLM speed test
./scripts/ai-stack.sh uninstall    # Remove Quadlet units (keeps models)
```

## Model Management

```bash
./scripts/models.sh list           # Installed models
./scripts/models.sh recommend      # Suggestions based on your GPU VRAM
./scripts/models.sh pull <model>   # Download a model
./scripts/models.sh remove <model> # Delete a model
./scripts/models.sh test [model]   # Quick generation test
./scripts/models.sh chat <model>   # Interactive chat
```

## Emacs Integration

### Doom Emacs

Add to `packages.el`:
```elisp
(package! gptel)
(package! ellama)
```

Add to `config.el`:
```elisp
(add-to-list 'load-path "~/dev/llm-arc/emacs")
(load! "llm-arc" "~/dev/llm-arc/emacs")
```

Then run `doom sync`.

### Vanilla Emacs

```elisp
(add-to-list 'load-path "~/dev/llm-arc/emacs")
(require 'llm-arc)
```

### Key Bindings (Doom)

| Binding | Action |
|---------|--------|
| `SPC l l` | Open gptel chat buffer |
| `SPC l s` | Send region/buffer to LLM |
| `SPC l m` | gptel menu (model, params) |
| `SPC l r` | Rewrite region |
| `SPC l c` | Switch to code model |
| `SPC l e` | Ellama actions (s=summarize, g=grammar, t=translate) |
| `SPC l w` | Toggle dictation |
| `SPC l o` | Org LLM (d=daily plan, c=smart capture, s=summary) |
| `, l` | mu4e LLM actions (in email buffers) |
| `SPC TAB a` | Switch to AI workspace |

## Project Structure

```
llm-arc/
  install.sh                        # One-command installer
  scripts/
    env.sh                          # Shared variables and helpers
    detect-gpu.sh                   # Intel GPU auto-detection
    ai-stack.sh                     # Service lifecycle management
    models.sh                       # Model management CLI
    00-setup-host.sh                # Host prerequisite checker
    dictate.sh                      # Dictation helper
  containers/
    Containerfile.ollama-ipex       # Ollama + IPEX-LLM (SYCL, for iGPU)
    Containerfile.ollama-vulkan     # Ollama + Vulkan (for discrete Arc)
    Containerfile.whisper-sycl      # whisper.cpp with SYCL backend
    Containerfile.whisper-cuda      # Whisper with CUDA backend (for NVIDIA)
  quadlet/
    ai-stack.pod                    # Pod definition (shared network)
    ollama.container                # Ollama Quadlet unit
    whisper.container               # Whisper Quadlet unit
    piper.container                 # Piper TTS Quadlet unit
    searxng.container               # SearXNG Quadlet unit
    openwebui.container             # Open WebUI Quadlet unit
  emacs/
    llm-arc.el                      # Master Emacs config
    llm-arc-gptel.el                # gptel (chat/completion) config
    llm-arc-ellama.el               # Ellama integration
    llm-arc-whisper.el              # Dictation via Whisper
    llm-arc-org.el                  # Org-mode LLM features
    llm-arc-mu4e.el                 # mu4e email LLM actions
  config/
    gpu.env                         # Cached GPU detection results
    searxng/
      settings.yml                  # SearXNG config (limiter off, JSON enabled)
```

## Data Locations

| Path | Contents |
|------|----------|
| `~/.local/share/ai-models/ollama` | Ollama model weights |
| `~/.local/share/ai-models/whisper` | Whisper models |
| `~/.local/share/ai-models/piper` | Piper voice models |
| `~/.local/share/ai-models/sycl-cache` | SYCL kernel compilation cache |
| `~/.local/share/ai-models/openwebui` | Open WebUI chat history & settings |
| `~/.local/share/ai-models/searxng` | SearXNG settings.yml (edit to add engines) |
| `~/.config/containers/systemd/` | Installed Quadlet unit files |

## Troubleshooting

**Intel GPU not detected**: Ensure GPU drivers are loaded and `/dev/dri/renderD*` exists. Run `./scripts/00-setup-host.sh` — it will pinpoint the issue.

**Permission denied on Intel GPU**: Group membership may not be active yet in the current shell:
```bash
sudo usermod -aG render,video $USER
newgrp render   # activate immediately, or log out and back in
```

**NVIDIA GPU not detected**: Confirm `nvidia-smi` works. If not, install the driver:
```bash
sudo dnf install akmod-nvidia   # RPM Fusion required
```

**NVIDIA container fails (no GPU visible)**: The CDI spec may be missing or stale. Regenerate it after every driver upgrade:
```bash
sudo nvidia-ctk cdi generate --output=/etc/cdi/nvidia.yaml
```

**Whisper slow on first start**: SYCL compiles GPU kernels on first run (~1-2 min). `SYCL_CACHE_PERSISTENT=1` (already set) caches the result so subsequent starts are instant.

**Ollama out of memory**: Use a smaller quantized model. Run `./scripts/models.sh recommend` for VRAM-appropriate suggestions.
