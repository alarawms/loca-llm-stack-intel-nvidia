# llm-arc

Local AI stack for Intel Arc GPUs, managed with Podman Quadlet and integrated with Emacs.

Runs **Ollama** (LLM), **Whisper** (speech-to-text), **Piper** (text-to-speech), and **Open WebUI** in a single rootless pod with GPU passthrough.

## Quick Start

```bash
# Check prerequisites
./scripts/00-setup-host.sh

# Install and start everything
./install.sh
```

The installer will:
1. Verify host prerequisites (kernel, groups, GPU nodes)
2. Auto-detect your Intel GPU and select the optimal backend
3. Build container images (IPEX-LLM or Vulkan for Ollama, SYCL for Whisper)
4. Install Quadlet systemd units for boot persistence
5. Start all services and pull a starter model
6. Run smoke tests

## Requirements

- **Linux** with kernel >= 6.2
- **Intel GPU** (iGPU or discrete Arc A/B-series)
- **Podman** >= 4.4 (for Quadlet support)
- User in `render` and `video` groups
- `curl`, `jq` (optional but recommended)

## GPU Support

| GPU Family | Type | Backend | Performance |
|------------|------|---------|-------------|
| Arrow Lake, Meteor Lake, Lunar Lake | iGPU | IPEX-LLM (SYCL) | Good |
| Alder Lake, Tiger Lake | iGPU | IPEX-LLM (SYCL) | Fair |
| Arc A-series (Alchemist) | dGPU | Vulkan | Best |
| Arc B-series (Battlemage) | dGPU | Vulkan | Best |
| Arc Pro | dGPU | IPEX-LLM (SYCL) | Good |

Backend is auto-selected by `scripts/detect-gpu.sh`. Override with `LLM_ARC_GPU_BACKEND=vulkan`.

## Services

| Service | Port | Description |
|---------|------|-------------|
| Ollama | `127.0.0.1:11434` | LLM inference (GPU-accelerated) |
| Whisper | `127.0.0.1:9000` | Speech-to-text (SYCL on GPU) |
| Piper | `127.0.0.1:5002` | Text-to-speech (CPU, Wyoming protocol) |
| Open WebUI | `127.0.0.1:8080` | Web chat interface |

All ports are localhost-only. Services run in a shared Podman pod (`ai-stack`).

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
  quadlet/
    ai-stack.pod                    # Pod definition (shared network)
    ollama.container                # Ollama Quadlet unit
    whisper.container               # Whisper Quadlet unit
    piper.container                 # Piper TTS Quadlet unit
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
```

## Data Locations

| Path | Contents |
|------|----------|
| `~/.local/share/ai-models/ollama` | Ollama model weights |
| `~/.local/share/ai-models/whisper` | Whisper models |
| `~/.local/share/ai-models/piper` | Piper voice models |
| `~/.local/share/ai-models/sycl-cache` | SYCL kernel compilation cache |
| `~/.local/share/ai-models/openwebui` | Open WebUI chat history & settings |
| `~/.config/containers/systemd/` | Installed Quadlet unit files |

## Troubleshooting

**GPU not detected**: Ensure Intel GPU drivers are installed and `/dev/dri/renderD*` exists.

**Permission denied on GPU**: Add user to render/video groups:
```bash
sudo usermod -aG render,video $USER
# Then log out and back in
```

**Whisper slow on first start**: SYCL compiles GPU kernels on first run. Set `SYCL_CACHE_PERSISTENT=1` (already configured) so subsequent starts are fast.

**Ollama out of memory**: Use a smaller quantized model. Run `./scripts/models.sh recommend` for VRAM-appropriate suggestions.
