# NVIDIA GPU Support — Design Document

**Date**: 2026-02-14
**Scope**: Full parity with Intel (Ollama, Whisper, detection, Quadlet, prereqs)
**Approach**: Vendor Abstraction Layer — extend existing scripts with vendor-aware branching

## Goals

- Support consumer NVIDIA GPUs (GTX 10-series through RTX 50-series) and datacenter GPUs (Tesla, A100, H100, L40)
- Use NVIDIA Container Toolkit (CDI) for GPU passthrough in rootless Podman
- Auto-detect GPU vendor and select backend: Intel → ipex/vulkan, NVIDIA → cuda
- Maintain full backward compatibility with existing Intel paths

## Architecture

### New Variable: `LLM_ARC_GPU_VENDOR`

All vendor-specific branching keys off `LLM_ARC_GPU_VENDOR` (= `intel` | `nvidia`).

Existing `LLM_ARC_GPU_BACKEND` becomes:
- Intel iGPU → `ipex`
- Intel dGPU → `vulkan`
- NVIDIA → `cuda`

### GPU Detection (`detect-gpu.sh`)

Current: scans `/sys/class/drm/card*` for vendor `0x8086` only.

New behavior:
1. Scan for Intel (0x8086) and NVIDIA (0x10de) GPUs via `/sys/class/drm/`
2. For NVIDIA, get accurate VRAM and GPU name from `nvidia-smi --query-gpu=name,memory.total --format=csv,noheader,nounits`
3. Priority when both vendors present: prefer NVIDIA dGPU over Intel iGPU; prefer NVIDIA dGPU over Intel dGPU
4. New exports: `LLM_ARC_GPU_VENDOR`, all existing `LLM_ARC_GPU_*` vars still populated

NVIDIA classification buckets (for model recommendations):
- Datacenter: Tesla, A100, H100, L40 (by name match)
- Consumer high-end: RTX 3090/4090/5090 etc.
- Consumer mid: RTX 3060-3080, 4060-4080, etc.
- Consumer entry: GTX 1650-1660, RTX 3050, etc.

### Container Images

**Ollama (NVIDIA)**: Use stock `docker.io/ollama/ollama:latest` directly — it ships with CUDA. No custom Containerfile needed. Same as current Vulkan approach.

**Whisper (NVIDIA)**: New `Containerfile.whisper-cuda` based on `nvidia/cuda:12.6.3-runtime-ubuntu22.04`. Builds whisper.cpp with `-DGGML_CUDA=ON`.

**Piper**: No change (CPU only).
**Open WebUI**: No change (web app).

### Environment (`env.sh`)

New variables:
```bash
LLM_ARC_OLLAMA_CUDA_IMAGE="docker.io/ollama/ollama:latest"
LLM_ARC_WHISPER_CUDA_IMAGE="localhost/whisper-cuda:latest"

LLM_ARC_NVIDIA_ENV_VARS=(
    "NVIDIA_VISIBLE_DEVICES=all"
    "NVIDIA_DRIVER_CAPABILITIES=compute,utility"
    "OLLAMA_HOST=0.0.0.0"
    "OLLAMA_ORIGINS=*"
    "OLLAMA_NUM_GPU=999"
)
```

### Quadlet Units

Installer rewrites `ollama.container` and `whisper.container` based on vendor:

**NVIDIA ollama.container changes:**
- `Image=docker.io/ollama/ollama:latest`
- `AddDevice=nvidia.com/gpu=all` (CDI, replaces `/dev/dri`)
- Remove: `GroupAdd=keep-groups`, Intel SYCL env vars
- Add: NVIDIA env vars

**NVIDIA whisper.container changes:**
- `Image=localhost/whisper-cuda:latest`
- `AddDevice=nvidia.com/gpu=all`
- Remove: `GroupAdd=keep-groups`, Intel SYCL env vars
- Add: `NVIDIA_VISIBLE_DEVICES=all`

### Prerequisite Checks (`00-setup-host.sh`)

Vendor-specific checks:
- **Intel**: render/video groups, `/dev/dri/renderD*` (existing)
- **NVIDIA**: `nvidia-smi` works, `nvidia-ctk` installed, CDI spec generated
- **Common**: kernel >= 6.2, Podman >= 4.4, curl, systemctl

### Installer (`install.sh`)

Step 2: "Detecting GPU" (not "Detecting Intel GPU")
Step 3: NVIDIA → skip ollama image build (stock), build `whisper-cuda` instead of `whisper-sycl`
Step 4: Vendor-aware Quadlet rewriting (extend existing sed logic)

### README

- GPU Support table: add NVIDIA rows
- Requirements: add nvidia-container-toolkit
- Troubleshooting: NVIDIA-specific entries

## Files Changed

| File | Change |
|------|--------|
| `scripts/detect-gpu.sh` | Multi-vendor detection, NVIDIA classification |
| `scripts/env.sh` | NVIDIA image names, env vars, vendor default |
| `scripts/00-setup-host.sh` | NVIDIA prereq checks |
| `install.sh` | Vendor-aware build and Quadlet steps |
| `quadlet/ollama.container` | (rewritten at install time, no template change needed) |
| `quadlet/whisper.container` | (rewritten at install time) |
| `README.md` | NVIDIA documentation |

## Files Created

| File | Purpose |
|------|---------|
| `containers/Containerfile.whisper-cuda` | Whisper with CUDA backend |

## Not Changed

- `quadlet/ai-stack.pod` — vendor-agnostic (just port bindings)
- `quadlet/piper.container` — CPU only
- `quadlet/openwebui.container` — web app, no GPU
- `scripts/models.sh` — already VRAM-based, vendor-agnostic
- `scripts/ai-stack.sh` — lifecycle commands are service-name based, vendor-agnostic
- `emacs/` — talks to Ollama API, doesn't care about GPU vendor
