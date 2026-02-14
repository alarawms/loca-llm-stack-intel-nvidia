;;; llm-arc.el --- Local LLM integration for Emacs (Intel Arc GPU) -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: llm-arc
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9"))
;; Keywords: ai, llm, tools

;;; Commentary:
;;
;; Master configuration for the llm-arc Emacs integration.
;; Works with both Doom Emacs and vanilla Emacs.
;;
;; Doom Emacs (recommended):
;;   1. Add to packages.el:
;;        (package! gptel)
;;        (package! ellama)  ; pulls in `llm' + `llm-ollama' automatically
;;   2. Add to config.el:
;;        (load! "lisp/llm-arc" doom-user-dir)
;;      Or symlink: ln -s ~/dev/llm-arc/emacs ~/path-to-doom/lisp/llm-arc-emacs
;;      and: (add-to-list 'load-path (concat doom-user-dir "lisp/llm-arc-emacs"))
;;           (load! "llm-arc" (concat doom-user-dir "lisp/llm-arc-emacs"))
;;
;; Vanilla Emacs:
;;   (add-to-list 'load-path "~/dev/llm-arc/emacs")
;;   (require 'llm-arc)

;;; Code:

(defgroup llm-arc nil
  "Local LLM integration with Intel Arc GPU acceleration."
  :group 'tools
  :prefix "llm-arc-")

;; ── Doom detection ──────────────────────────────────────────────

(defvar llm-arc--doom-p (boundp 'doom-version)
  "Non-nil if running inside Doom Emacs.")

;; ── Load submodules ─────────────────────────────────────────────

;; Core: gptel (chat + code)
(if llm-arc--doom-p
    (load! "llm-arc-gptel" (file-name-directory load-file-name))
  (require 'llm-arc-gptel))

;; Core: ellama (task actions)
(if llm-arc--doom-p
    (load! "llm-arc-ellama" (file-name-directory load-file-name))
  (require 'llm-arc-ellama))

;; Whisper dictation — needs only json (built-in)
(if llm-arc--doom-p
    (load! "llm-arc-whisper" (file-name-directory load-file-name))
  (require 'llm-arc-whisper))

;; mu4e email helpers — only if mu4e is available
(when (or (featurep 'mu4e) (locate-library "mu4e"))
  (if llm-arc--doom-p
      (load! "llm-arc-mu4e" (file-name-directory load-file-name))
    (require 'llm-arc-mu4e nil t)))

;; Org-mode helpers — org is always available
(if llm-arc--doom-p
    (load! "llm-arc-org" (file-name-directory load-file-name))
  (require 'llm-arc-org nil t))

;; ── Doom: AI workspace ──────────────────────────────────────────

(when llm-arc--doom-p
  (after! persp-mode
    (defun +workspace/switch-to-ai ()
      "Switch to AI workspace and open gptel chat."
      (interactive)
      (+workspace/switch-to "AI")
      (delete-other-windows)
      (gptel "AI Chat"))

    (map! :leader
          (:prefix "TAB"
           :desc "AI" "a" #'+workspace/switch-to-ai))))

(provide 'llm-arc)
;;; llm-arc.el ends here
