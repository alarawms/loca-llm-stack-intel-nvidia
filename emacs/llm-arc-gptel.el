;;; llm-arc-gptel.el --- gptel configuration for local Ollama LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Configures gptel to use the local Ollama instance (containerized,
;; Intel Arc GPU accelerated).  Registers the Ollama backend, sets
;; default model, and provides keybindings.
;;
;; Doom Emacs:  SPC l prefix (chat, send, menu, model switching)
;; Vanilla:     C-c l prefix (same commands)
;;
;; Requires: gptel (MELPA)

;;; Code:

(require 'gptel)

(defgroup llm-arc-gptel nil
  "llm-arc gptel configuration."
  :group 'llm-arc
  :prefix "llm-arc-gptel-")

(defcustom llm-arc-gptel-host "localhost:11434"
  "Ollama server host:port."
  :type 'string)

(defcustom llm-arc-gptel-default-model "llama3.2:3b"
  "Default model for chat.  Should match a model pulled via models.sh."
  :type 'string)

(defcustom llm-arc-gptel-code-model "qwen2.5-coder:3b"
  "Model for code-related tasks."
  :type 'string)

;; ── Register Ollama backend ─────────────────────────────────────

(defvar llm-arc-gptel-backend
  (gptel-make-ollama "Ollama-Local"
    :host llm-arc-gptel-host
    :stream t
    :models '(llama3.2:3b
              llama3.2:1b
              llama3.1:8b
              phi3:3.8b
              mistral:7b
              qwen2.5-coder:3b
              qwen2.5-coder:7b
              gemma2:9b))
  "The gptel backend for local Ollama.")

(setq gptel-backend llm-arc-gptel-backend
      gptel-model llm-arc-gptel-default-model)

;; ── Model switching helpers ─────────────────────────────────────

(defun llm-arc-gptel-use-code-model ()
  "Switch gptel to the code-specialized model."
  (interactive)
  (setq gptel-model llm-arc-gptel-code-model)
  (message "Switched to code model: %s" llm-arc-gptel-code-model))

(defun llm-arc-gptel-use-chat-model ()
  "Switch gptel back to the default chat model."
  (interactive)
  (setq gptel-model llm-arc-gptel-default-model)
  (message "Switched to chat model: %s" llm-arc-gptel-default-model))

;; ── Server health check ─────────────────────────────────────────

(defun llm-arc-gptel-check-server ()
  "Check if the local Ollama server is responding."
  (interactive)
  (let ((url (format "http://%s/api/tags" llm-arc-gptel-host)))
    (url-retrieve url
                  (lambda (status)
                    (if (plist-get status :error)
                        (message "Ollama NOT responding at %s" llm-arc-gptel-host)
                      (message "Ollama OK at %s" llm-arc-gptel-host))
                    (kill-buffer (current-buffer)))
                  nil t)))

;; ── Keybindings ─────────────────────────────────────────────────

(if (boundp 'doom-version)
    ;; Doom Emacs: SPC l prefix
    (map! :leader
          (:prefix-map ("l" . "llm")
           :desc "Chat"            "l" #'gptel
           :desc "Send"            "s" #'gptel-send
           :desc "Menu"            "m" #'gptel-menu
           :desc "Rewrite"         "r" #'gptel-rewrite
           :desc "Abort"           "x" #'gptel-abort
           :desc "Code model"      "c" #'llm-arc-gptel-use-code-model
           :desc "Chat model"      "C" #'llm-arc-gptel-use-chat-model
           :desc "Check server"    "?" #'llm-arc-gptel-check-server))

  ;; Vanilla Emacs: C-c l prefix
  (defvar llm-arc-gptel-map (make-sparse-keymap) "llm-arc gptel keymap.")
  (define-key llm-arc-gptel-map (kbd "l") #'gptel)
  (define-key llm-arc-gptel-map (kbd "s") #'gptel-send)
  (define-key llm-arc-gptel-map (kbd "m") #'gptel-menu)
  (define-key llm-arc-gptel-map (kbd "r") #'gptel-rewrite)
  (define-key llm-arc-gptel-map (kbd "x") #'gptel-abort)
  (define-key llm-arc-gptel-map (kbd "c") #'llm-arc-gptel-use-code-model)
  (define-key llm-arc-gptel-map (kbd "C") #'llm-arc-gptel-use-chat-model)
  (define-key llm-arc-gptel-map (kbd "?") #'llm-arc-gptel-check-server)
  (global-set-key (kbd "C-c l") llm-arc-gptel-map))

(provide 'llm-arc-gptel)
;;; llm-arc-gptel.el ends here
