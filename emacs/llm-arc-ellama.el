;;; llm-arc-ellama.el --- ellama task-oriented actions for local LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Configures ellama for task-oriented LLM actions (summarize, improve
;; grammar, translate, code review, etc.) using the local Ollama.
;;
;; Doom Emacs:  SPC l e prefix (ellama actions)
;; Vanilla:     C-c l e prefix
;;
;; Requires: ellama (MELPA) — llm-ollama is bundled inside the `llm' package,
;;   which ellama pulls in as a dependency.

;;; Code:

(require 'ellama)
(require 'llm-ollama)  ; provided by the `llm' package (dep of ellama)

(defgroup llm-arc-ellama nil
  "llm-arc ellama configuration."
  :group 'llm-arc
  :prefix "llm-arc-ellama-")

(defcustom llm-arc-ellama-host "localhost"
  "Ollama server hostname."
  :type 'string)

(defcustom llm-arc-ellama-port 11434
  "Ollama server port."
  :type 'integer)

;; ── Providers ───────────────────────────────────────────────────

(defvar llm-arc-ellama-chat-provider
  (make-llm-ollama
   :host llm-arc-ellama-host
   :port llm-arc-ellama-port
   :chat-model "llama3.2:3b"
   :embedding-model "llama3.2:3b")
  "Ollama provider for general chat/text tasks.")

(defvar llm-arc-ellama-code-provider
  (make-llm-ollama
   :host llm-arc-ellama-host
   :port llm-arc-ellama-port
   :chat-model "qwen2.5-coder:3b"
   :embedding-model "qwen2.5-coder:3b")
  "Ollama provider for code-related tasks.")

(setq ellama-provider llm-arc-ellama-chat-provider
      ellama-coding-provider llm-arc-ellama-code-provider)

(setq ellama-providers
      '(("chat" . llm-arc-ellama-chat-provider)
        ("code" . llm-arc-ellama-code-provider)))

;; ── Keybindings ─────────────────────────────────────────────────

(if (boundp 'doom-version)
    ;; Doom: SPC l e prefix (nested under SPC l)
    (map! :leader
          (:prefix "l"
           (:prefix-map ("e" . "ellama")
            :desc "Summarize"      "s" #'ellama-summarize
            :desc "Grammar"        "g" #'ellama-improve-grammar
            :desc "Wording"        "w" #'ellama-improve-wording
            :desc "Translate"      "t" #'ellama-translate
            :desc "Code review"    "r" #'ellama-code-review
            :desc "Code complete"  "c" #'ellama-code-complete
            :desc "Code add"       "a" #'ellama-code-add
            :desc "Code edit"      "e" #'ellama-code-edit
            :desc "Define word"    "d" #'ellama-define-word)))

  ;; Vanilla: C-c l e prefix
  (defvar llm-arc-ellama-map (make-sparse-keymap) "llm-arc ellama keymap.")
  (define-key llm-arc-ellama-map (kbd "s") #'ellama-summarize)
  (define-key llm-arc-ellama-map (kbd "g") #'ellama-improve-grammar)
  (define-key llm-arc-ellama-map (kbd "w") #'ellama-improve-wording)
  (define-key llm-arc-ellama-map (kbd "t") #'ellama-translate)
  (define-key llm-arc-ellama-map (kbd "r") #'ellama-code-review)
  (define-key llm-arc-ellama-map (kbd "c") #'ellama-code-complete)
  (define-key llm-arc-ellama-map (kbd "a") #'ellama-code-add)
  (define-key llm-arc-ellama-map (kbd "e") #'ellama-code-edit)
  (define-key llm-arc-ellama-map (kbd "d") #'ellama-define-word)
  ;; Nest under the C-c l prefix set by llm-arc-gptel
  (when (boundp 'llm-arc-gptel-map)
    (define-key llm-arc-gptel-map (kbd "e") llm-arc-ellama-map)))

(provide 'llm-arc-ellama)
;;; llm-arc-ellama.el ends here
