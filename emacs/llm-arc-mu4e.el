;;; llm-arc-mu4e.el --- mu4e email LLM helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; LLM-powered email actions for mu4e.
;; Integrates with multi-context mu4e setups (auto-detects current context).
;;
;; Doom Emacs:  , l prefix in mu4e view/compose buffers (localleader)
;; Vanilla:     C-c l prefix in mu4e buffers
;;
;; Commands:
;;   llm-arc-mu4e-summarize       — Summarize email in bullets
;;   llm-arc-mu4e-generate-reply  — Generate reply draft
;;   llm-arc-mu4e-translate       — Translate email
;;   llm-arc-mu4e-draft-reply     — Draft reply from quoted original (compose)
;;   llm-arc-mu4e-improve-grammar — Improve grammar of draft (compose)

;;; Code:

(require 'gptel)

(defgroup llm-arc-mu4e nil
  "llm-arc mu4e email integration."
  :group 'llm-arc
  :prefix "llm-arc-mu4e-")

(defcustom llm-arc-mu4e-summary-system-prompt
  "You are an email assistant. Summarize the following email in 2-3 concise bullet points. Focus on action items and key information."
  "System prompt for email summarization."
  :type 'string)

(defcustom llm-arc-mu4e-reply-system-prompt
  "You are drafting an email reply. Write a professional, concise reply to the email below. Match the tone of the original. Do not include a subject line."
  "System prompt for reply generation."
  :type 'string)

(defcustom llm-arc-mu4e-grammar-system-prompt
  "Improve the grammar, clarity, and professionalism of the following email draft. Preserve the meaning and tone. Return only the improved text, no explanations."
  "System prompt for grammar improvement."
  :type 'string)

(defcustom llm-arc-mu4e-translate-language "English"
  "Target language for email translation."
  :type 'string)

;; ── Helpers ─────────────────────────────────────────────────────

(defun llm-arc-mu4e--get-email-body ()
  "Get the body text of the currently viewed email."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^$" nil t)
        (buffer-substring-no-properties (point) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun llm-arc-mu4e--get-compose-body ()
  "Get the body text of the compose buffer (after mail headers).
Handles both message-mode (Doom/mu4e) and mml-mode separators."
  (save-excursion
    (goto-char (point-min))
    ;; Doom mu4e uses message-mode which has this separator
    (cond
     ((re-search-forward "^--text follows this line--$" nil t)
      (buffer-substring-no-properties (1+ (point)) (point-max)))
     ;; Some setups use a blank line after headers
     ((progn (goto-char (point-min))
             (re-search-forward "^$" nil t))
      (buffer-substring-no-properties (point) (point-max)))
     (t (buffer-string)))))

(defun llm-arc-mu4e--gptel-request (prompt system-prompt callback)
  "Send PROMPT with SYSTEM-PROMPT to LLM, call CALLBACK with response text."
  (gptel-request prompt
    :system system-prompt
    :callback (lambda (response info)
                (if response
                    (funcall callback response)
                  (message "LLM request failed: %s"
                           (plist-get info :error))))))

;; ── Commands: View mode ─────────────────────────────────────────

(defun llm-arc-mu4e-summarize ()
  "Summarize the current email in 2-3 bullet points."
  (interactive)
  (let ((body (llm-arc-mu4e--get-email-body)))
    (llm-arc-mu4e--gptel-request
     body
     llm-arc-mu4e-summary-system-prompt
     (lambda (summary)
       (with-current-buffer (get-buffer-create "*Email Summary*")
         (erase-buffer)
         (insert summary)
         (goto-char (point-min))
         (display-buffer (current-buffer)))
       (message "Email summarized")))))

(defun llm-arc-mu4e-generate-reply ()
  "Generate a reply draft for the current email."
  (interactive)
  (let ((body (llm-arc-mu4e--get-email-body)))
    (llm-arc-mu4e--gptel-request
     body
     llm-arc-mu4e-reply-system-prompt
     (lambda (reply)
       (with-current-buffer (get-buffer-create "*Email Reply Draft*")
         (erase-buffer)
         (insert reply)
         (goto-char (point-min))
         (display-buffer (current-buffer)))
       (message "Reply draft generated")))))

(defun llm-arc-mu4e-translate ()
  "Translate the current email."
  (interactive)
  (let ((body (llm-arc-mu4e--get-email-body))
        (sys-prompt (format "Translate the following email to %s. Return only the translation."
                            llm-arc-mu4e-translate-language)))
    (llm-arc-mu4e--gptel-request
     body sys-prompt
     (lambda (translation)
       (with-current-buffer (get-buffer-create "*Email Translation*")
         (erase-buffer)
         (insert translation)
         (goto-char (point-min))
         (display-buffer (current-buffer)))
       (message "Email translated to %s" llm-arc-mu4e-translate-language)))))

;; ── Commands: Compose mode ──────────────────────────────────────

(defun llm-arc-mu4e-draft-reply ()
  "In compose buffer: use LLM to draft a reply based on the quoted original."
  (interactive)
  (let ((body (llm-arc-mu4e--get-compose-body)))
    (llm-arc-mu4e--gptel-request
     (concat "Original email (quoted below). Write a reply:\n\n" body)
     llm-arc-mu4e-reply-system-prompt
     (lambda (reply)
       (save-excursion
         (goto-char (point-min))
         (when (re-search-forward "^--text follows this line--$" nil t)
           (forward-line 1))
         (insert reply "\n"))
       (message "Reply drafted")))))

(defun llm-arc-mu4e-improve-grammar ()
  "Improve grammar of the current compose draft."
  (interactive)
  (let ((body (llm-arc-mu4e--get-compose-body)))
    (llm-arc-mu4e--gptel-request
     body
     llm-arc-mu4e-grammar-system-prompt
     (lambda (improved)
       (save-excursion
         (goto-char (point-min))
         (let ((body-start
                (if (re-search-forward "^--text follows this line--$" nil t)
                    (1+ (point))
                  (progn (goto-char (point-min))
                         (re-search-forward "^$" nil t)
                         (point)))))
           (delete-region body-start (point-max))
           (goto-char body-start)
           (insert improved)))
       (message "Grammar improved")))))

;; ── Keybindings ─────────────────────────────────────────────────

(if (boundp 'doom-version)
    ;; Doom: localleader (, l) in mu4e view and compose buffers
    ;; This extends your existing mu4e-config.el localleader bindings
    (progn
      (after! mu4e
        (map! :map mu4e-view-mode-map
              :localleader
              (:prefix-map ("l" . "llm")
               :desc "Summarize"  "s" #'llm-arc-mu4e-summarize
               :desc "Reply"      "r" #'llm-arc-mu4e-generate-reply
               :desc "Translate"  "t" #'llm-arc-mu4e-translate))

        (map! :map mu4e-compose-mode-map
              :localleader
              (:prefix-map ("l" . "llm")
               :desc "Draft reply" "d" #'llm-arc-mu4e-draft-reply
               :desc "Grammar"     "g" #'llm-arc-mu4e-improve-grammar
               :desc "Translate"   "t" #'llm-arc-mu4e-translate))))

  ;; Vanilla: C-c l prefix in mu4e modes
  (with-eval-after-load 'mu4e
    (let ((view-map (make-sparse-keymap))
          (compose-map (make-sparse-keymap)))
      (define-key view-map (kbd "s") #'llm-arc-mu4e-summarize)
      (define-key view-map (kbd "r") #'llm-arc-mu4e-generate-reply)
      (define-key view-map (kbd "t") #'llm-arc-mu4e-translate)
      (define-key compose-map (kbd "d") #'llm-arc-mu4e-draft-reply)
      (define-key compose-map (kbd "g") #'llm-arc-mu4e-improve-grammar)
      (define-key compose-map (kbd "t") #'llm-arc-mu4e-translate)
      (define-key mu4e-view-mode-map (kbd "C-c l") view-map)
      (define-key mu4e-compose-mode-map (kbd "C-c l") compose-map))))

(provide 'llm-arc-mu4e)
;;; llm-arc-mu4e.el ends here
