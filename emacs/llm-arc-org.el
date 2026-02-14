;;; llm-arc-org.el --- Org-mode LLM automation -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; LLM-powered org-mode helpers that integrate with GTD workflows.
;;
;; Doom Emacs:  SPC l o prefix (org LLM actions)
;; Vanilla:     M-x llm-arc-daily-plan, etc.
;;
;; Designed to work with org-super-agenda and the GTD keyword set:
;;   TODO NEXT WAITING HOLD | DONE CANCELLED

;;; Code:

(require 'gptel)
(require 'org)

(defgroup llm-arc-org nil
  "llm-arc org-mode integration."
  :group 'llm-arc
  :prefix "llm-arc-org-")

(defcustom llm-arc-org-daily-plan-system-prompt
  "You are a productivity assistant. Given a list of TODO items from org-mode, create a prioritized daily plan. Group by priority, suggest an order, and flag any items that seem overdue. Use plain text, not org-mode syntax."
  "System prompt for daily plan generation."
  :type 'string)

(defcustom llm-arc-org-capture-system-prompt
  "Convert the following freeform text into a structured org-mode TODO entry. Include:
- A clear headline (one line, starting with TODO)
- SCHEDULED or DEADLINE if dates are mentioned
- Tags if topics are apparent (use @work, @home, @errands for context tags)
- A brief body if needed

Return ONLY the org-mode entry, nothing else. Example format:
* TODO Headline :@work:
DEADLINE: <2025-01-15>
Brief notes here."
  "System prompt for smart capture.
The tag suggestions match the GTD tag alist in config.el."
  :type 'string)

(defcustom llm-arc-org-summary-system-prompt
  "Summarize the project status based on these org-mode TODO items. Provide:
- Total tasks and completion percentage
- Key items in progress (NEXT state)
- Blocked or waiting items (WAITING state)
- Overdue items
- Brief overall assessment
Keep it concise (5-8 lines)."
  "System prompt for project summary."
  :type 'string)

;; ── Helpers ─────────────────────────────────────────────────────

(defun llm-arc-org--collect-agenda-items ()
  "Collect today's agenda items as a string.
Respects the GTD keyword set: TODO, NEXT, WAITING, HOLD, MEET."
  (require 'org-agenda)
  (let ((items '()))
    (org-map-entries
     (lambda ()
       (let* ((heading (org-get-heading t t t t))
              (state (org-get-todo-state))
              (tags (org-get-tags))
              (deadline (org-get-deadline-time (point)))
              (scheduled (org-get-scheduled-time (point))))
         (push (format "%s %s%s%s%s"
                       (or state "")
                       heading
                       (if tags (format " :%s:" (string-join tags ":")) "")
                       (if deadline (format " DEADLINE: %s"
                                            (format-time-string "<%Y-%m-%d>" deadline))
                         "")
                       (if scheduled (format " SCHEDULED: %s"
                                             (format-time-string "<%Y-%m-%d>" scheduled))
                         ""))
               items)))
     "/TODO|NEXT|WAITING|HOLD|MEET"
     'agenda)
    (string-join (nreverse items) "\n")))

(defun llm-arc-org--collect-project-items ()
  "Collect all TODO items from current org buffer."
  (let ((items '()))
    (org-map-entries
     (lambda ()
       (let* ((heading (org-get-heading t t t t))
              (state (org-get-todo-state))
              (tags (org-get-tags)))
         (push (format "%s %s%s"
                       (or state "NONE")
                       heading
                       (if tags (format " :%s:" (string-join tags ":")) ""))
               items)))
     nil 'file)
    (string-join (nreverse items) "\n")))

;; ── Commands ────────────────────────────────────────────────────

(defun llm-arc-daily-plan ()
  "Generate a prioritized daily plan from org-agenda TODO items."
  (interactive)
  (message "Collecting agenda items...")
  (let ((items (llm-arc-org--collect-agenda-items)))
    (if (string-empty-p items)
        (message "No TODO items found in agenda files")
      (gptel-request items
        :system llm-arc-org-daily-plan-system-prompt
        :callback
        (lambda (response _info)
          (if response
              (with-current-buffer (get-buffer-create "*Daily Plan*")
                (erase-buffer)
                (insert "Daily Plan -- " (format-time-string "%A, %B %d %Y") "\n")
                (insert (make-string 50 ?-) "\n\n")
                (insert response)
                (goto-char (point-min))
                (display-buffer (current-buffer))
                (message "Daily plan generated"))
            (message "Failed to generate daily plan")))))))

(defun llm-arc-smart-capture (text)
  "Convert freeform TEXT into a structured org-mode TODO and capture it.
When called interactively, prompts for text input."
  (interactive "sFreeform task description: ")
  (gptel-request text
    :system llm-arc-org-capture-system-prompt
    :callback
    (lambda (response _info)
      (if response
          (let ((clean-response (string-trim response)))
            (with-current-buffer (get-buffer-create "*Smart Capture Preview*")
              (erase-buffer)
              (org-mode)
              (insert clean-response)
              (goto-char (point-min))
              (display-buffer (current-buffer))
              (message "Smart capture ready -- review and refile")))
        (message "Failed to generate org TODO")))))

(defun llm-arc-project-summary ()
  "Summarize project status from TODO items in the current org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let ((items (llm-arc-org--collect-project-items)))
    (if (string-empty-p items)
        (message "No items found in current buffer")
      (gptel-request items
        :system llm-arc-org-summary-system-prompt
        :callback
        (lambda (response _info)
          (if response
              (with-current-buffer (get-buffer-create "*Project Summary*")
                (erase-buffer)
                (insert "Project Summary -- " (buffer-name) "\n")
                (insert (make-string 50 ?-) "\n\n")
                (insert response)
                (goto-char (point-min))
                (display-buffer (current-buffer))
                (message "Project summary generated"))
            (message "Failed to generate project summary")))))))

;; ── Keybindings ─────────────────────────────────────────────────

(if (boundp 'doom-version)
    ;; Doom: SPC l o prefix (nested under SPC l)
    (map! :leader
          (:prefix "l"
           (:prefix-map ("o" . "org-llm")
            :desc "Daily plan"       "d" #'llm-arc-daily-plan
            :desc "Smart capture"    "c" #'llm-arc-smart-capture
            :desc "Project summary"  "s" #'llm-arc-project-summary)))

  ;; Vanilla: accessible via M-x (no global keybinding to avoid conflicts)
  (when (boundp 'llm-arc-gptel-map)
    (let ((org-map (make-sparse-keymap)))
      (define-key org-map (kbd "d") #'llm-arc-daily-plan)
      (define-key org-map (kbd "c") #'llm-arc-smart-capture)
      (define-key org-map (kbd "s") #'llm-arc-project-summary)
      (define-key llm-arc-gptel-map (kbd "o") org-map))))

(provide 'llm-arc-org)
;;; llm-arc-org.el ends here
