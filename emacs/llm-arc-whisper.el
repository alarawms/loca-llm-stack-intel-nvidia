;;; llm-arc-whisper.el --- Dictation / STT integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Speech-to-text via the containerized whisper server.
;; Records audio via ffmpeg/pw-record subprocess, POSTs to the
;; whisper container at localhost:9000, and inserts text at point.
;;
;; Works in any buffer: chat, email compose, org, code.
;;
;; Doom Emacs:  SPC l w  (toggle dictation)
;; Vanilla:     C-c l w  (toggle dictation)

;;; Code:

(require 'json)

(defgroup llm-arc-whisper nil
  "llm-arc speech-to-text configuration."
  :group 'llm-arc
  :prefix "llm-arc-whisper-")

(defcustom llm-arc-whisper-url "http://127.0.0.1:9000/inference"
  "URL of the whisper.cpp server transcription endpoint."
  :type 'string)

(defcustom llm-arc-whisper-audio-file "/tmp/llm-arc-emacs-dictate.wav"
  "Temporary file for audio recording."
  :type 'string)

(defcustom llm-arc-whisper-record-command
  (cond
   ((executable-find "ffmpeg")
    '("ffmpeg" "-y" "-f" "pulse" "-i" "default"
      "-ar" "16000" "-ac" "1" "-f" "wav"))
   ((executable-find "pw-record")
    '("pw-record" "--format=s16" "--rate=16000" "--channels=1"))
   (t '("ffmpeg" "-y" "-f" "pulse" "-i" "default"
        "-ar" "16000" "-ac" "1" "-f" "wav")))
  "Command to record audio.  The audio file path is appended."
  :type '(repeat string))

;; ── State ───────────────────────────────────────────────────────

(defvar llm-arc-whisper--recording-process nil
  "Process handle for the active recording.")

(defvar llm-arc-whisper--recording-buffer nil
  "Buffer where transcription result should be inserted.")

(defvar llm-arc-whisper--recording-point nil
  "Point position where transcription should be inserted.")

;; ── Recording ───────────────────────────────────────────────────

(defun llm-arc-whisper-start ()
  "Start recording audio from the default audio source."
  (interactive)
  (when llm-arc-whisper--recording-process
    (user-error "Already recording.  Use `llm-arc-whisper-stop' first"))
  (setq llm-arc-whisper--recording-buffer (current-buffer)
        llm-arc-whisper--recording-point (point))
  (when (file-exists-p llm-arc-whisper-audio-file)
    (delete-file llm-arc-whisper-audio-file))
  (let ((cmd (append llm-arc-whisper-record-command
                     (list llm-arc-whisper-audio-file))))
    (setq llm-arc-whisper--recording-process
          (make-process
           :name "llm-arc-whisper-record"
           :buffer nil
           :command cmd
           :noquery t))
    (message "Recording... Press the same key again to stop and transcribe.")))

(defun llm-arc-whisper-stop ()
  "Stop recording and send audio to whisper server for transcription."
  (interactive)
  (unless llm-arc-whisper--recording-process
    (user-error "Not recording"))
  (when (process-live-p llm-arc-whisper--recording-process)
    (interrupt-process llm-arc-whisper--recording-process)
    (sit-for 0.5))
  (setq llm-arc-whisper--recording-process nil)
  (message "Transcribing...")
  (llm-arc-whisper--transcribe-async
   llm-arc-whisper-audio-file
   llm-arc-whisper--recording-buffer
   llm-arc-whisper--recording-point))

(defun llm-arc-whisper-toggle ()
  "Toggle dictation: start recording or stop and transcribe."
  (interactive)
  (if llm-arc-whisper--recording-process
      (llm-arc-whisper-stop)
    (llm-arc-whisper-start)))

;; ── Transcription ───────────────────────────────────────────────

(defun llm-arc-whisper--transcribe-async (audio-file target-buffer target-point)
  "Transcribe AUDIO-FILE via whisper server, insert at TARGET-POINT in TARGET-BUFFER."
  (let ((output-buffer (generate-new-buffer " *whisper-transcribe*")))
    (make-process
     :name "llm-arc-whisper-transcribe"
     :buffer output-buffer
     :command (list "curl" "-sf" "-X" "POST"
                    llm-arc-whisper-url
                    "-F" (format "file=@%s" audio-file)
                    "-F" "response_format=json"
                    "-F" "temperature=0.0")
     :sentinel
     (lambda (process _event)
       (when (eq (process-status process) 'exit)
         (unwind-protect
             (let ((text (llm-arc-whisper--parse-response output-buffer)))
               (if (and text (not (string-empty-p text)))
                   (progn
                     (with-current-buffer target-buffer
                       (goto-char target-point)
                       (insert text))
                     (message "Dictation: %s" (truncate-string-to-width text 60)))
                 (message "Whisper: no transcription returned")))
           (kill-buffer output-buffer)))))))

(defun llm-arc-whisper--parse-response (buffer)
  "Parse whisper server JSON response from BUFFER, return text string."
  (with-current-buffer buffer
    (goto-char (point-min))
    (condition-case nil
        (let ((json (json-read)))
          (or (alist-get 'text json)
              (alist-get 'error json)
              ""))
      (error
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun llm-arc-whisper-transcribe-file (file)
  "Transcribe FILE via whisper server and insert at point."
  (interactive "fAudio file: ")
  (llm-arc-whisper--transcribe-async
   (expand-file-name file) (current-buffer) (point)))

;; ── Keybindings ─────────────────────────────────────────────────

(if (boundp 'doom-version)
    ;; Doom: SPC l w (toggle) under the llm prefix
    (map! :leader
          (:prefix "l"
           :desc "Dictate"  "w" #'llm-arc-whisper-toggle
           :desc "Transcribe file" "W" #'llm-arc-whisper-transcribe-file))

  ;; Vanilla: C-c l w (nested under C-c l prefix)
  (when (boundp 'llm-arc-gptel-map)
    (define-key llm-arc-gptel-map (kbd "w") #'llm-arc-whisper-toggle)
    (define-key llm-arc-gptel-map (kbd "W") #'llm-arc-whisper-transcribe-file)))

;; ── Optional: whisper.el fallback ───────────────────────────────

(with-eval-after-load 'whisper
  (setq whisper-model "base"
        whisper-language "en"
        whisper-translate nil))

(provide 'llm-arc-whisper)
;;; llm-arc-whisper.el ends here
