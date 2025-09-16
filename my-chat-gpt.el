;;; -*- lexical-binding: t; -*-

;; Author: 2025 Joakim G.
;; URL:
;; Version: 0.1
;; Keywords: convenience, tools, ai, gpt
;; License: MIT
;; Description: Simple ChatGPT code assistant using gptel
;; 2025-09-11 Initial version
;;
;; This file provides simple Emacs commands to interact with ChatGPT
;; (via the gptel package) for code-related tasks. It includes commands
;; to change, review, explain, and ask questions about the selected region.
;; Responses are either inserted back into the buffer (for changes) or
;; logged in a dedicated buffer (for reviews, explanations, and questions).
;; The commands use a consistent prompt style to guide the model's responses.
;; The log buffer keeps a history of interactions with timestamps and model info.
;;
;; Installation:
;;
;; * Install the gptel package
;; * Put this file in your load-path
;; * Put the OpenAI API key in ~/.authinfo
;; * Get an OpenAI API key from https://platform.openai.com/account/api-keys
;;
;; Add this to your Emacs config:
;;
;; (require 'my-chat-gpt)
;; (setq default-gptel-model 'gpt-5-mini)
;; (global-set-key (kbd "C-c r") #'gptel-review-region)
;; (global-set-key (kbd "C-c e") #'gptel-explain-region)
;; (global-set-key (kbd "C-c a") #'gptel-ask-about-region)
;; (global-set-key (kbd "C-c c") #'gptel-change-region)

(require 'subr-x) ; string-trim, string-empty-p

(defvar default-gptel-model 'gpt-5-mini)

(use-package gptel
  :ensure t
  :config
  (let ((key (or (getenv "OPENAI_API_KEY")
                 (auth-source-pick-first-password
                  :host "api.openai.com"
                  :user "apikey"))))
    (unless key (error "No OpenAI key for api.openai.com (login apikey)"))
    (setq gptel-backend
          (gptel-make-openai
              "OpenAI"
            :host "api.openai.com"
            :endpoint "/v1/chat/completions"
            :key key
            :models (list default-gptel-model)))
    (setq-default gptel-model default-gptel-model)))

(defun my/gptel--repo-tag (root)
  "Short, stable tag for a Git ROOT (folder-name + 7-char md5)."
  (let* ((base (file-name-nondirectory (directory-file-name root)))
         (hash (substring (md5 (expand-file-name root)) 0 7)))
    (format "%s-%s" base hash)))

(defun my/git-root (dir)
  "Return DIR's Git repo root or nil if not in a Git repo."
  (let ((root (locate-dominating-file dir ".git")))
    (when root (expand-file-name root))))

(defun my/gptel--session-name-for (dir)
  "Buffer name for the gptel session associated with DIR's Git repo."
  (let ((root (and (not (file-remote-p dir)) (my/git-root dir))))
    (if root
        (format "*gptel:%s*" (my/gptel--repo-tag root))
      "*gptel:default*")))

(defun my/gptel--ensure-symbol-model (buf &optional fallback)
  (with-current-buffer buf
    (when (boundp 'gptel-model)
      (let* ((base (or gptel-model (default-value 'gptel-model)
                       fallback default-gptel-model)))
        (setq-local gptel-model
                    (if (symbolp base) base (intern (format "%s" base))))))))

(defun my/gptel--ensure-session-buffer (&optional dir)
  "Get/create the persistent gptel chat buffer for DIR's repo (or default)."
  (let* ((dir  (or dir default-directory))
         (name (my/gptel--session-name-for dir))
         (buf  (get-buffer-create name)))
    (with-current-buffer buf
      (when (eq major-mode 'fundamental-mode) (text-mode))
      (unless (bound-and-true-p gptel-mode) (gptel-mode 1)))
    buf))

(defun my/gptel--region-string ()
  (unless (use-region-p) (user-error "No region selected"))
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my/gptel--open-log ()
  "Show and reuse the single chat log buffer/window. Return (BUF . WIN)."
  (let* ((buf (get-buffer-create "*chat-gpt-log*"))
         (win (or (get-buffer-window buf t)
                  (display-buffer
                   buf
                   '((display-buffer-reuse-window display-buffer-pop-up-window)
                     (inhibit-same-window . t))))))
    (when win
      (with-current-buffer buf
        (read-only-mode -1)
        (goto-char (point-max))
        (set-window-point win (point))))
    (cons buf win)))

(defun my/gptel--insert-log-header (buf title model &optional session)
  "Insert a banner into BUF and return a marker at its beginning.
If SESSION is non-nil, include it in the banner."
  (with-current-buffer buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert (format "=== %s | %s | %s%s ===\n\n"
                      title
                      (format-time-string "%H:%M:%S")
                      (or model "unknown")
                      (if session (format " | %s" session) "")))
      (copy-marker start t))))

(defun my/gptel--prompt-region (log-title prompt question)
  "Prompt ChatGPT about the selected region; log to *chat-gpt-log* and keep
point at header."
  (let* ((chatb   (my/gptel--ensure-session-buffer))
         (code    (substring-no-properties (my/gptel--region-string)))
         (bw      (my/gptel--open-log))
         (logb    (car bw))
         (win     (cdr bw))
         (_       (my/gptel--ensure-symbol-model chatb default-gptel-model))
         (sname   (buffer-name chatb))
         (hdr     (my/gptel--insert-log-header logb log-title gptel-model sname))
         (payload (concat (substring-no-properties prompt) code)))
    ;; Write header (and optional question) immediately
    (with-current-buffer logb
      (let ((inhibit-read-only t))
        (goto-char (marker-position hdr))
        (when question
          (insert "Question: " question "\n\n"))))
    ;; Focus the log buffer at the header
    (when (and (window-live-p win) (markerp hdr))
      (set-window-point win (marker-position hdr)))
    (message "[gptel] prompt %d chars to model=%s (session=%s)"
             (length payload) (format "%s" gptel-model) sname)
    (gptel-request
        payload
      :stream  nil
      :buffer  chatb
      :context `(:logbuf ,logb :win ,win :hdr ,hdr)
      :system  "You are a senior engineer. Be precise, concise, and practical."
      :callback
      (lambda (response info)
        (let* ((err  (plist-get info :error))
               (text (cond (err (format "[ERROR]\n%S\n" err))
                           ((stringp response) response)
                           ((and (listp response) (plist-get response :content))
                            (plist-get response :content))
                           (t (format "%s" response)))))
          (when (buffer-live-p logb)
            (with-current-buffer logb
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert text "\n\n"))))
          (when (and (window-live-p win) (markerp hdr))
            (set-window-point win (marker-position hdr))))))))

(defun my/gptel-strip-fences (s)
  "If S is an entire fenced code block, return its body; otherwise return S."
  (if (stringp s)
      (save-match-data
        (if (string-match "^```[[:alnum:]_-]*\\(?:\r?\n\\|\\'\\)" s)
            (let* ((beg (match-end 0))
                   (end (or (string-match "\r?\n```[ \t]*\\'" s beg)
                            (length s))))
              (substring s beg end))
          s))
    s))

(defun my/gptel-maybe-indent (beg end)
  "Indent without echo-area noise."
  (when (derived-mode-p 'prog-mode)
    (save-excursion
      (let ((inhibit-message t)
            (message-log-max nil)
            (inhibit-redisplay t))
        (ignore-errors
          (indent-region (max (point-min) beg)
                         (min (point-max) end)))))))

(defun gptel-review-region ()
  (interactive)
  (my/gptel--prompt-region
   "Review code"
   "Review the following code. Be concrete and terse.\n\
- Find bugs, undefined behavior, API misuse.\n\
- Note edge cases and complexity hotspots.\n\
- Suggest exact fixes. Use minimal diffs/snippets only when needed.\n\n\
CODE:\n\n" nil))

(defun gptel-explain-region ()
  (interactive)
  (my/gptel--prompt-region
   "Explain code"
   "Explain this code clearly and concisely for an experienced developer.\n\
- What it does step-by-step.\n\
- Key data flows and invariants.\n\
- Time/space complexity for hot paths.\n\
- Edge cases and failure modes.\n\
- Provide a short commented version if helpful.\n\n\
CODE:\n\n" nil))

(defun gptel-ask-about-region (question)
  (interactive "sQuestion: ")
  (my/gptel--prompt-region
   "Question code"
   (concat
    "Answer the user's QUESTION about the following code.\n"
    "Be precise and concise. If relevant, include minimal snippets.\n\n"
    "QUESTION:\n"
    question "\n\n"
    "CODE:\n\n")
   question))

(defun gptel-change-region (prompt)
  "Send active region with PROMPT to GPT and replace it with the reply."
  (interactive "sChange request: ")
  (unless (use-region-p) (user-error "No region selected"))
  (let* ((buf (current-buffer))
         (rb (copy-marker (region-beginning) t))
         (re (copy-marker (region-end) nil))
         (chatb (my/gptel--ensure-session-buffer))
         (_ (my/gptel--ensure-symbol-model chatb default-gptel-model))
         (input (buffer-substring-no-properties rb re))
         (payload (concat (substring-no-properties prompt)
                          "\n\n---\nRewrite ONLY this code.\n"
                          "Return code only (no prose). Fences allowed.\n\n"
                          input)))
    (deactivate-mark)
    (message "[gptel] change %d chars to model=%s (session=%s)"
             (length payload) (format "%s" gptel-model) (buffer-name chatb))
    (gptel-request
        payload
      :stream nil
      :buffer chatb
      :callback
      (lambda (response info)
        (unwind-protect
            (let* ((err (or (plist-get info :error)
                            (alist-get 'error info)))
                   (status (or (plist-get info :status)
                               (alist-get 'status info)))
                   (http (or (plist-get info :http-status)
                             (alist-get 'http-status info))))
              (cond
               (err
                (message "[gptel] ERROR (http=%s status=%s): %s"
                         (or http "?") (or status "?")
                         (if (stringp err) err (format "%S" err))))
               (t
                (let* ((rs (cond
                            ((stringp response) response)
                            ((and (listp response) (plist-get response :content))
                             (plist-get response :content))
                            (t (format "%s" response))))
                       (body (my/gptel-strip-fences (string-trim rs)))
                       (s    (if (and (stringp body) (not (string-empty-p body)))
                                 body
                               (string-trim rs))))
                  (if (string-empty-p s)
                      (progn
                        (message "[gptel] empty after strip (http=%s status=%s)"
                                 (or http "?") (or status "?"))
                        (message "[gptel] raw: %.200s" rs))  ;; debug preview
                    (when (and (buffer-live-p buf)
                               (markerp rb) (marker-position rb)
                               (markerp re) (marker-position re))
                      (with-current-buffer buf
                        (let ((inhibit-modification-hooks t)
                              (inhibit-message t)
                              (message-log-max nil)
                              (inhibit-redisplay t))
                          (save-excursion
                            (if (fboundp 'replace-region-contents) ; Emacs 29
                                (condition-case _
                                    (replace-region-contents rb re
                                                             (lambda (_a _b) s))
                                  (wrong-number-of-arguments
                                   (replace-region-contents rb re
                                                            (lambda () s))))
                              (atomic-change-group
                                (delete-region rb re)
                                (goto-char rb)
                                (insert s))))
                          (my/gptel-maybe-indent rb (min (point-max)
                                                         (max rb re)))))))
                  (message "[gptel] done model=%s" (format "%s" gptel-model))))))
          (when (markerp rb) (set-marker rb nil))
          (when (markerp re) (set-marker re nil)))))))

(provide 'my-chat-gpt)
