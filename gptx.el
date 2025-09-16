;;; gptx.el --- Lean, predictable gptel workflows for code  -*- lexical-binding: t; -*-
;;
;; Author: Joakim G.
;; Maintainer: Joakim G.
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (gptel "0.8"))
;; Keywords: convenience, tools, ai
;; License: MIT
;; URL:
;;
;;; Commentary:
;;
;; Region-aware helpers around gptel:
;; - Review, Explain, Ask: send code to a persistent repo-scoped chat buffer,
;;   append responses to a single log buffer.
;; - Change: rewrite region or buffer in place with minimal ceremony.
;;
;; Example usage:
;;   (require 'gptx)
;;   (setq gptx-model 'gpt-5-mini)
;;   (global-set-key (kbd "C-c r") #'gptx-review)
;;   (global-set-key (kbd "C-c e") #'gptx-explain)
;;   (global-set-key (kbd "C-c a") #'gptx-ask)
;;   (global-set-key (kbd "C-c c") #'gptx-change)
;;
;;; Code:

(eval-when-compile (require 'subr-x))
(require 'cl-lib)
(require 'auth-source)
(require 'gptel)

;; ----- Customization -------------------------------------------------

(defgroup gptx nil
  "Lean, predictable gptel commands for code."
  :group 'tools
  :prefix "gptx-")

(defcustom gptx-model 'gpt-5-mini
  "Default gptel model (symbol)."
  :type 'symbol
  :group 'gptx)

(defcustom gptx-system
  "You are a senior engineer. Be precise, concise, and practical."
  "System prompt used for all requests."
  :type 'string
  :group 'gptx)

(defcustom gptx-indent-after-change t
  "Indent the replaced region or buffer after a successful change."
  :type 'boolean
  :group 'gptx)

(defcustom gptx-act-on-buffer-when-no-region t
  "If non-nil, commands operate on the whole buffer when no region is active."
  :type 'boolean
  :group 'gptx)

(defcustom gptx-log-buffer "*chat-gpt-log*"
  "Name of the shared log buffer."
  :type 'string
  :group 'gptx)

(defcustom gptx-log-max-lines 5000
  "Trim the log buffer to approximately this many trailing lines."
  :type 'integer
  :group 'gptx)

;; ----- Backend bootstrap --------------------------------------------

(defun gptx--ensure-backend ()
  "Ensure `gptel-backend' is configured for OpenAI."
  (unless (and (boundp 'gptel-backend) gptel-backend)
    (let* ((key (or (getenv "OPENAI_API_KEY")
                    (auth-source-pick-first-password
                     :host "api.openai.com" :user "apikey"))))
      (unless (and key (stringp key) (not (string-empty-p key)))
        (error "No OpenAI key for api.openai.com (login apikey)"))
      (setq gptel-backend
            (gptel-make-openai
             "OpenAI"
             :host "api.openai.com"
             :endpoint "/v1/chat/completions"
             :key key
             :models (list gptx-model)))))
  (setq-default gptel-model gptx-model))

;; ----- Repo-scoped sessions and log ---------------------------------

(defun gptx--git-root (dir)
  "Return DIR git repo root or nil."
  (let ((root (locate-dominating-file dir ".git")))
    (when root (expand-file-name root))))

(defun gptx--repo-tag (root)
  "Short stable tag for a git ROOT (folder name plus 7-char md5)."
  (let* ((base (file-name-nondirectory (directory-file-name root)))
         (hash (substring (md5 (expand-file-name root)) 0 7)))
    (format "%s-%s" base hash)))

(defun gptx--session-name-for (dir)
  "Name of the gptel session buffer associated with DIR repo."
  (let* ((root (and (not (file-remote-p dir))
                    (gptx--git-root dir))))
    (if root
        (format "*gptel:%s*" (gptx--repo-tag root))
      "*gptel:default*")))

(defun gptx--ensure-symbol-model (buf &optional fallback)
  "Ensure `gptel-model' in BUF is a symbol."
  (with-current-buffer buf
    (when (boundp 'gptel-model)
      (let* ((base (or gptel-model (default-value 'gptel-model)
                       fallback gptx-model)))
        (setq-local gptel-model
                    (if (symbolp base) base (intern (format "%s" base))))))))

(defun gptx--ensure-session-buffer (&optional dir)
  "Get or create the persistent gptel chat buffer for DIR repo."
  (let* ((dir (or dir default-directory))
         (name (gptx--session-name-for dir))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (when (eq major-mode 'fundamental-mode) (text-mode))
      (unless (bound-and-true-p gptel-mode) (gptel-mode 1)))
    buf))

(defun gptx--open-log ()
  "Return (BUF . WIN) for the shared log buffer.
Ensure it is a proper special-mode and make it writable for appends."
  (let* ((buf (get-buffer-create gptx-log-buffer))
         (win (or (get-buffer-window buf t)
                  (display-buffer
                   buf
                   '((display-buffer-reuse-window display-buffer-pop-up-window)
                     (inhibit-same-window . t))))))
    (when win
      (with-current-buffer buf
        (unless (derived-mode-p 'special-mode)
          (special-mode)
          (setq-local truncate-lines t))
        (read-only-mode -1)
        (goto-char (point-max))
        (set-window-point win (point))))
    (cons buf win)))

(defun gptx--trim-log (buf)
  "Trim BUF to last `gptx-log-max-lines' lines."
  (when (and (integerp gptx-log-max-lines)
             (> gptx-log-max-lines 0)
             (buffer-live-p buf))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (let ((total (count-lines (point-min) (point-max))))
            (when (> total gptx-log-max-lines)
              (goto-char (point-max))
              (forward-line (- gptx-log-max-lines))
              (beginning-of-line)
              (delete-region (point-min) (point)))))))))

(defun gptx--log-insert (buf &rest strings)
  "Append STRINGS to BUF and trim."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (apply #'insert strings)
        (insert "\n\n"))
      (gptx--trim-log buf))))

(defun gptx--insert-log-header (buf title model &optional session)
  "Insert a banner into BUF and return a marker at its beginning."
  (with-current-buffer buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert (format "=== %s | %s | %s%s ===\n\n"
                      title
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (or model "unknown")
                      (if session (format " | %s" session) "")))
      (copy-marker start t))))

;; ----- Utilities ----------------------------------------------------

(defun gptx--region-or-buffer ()
  "Return (BEG . END) of active region, or whole buffer if allowed."
  (cond
   ((use-region-p) (cons (region-beginning) (region-end)))
   (gptx-act-on-buffer-when-no-region (cons (point-min) (point-max)))
   (t (user-error "No region selected"))))

(defun gptx--slice (beg end)
  "Return buffer substring without properties for [BEG, END)."
  (buffer-substring-no-properties beg end))

(defun gptx--strip-fences (s)
  "If S is a full fenced code block, return its body; otherwise return S."
  (if (and (stringp s)
           (string-match "^```[[:alnum:]_-]*\\(?:\r?\n\\|\\'\\)" s))
      (let* ((beg (match-end 0))
             (end (or (string-match "\r?\n```[ \t]*\\'" s beg) (length s))))
        (substring s beg end))
    s))

(defun gptx--maybe-indent (beg end)
  "Indent region when in prog-mode if `gptx-indent-after-change' is non-nil."
  (when (and gptx-indent-after-change (derived-mode-p 'prog-mode))
    (save-excursion
      (let ((inhibit-message t)
            (message-log-max nil)
            (inhibit-redisplay t))
        (ignore-errors
          (indent-region (max (point-min) beg)
                         (min (point-max) end)))))))

;; ----- Core plumbing ------------------------------------------------

(defun gptx--prompt (log-title prompt question beg end)
  "Send code slice [BEG, END) with PROMPT to gptel and log."
  (gptx--ensure-backend)
  (let* ((chatb (gptx--ensure-session-buffer))
         (_model(gptx--ensure-symbol-model chatb gptx-model))
         (code (gptx--slice beg end))
         (bw (gptx--open-log))
         (logb (car bw))
         (win (cdr bw))
         (sname (buffer-name chatb))
         (hdr (gptx--insert-log-header logb log-title gptel-model sname))
         (payload (concat prompt code)))
    ;; Write header and optional question.
    (when question
      (with-current-buffer logb
        (let ((inhibit-read-only t))
          (goto-char (marker-position hdr))
          (insert "Question: " question "\n\n"))))
    ;; Focus the log buffer at the header.
    (when (and (window-live-p win) (markerp hdr))
      (set-window-point win (marker-position hdr)))
    (message "[gptel] prompt %d chars model=%s session=%s"
             (length payload) (format "%s" gptel-model) sname)
    (gptel-request
     payload
     :buffer  chatb
     :context `(:logbuf ,logb :win ,win :hdr ,hdr)
     :system  gptx-system
     :callback
     (lambda (response info)
       (let* ((err (plist-get info :error))
              (text (cond
                     (err (format "[ERROR]\n%S" err))
                     ((stringp response) response)
                     ((and (listp response) (plist-get response :content))
                      (plist-get response :content))
                     (t (format "%s" response)))))
         (gptx--log-insert logb text)
         (when (and (window-live-p win) (markerp hdr))
           (set-window-point win (marker-position hdr))))))))

(defun gptx--change (prompt beg end)
  "Rewrite [BEG, END) with PROMPT."
  (gptx--ensure-backend)
  (let* ((buf (current-buffer))
         (chatb (gptx--ensure-session-buffer))
         (_model (gptx--ensure-symbol-model chatb gptx-model))
         (input (gptx--slice beg end))
         (payload (concat (string-trim-right prompt)
                          "\n\n---\nRewrite ONLY this code.\n"
                          "Return code only (no prose). Fences allowed.\n\n"
                          input)))
    (message "[gptel] change %d chars model=%s session=%s"
             (length payload) (format "%s" gptel-model) (buffer-name chatb))
    (gptel-request
     payload
     :buffer  chatb
     :system  gptx-system
     :callback
     (lambda (response info)
       (let* ((err (plist-get info :error))
              (status (plist-get info :status))
              (http (plist-get info :http-status)))
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
                  (body (gptx--strip-fences (string-trim rs)))
                  (s (if (and (stringp body) (not (string-empty-p body))) body
                       (string-trim rs))))
             (if (string-empty-p s)
                 (progn
                   (message "[gptel] empty after strip (http=%s status=%s)"
                            (or http "?") (or status "?"))
                   (message "[gptel] raw: %.200s" rs))
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((inhibit-modification-hooks t)
                         (inhibit-message t)
                         (message-log-max nil)
                         (inhibit-redisplay t))
                     (save-excursion
                       (if (fboundp 'replace-region-contents) ; Emacs 29+
                           (condition-case _
                               (replace-region-contents beg end
                                                        (lambda (_a _b) s))
                             (wrong-number-of-arguments
                              (replace-region-contents beg end (lambda () s))))
                         (atomic-change-group
                           (delete-region beg end)
                           (goto-char beg)
                           (insert s))))
                     (gptx--maybe-indent beg (min (point-max) end))))))
             (message "[gptel] done model=%s" (format "%s" gptel-model))))))))))

;; ----- Public commands ----------------------------------------------

;;;###autoload
(defun gptx-review ()
  "Review the active region or whole buffer."
  (interactive)
  (cl-destructuring-bind (beg . end) (gptx--region-or-buffer)
    (gptx--prompt
     "Review code"
     "Review the following code. Be concrete and terse.\n\
- Find bugs, undefined behavior, API misuse.\n\
- Note edge cases and complexity hotspots.\n\
- Suggest exact fixes. Use minimal diffs or snippets only when needed.\n\n\
CODE:\n\n"
     nil beg end)))

;;;###autoload
(defun gptx-explain ()
  "Explain the active region or whole buffer."
  (interactive)
  (cl-destructuring-bind (beg . end) (gptx--region-or-buffer)
    (gptx--prompt
     "Explain code"
     "Explain this code clearly and concisely for an experienced developer.\n\
- What it does step by step.\n\
- Key data flows and invariants.\n\
- Time and space complexity for hot paths.\n\
- Edge cases and failure modes.\n\
- Provide a short commented version if helpful.\n\n\
CODE:\n\n"
     nil beg end)))

;;;###autoload
(defun gptx-ask (question)
  "Ask QUESTION about the active region or whole buffer."
  (interactive "sQuestion: ")
  (cl-destructuring-bind (beg . end) (gptx--region-or-buffer)
    (gptx--prompt
     "Question code"
     (concat "Answer the user's QUESTION about the following code.\n"
             "Be precise and concise. If relevant, include minimal snippets.\n\n"
             "QUESTION:\n" question "\n\nCODE:\n\n")
     question beg end)))

;;;###autoload
(defun gptx-change (prompt)
  "Send active region or whole buffer with PROMPT and replace it with the reply."
  (interactive "sChange request: ")
  (cl-destructuring-bind (beg . end) (gptx--region-or-buffer)
    (deactivate-mark)
    (gptx--change prompt beg end)))

(provide 'gptx)
