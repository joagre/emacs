;; Your existing gptel setup (backend/model) here, e.g.:

;; (require 'my-chat-gpt)
;; (global-set-key (kbd "C-c g") #'gptel-change-region)
;; (global-set-key (kbd "C-c r") #'gptel-review-region)
;; (global-set-key (kbd "C-c e") #'gptel-explain-region)
;; (global-set-key (kbd "C-c a") #'gptel-ask-about-region)

(use-package gptel :ensure t)

(unless (boundp 'gptel-backend)
  (setq gptel-backend
        (gptel-make-openai "OpenAI"
          :host "api.openai.com"
          :endpoint "/v1/chat/completions"
          :models '("gpt-5-mini" "gpt-5" "gpt-5-nano" "gpt-4o" "gpt-4o-mini"))))
(when (fboundp 'gptel-set-backend)
  (gptel-set-backend gptel-backend))

(unless (boundp 'gptel-model)
  (defvar gptel-model "gpt-5-mini"))
(when (fboundp 'gptel-set-model)
  (gptel-set-model "gpt-5-mini"))

(defun my/gptel-strip-fences (s)
  (if (and (stringp s)
           (string-match "^```[a-zA-Z0-9_-]*\\(\n\\|\\'\\)" s))
      (let* ((beg (match-end 0))
             (end (or (string-match "\n```\\s-*$" s) (length s))))
        (substring s beg end))
    s))

(defun my/gptel-maybe-indent (beg end)
  "Indent without echo-area noise."
  (when (derived-mode-p 'prog-mode)
    (let ((inhibit-message t))
      (ignore-errors (indent-region beg end)))))

(defun gptel-change-region (prompt)
  "Send active region with PROMPT to GPT and replace it with the reply."
  (interactive "sChange request: ")
  (unless (use-region-p) (user-error "No region selected"))
  (require 'gptel)
  (let* ((buf   (current-buffer))
         (rb    (copy-marker (region-beginning) t))
         (re    (copy-marker (region-end) nil))
         (model (or (and (boundp 'gptel-model) gptel-model) "gpt-5-mini")))
    (when (or (null model) (string-empty-p model))
      (user-error
       "gptel model is nil/empty; set gptel-model or (gptel-set-model ...)"))
    (let ((input (buffer-substring-no-properties rb re)))
      (deactivate-mark)
      (message "[gptel] sending to model=%s…" model)
      (gptel-request
       (concat (substring-no-properties prompt)
               "\n\n---\nRewrite ONLY this code.\n"
               "Return code only (no prose). Fences allowed.\n\n"
               input)
       :stream nil
       :context `(:buf ,buf :rb ,rb :re ,re :model ,model)
       :callback
       (lambda (response info)
         (let* ((ctx    (or (plist-get info :context)
                            (alist-get :context info)))
                (buf    (plist-get ctx :buf))
                (rb     (plist-get ctx :rb))
                (re     (plist-get ctx :re))
                (err    (or (plist-get info :error)
                            (alist-get 'error info)))
                (status (or (plist-get info :status)
                            (alist-get 'status info)))
                (http   (or (plist-get info :http-status)
                            (alist-get 'http-status info))))
           (unwind-protect
               (cond
                (err
                 (message "[gptel] ERROR (http=%s status=%s): %s"
                          (or http "?") (or status "?")
                          (if (stringp err) err (format "%S" err))))
                ((or (null response)
                     (and (stringp response)
                          (string-empty-p (string-trim response))))
                 (message "[gptel] empty response (http=%s status=%s)"
                          (or http "?") (or status "?")))
                (t
                 ;; Normalize response to string
                 (let* ((rs (cond
                             ((stringp response) response)
                             ((and (listp response)
                                   (plist-get response :content))
                              (plist-get response :content))
                             (t (format "%s" response))))
                        (s (my/gptel-strip-fences (string-trim rs))))
                   (when (and (buffer-live-p buf)
                              (markerp rb) (marker-position rb)
                              (markerp re) (marker-position re))
                     (with-current-buffer buf
                       (let ((inhibit-modification-hooks t))
                         (save-excursion
                           (atomic-change-group
                             (delete-region rb re)
                             (goto-char rb)
                             (insert s)
                             (my/gptel-maybe-indent rb (point)))))))))
                 (message "[gptel] done model=%s" model)))
           (when (markerp rb) (set-marker rb nil))
           (when (markerp re) (set-marker re nil))))))))

(defun my/gptel--open-log ()
  "Show and reuse the single chat log buffer/window. Return (BUF . WIN)."
  (let* ((buf (get-buffer-create "chat-gpt-log"))
         (win (or (get-buffer-window buf t)
                  (display-buffer
                   buf
                   '((display-buffer-reuse-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t))))))
    (with-current-buffer buf
      (read-only-mode -1)
      (goto-char (point-max)))
    (cons buf win)))

(defun my/gptel--insert-log-header (buf title model)
  "Insert a header and return a marker at the first '#'."
  (with-current-buffer buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert (format "\n\n=== %s | %s | %s ===\n\n"
                      title
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (or model "unknown")))
      (copy-marker start t))))

(defun my/gptel--region-string ()
  (unless (use-region-p) (user-error "No region selected"))
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun gptel-review-region ()
  "Ask ChatGPT to REVIEW the selected region; log to chat-gpt-log and keep point at header."
  (interactive)
  (require 'gptel)
  (let* ((model (and (boundp 'gptel-model) gptel-model))
         (code  (my/gptel--region-string))
         (bw    (my/gptel--open-log))
         (logb  (car bw))
         (win   (cdr bw))
         (hdr   (my/gptel--insert-log-header logb "Code Review" model)))
    ;; position point at header immediately
    (when (window-live-p win) (select-window win) (goto-char hdr))
    (gptel-request
        (concat
         "Review the following code. Be concrete and terse.\n"
         "- Find bugs, undefined behavior, API misuse.\n"
         "- Note edge cases and complexity hotspots.\n"
         "- Suggest exact fixes. Use minimal diffs/snippets only when needed.\n\n"
         "CODE:\n\n" code)
      :stream nil
      :context `(:logbuf ,logb :win ,win :hdr ,hdr)
      :system "You are a senior engineer. Be precise, concise, and practical."
      :callback
      (lambda (response info)
        (let* ((ctx  (or (plist-get info :context) (alist-get :context info)))
               (lb   (plist-get ctx :logbuf))
               (win  (plist-get ctx :win))
               (hdr  (plist-get ctx :hdr))
               (err  (or (plist-get info :error)
                         (alist-get 'error info)))
               (http (or (plist-get info :http-status)
                         (alist-get 'http-status info)))
               (stat (or (plist-get info :status)
                         (alist-get 'status info)))
               (text (cond
                      (err (format "[ERROR] http=%s status=%s\n%S\n"
                                   (or http "?") (or stat "?") err))
                      ((stringp response) response)
                      ((and (listp response) (plist-get response :content))
                       (plist-get response :content))
                      (t (format "%s" response)))))
          (when (buffer-live-p lb)
            (with-current-buffer lb
              (save-excursion
                (goto-char (point-max))
                (insert text "\n"))))
          ;; restore point to header after appending
          (when (and (window-live-p win) (markerp hdr))
            (select-window win)
            (goto-char hdr)))))))

(defun gptel-explain-region ()
  "Ask ChatGPT to EXPLAIN the selected region; log to chat-gpt-log and keep point at header."
  (interactive)
  (require 'gptel)
  (let* ((model (and (boundp 'gptel-model) gptel-model))
         (code  (my/gptel--region-string))
         (bw    (my/gptel--open-log))
         (logb  (car bw))
         (win   (cdr bw))
         (hdr   (my/gptel--insert-log-header logb "Code Explain" model)))
    ;; position point at header immediately
    (when (window-live-p win) (select-window win) (goto-char hdr))
    (gptel-request
     (concat
      "Explain this code clearly and concisely for an experienced developer.\n"
      "- What it does step-by-step.\n"
      "- Key data flows and invariants.\n"
      "- Time/space complexity for hot paths.\n"
      "- Edge cases and failure modes.\n"
      "- Provide a short commented version if helpful.\n\n"
      "CODE:\n\n" code)
     :stream nil
     :context `(:logbuf ,logb :win ,win :hdr ,hdr)
     :system "You are a senior engineer. Be precise, concise, and practical."
     :callback
     (lambda (response info)
       (let* ((ctx  (or (plist-get info :context) (alist-get :context info)))
              (lb   (plist-get ctx :logbuf))
              (win  (plist-get ctx :win))
              (hdr  (plist-get ctx :hdr))
              (err  (or (plist-get info :error)
                        (alist-get 'error info)))
              (http (or (plist-get info :http-status)
                        (alist-get 'http-status info)))
              (stat (or (plist-get info :status)
                        (alist-get 'status info)))
              (text (cond
                     (err (format "[ERROR] http=%s status=%s\n%S\n"
                                  (or http "?") (or stat "?") err))
                     ((stringp response) response)
                     ((and (listp response) (plist-get response :content))
                      (plist-get response :content))
                     (t (format "%s" response)))))
         (when (buffer-live-p lb)
           (with-current-buffer lb
             (save-excursion
               (goto-char (point-max))
               (insert text "\n"))))
         ;; restore point to header after appending
         (when (and (window-live-p win) (markerp hdr))
           (select-window win)
           (goto-char hdr)))))))

(defun gptel-ask-about-region (question)
  "Ask ChatGPT QUESTION about the selected region.
Appends the answer to the 'chat-gpt-log' buffer and keeps point at the header."
  (interactive "sQuestion: ")
  (require 'gptel)
  (unless (use-region-p) (user-error "No region selected"))
  (let* ((model (and (boundp 'gptel-model) gptel-model))
         (code  (buffer-substring-no-properties (region-beginning)
                                                (region-end)))
         (bw    (my/gptel--open-log))
         (logb  (car bw))
         (win   (cdr bw))
         (hdr   (my/gptel--insert-log-header logb "Code Question" model)))
    ;; show the question right under the header
    (with-current-buffer logb
      (goto-char (marker-position hdr))
      (forward-line 2) ;; after the blank line following header
      (insert (format "Q: %s\n\n" question)))
    ;; jump cursor to header
    (when (window-live-p win)
      (select-window win)
      (goto-char hdr))
    ;; ask the model (non-streaming), with region as context
    (gptel-request
     (concat
      "Answer the user's QUESTION about the following code.\n"
      "Be precise and concise. If relevant, include minimal snippets.\n\n"
      "QUESTION:\n" question "\n\n"
      "CODE:\n\n" code)
     :stream nil
     :context `(:logbuf ,logb :win ,win :hdr ,hdr)
     :system "You are a senior engineer. Be precise, concise, and practical."
     :callback
     (lambda (response info)
       (let* ((ctx  (or (plist-get info :context) (alist-get :context info)))
              (lb   (plist-get ctx :logbuf))
              (win  (plist-get ctx :win))
              (hdr  (plist-get ctx :hdr))
              (err  (or (plist-get info :error)
                        (alist-get 'error info)))
              (http (or (plist-get info :http-status)
                        (alist-get 'http-status info)))
              (stat (or (plist-get info :status)
                        (alist-get 'status info)))
              (text (cond
                     (err (format "[ERROR] http=%s status=%s\n%S\n"
                                  (or http "?") (or stat "?") err))
                     ((stringp response) response)
                     ((and (listp response) (plist-get response :content))
                      (plist-get response :content))
                     (t (format "%s" response)))))
         (when (buffer-live-p lb)
           (with-current-buffer lb
             (save-excursion
               (goto-char (point-max))
               (insert text "\n"))))
         ;; keep point at the header
         (when (and (window-live-p win) (markerp hdr))
           (select-window win)
           (goto-char hdr)))))))

(provide 'my-chat-gpt)
