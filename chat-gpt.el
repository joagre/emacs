;;; chat-gpt.el --- ChatGPT integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ChatGPT Extensions
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (request "0.3.0"))
;; Keywords: ai, chatgpt, openai, tools
;; URL: https://github.com/joagre/emacs

;;; Commentary:

;; This package provides ChatGPT integration for Emacs, allowing users to
;; interact with OpenAI's ChatGPT directly from within Emacs.
;;
;; Features:
;; - Chat with ChatGPT in a dedicated buffer
;; - Configurable API settings (key, model, temperature)
;; - Send selected text or entire buffer for processing
;; - Conversation history management
;; - Streaming responses support

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

;;; Customization

(defgroup chat-gpt nil
  "ChatGPT integration for Emacs."
  :group 'tools
  :prefix "chat-gpt-")

(defcustom chat-gpt-api-key ""
  "OpenAI API key for ChatGPT.
Get your API key from https://platform.openai.com/api-keys"
  :type 'string
  :group 'chat-gpt)

(defcustom chat-gpt-model "gpt-3.5-turbo"
  "ChatGPT model to use.
Common options: gpt-3.5-turbo, gpt-4, gpt-4-turbo-preview"
  :type 'string
  :group 'chat-gpt)

(defcustom chat-gpt-temperature 0.7
  "Temperature for ChatGPT responses (0.0 to 2.0).
Lower values make responses more focused and deterministic."
  :type 'number
  :group 'chat-gpt)

(defcustom chat-gpt-max-tokens 2048
  "Maximum number of tokens in ChatGPT response."
  :type 'integer
  :group 'chat-gpt)

(defcustom chat-gpt-system-message "You are a helpful assistant."
  "System message to set ChatGPT behavior."
  :type 'string
  :group 'chat-gpt)

;;; Variables

(defvar chat-gpt-conversation-history '()
  "Current conversation history with ChatGPT.")

(defvar chat-gpt-buffer-name "*ChatGPT*"
  "Name of the ChatGPT conversation buffer.")

(defvar chat-gpt-api-url "https://api.openai.com/v1/chat/completions"
  "OpenAI API endpoint URL.")

;;; Utility Functions

(defun chat-gpt--check-api-key ()
  "Check if API key is configured."
  (if (string-empty-p chat-gpt-api-key)
      (error "ChatGPT API key not configured. Please set chat-gpt-api-key")
    t))

(defun chat-gpt--prepare-headers ()
  "Prepare HTTP headers for API request."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Bearer " chat-gpt-api-key))))

(defun chat-gpt--prepare-payload (message)
  "Prepare JSON payload for API request with MESSAGE."
  (let ((messages (append
                   (list `((role . "system") (content . ,chat-gpt-system-message)))
                   chat-gpt-conversation-history
                   (list `((role . "user") (content . ,message))))))
    (json-encode
     `((model . ,chat-gpt-model)
       (messages . ,messages)
       (temperature . ,chat-gpt-temperature)
       (max_tokens . ,chat-gpt-max-tokens)))))

(defun chat-gpt--parse-response (response)
  "Parse ChatGPT API response and extract the message content."
  (condition-case err
      (let* ((json-data (json-read-from-string response))
             (choices (cdr (assoc 'choices json-data)))
             (first-choice (aref choices 0))
             (message (cdr (assoc 'message first-choice)))
             (content (cdr (assoc 'content message))))
        content)
    (error
     (format "Error parsing response: %s" (error-message-string err)))))

;;; API Communication

(defun chat-gpt--make-request (message callback)
  "Make API request with MESSAGE and call CALLBACK with response."
  (chat-gpt--check-api-key)
  (let ((url-request-method "POST")
        (url-request-extra-headers (chat-gpt--prepare-headers))
        (url-request-data (chat-gpt--prepare-payload message)))
    (url-retrieve
     chat-gpt-api-url
     (lambda (status)
       (condition-case err
           (progn
             (goto-char (point-min))
             (re-search-forward "^$" nil t)
             (let ((response-body (buffer-substring-no-properties (point) (point-max))))
               (kill-buffer)
               (funcall callback (chat-gpt--parse-response response-body))))
         (error
          (kill-buffer)
          (funcall callback (format "Error: %s" (error-message-string err)))))))))

;;; Buffer Management

(defun chat-gpt--get-or-create-buffer ()
  "Get or create the ChatGPT conversation buffer."
  (let ((buffer (get-buffer chat-gpt-buffer-name)))
    (unless buffer
      (setq buffer (generate-new-buffer chat-gpt-buffer-name))
      (with-current-buffer buffer
        (setq-local chat-gpt-conversation-history '())
        (insert "ChatGPT Conversation\n")
        (insert "==================\n\n")
        (insert "Type your message and press C-c C-s to send.\n")
        (insert "Use C-c C-c to clear conversation history.\n\n")))
    buffer))

(defun chat-gpt--insert-message (buffer role message)
  "Insert MESSAGE from ROLE into BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (let ((timestamp (format-time-string "%H:%M:%S")))
      (insert (format "[%s] %s: %s\n\n" 
                      timestamp
                      (upcase (symbol-name role))
                      message)))
    (goto-char (point-max))))

(defun chat-gpt--update-conversation-history (role message)
  "Update conversation history with ROLE and MESSAGE."
  (setq chat-gpt-conversation-history
        (append chat-gpt-conversation-history
                (list `((role . ,(symbol-name role)) (content . ,message))))))

;;; Interactive Functions

;;;###autoload
(defun chat-gpt-send-message (message)
  "Send MESSAGE to ChatGPT and display response."
  (interactive "sMessage: ")
  (when (string-empty-p message)
    (error "Message cannot be empty"))
  
  (let ((buffer (chat-gpt--get-or-create-buffer)))
    (pop-to-buffer buffer)
    
    ;; Insert user message
    (chat-gpt--insert-message buffer 'user message)
    (chat-gpt--update-conversation-history 'user message)
    
    ;; Insert loading indicator
    (chat-gpt--insert-message buffer 'assistant "Thinking...")
    
    ;; Make API request
    (chat-gpt--make-request
     message
     (lambda (response)
       (with-current-buffer buffer
         ;; Remove loading indicator
         (goto-char (point-max))
         (forward-line -2)
         (let ((start (point)))
           (forward-line 2)
           (delete-region start (point)))
         
         ;; Insert actual response
         (chat-gpt--insert-message buffer 'assistant response)
         (chat-gpt--update-conversation-history 'assistant response))))))

;;;###autoload
(defun chat-gpt-send-region ()
  "Send selected region to ChatGPT."
  (interactive)
  (unless (region-active-p)
    (error "No region selected"))
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (chat-gpt-send-message text)))

;;;###autoload
(defun chat-gpt-send-buffer ()
  "Send entire buffer content to ChatGPT."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (chat-gpt-send-message text)))

;;;###autoload
(defun chat-gpt-clear-conversation ()
  "Clear the conversation history."
  (interactive)
  (setq chat-gpt-conversation-history '())
  (let ((buffer (get-buffer chat-gpt-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)
        (insert "ChatGPT Conversation\n")
        (insert "==================\n\n")
        (insert "Conversation cleared. Type your message and press C-c C-s to send.\n\n"))))
  (message "ChatGPT conversation history cleared"))

;;;###autoload
(defun chat-gpt-set-api-key (api-key)
  "Set the OpenAI API key to API-KEY."
  (interactive "sEnter your OpenAI API key: ")
  (setq chat-gpt-api-key api-key)
  (message "ChatGPT API key configured"))

;;;###autoload
(defun chat-gpt-set-model (model)
  "Set the ChatGPT model to MODEL."
  (interactive
   (list (completing-read "Choose model: "
                          '("gpt-3.5-turbo" "gpt-4" "gpt-4-turbo-preview" "gpt-4o")
                          nil t)))
  (setq chat-gpt-model model)
  (message "ChatGPT model set to: %s" model))

;;; Key Bindings

(defvar chat-gpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'chat-gpt-send-message)
    (define-key map (kbd "C-c C-r") 'chat-gpt-send-region)
    (define-key map (kbd "C-c C-b") 'chat-gpt-send-buffer)
    (define-key map (kbd "C-c C-c") 'chat-gpt-clear-conversation)
    map)
  "Keymap for ChatGPT mode.")

;;;###autoload
(define-minor-mode chat-gpt-mode
  "Minor mode for ChatGPT integration."
  :init-value nil
  :lighter " ChatGPT"
  :keymap chat-gpt-mode-map
  :group 'chat-gpt)

;;;###autoload
(defun chat-gpt-enable ()
  "Enable ChatGPT mode globally."
  (interactive)
  (global-set-key (kbd "C-c g m") 'chat-gpt-send-message)
  (global-set-key (kbd "C-c g r") 'chat-gpt-send-region)
  (global-set-key (kbd "C-c g b") 'chat-gpt-send-buffer)
  (global-set-key (kbd "C-c g c") 'chat-gpt-clear-conversation)
  (global-set-key (kbd "C-c g k") 'chat-gpt-set-api-key)
  (global-set-key (kbd "C-c g o") 'chat-gpt-set-model)
  (message "ChatGPT global key bindings enabled"))

(provide 'chat-gpt)

;;; chat-gpt.el ends here