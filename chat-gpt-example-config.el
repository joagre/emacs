;;; chat-gpt-example-config.el --- Example configuration for ChatGPT extensions -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides an example configuration for the ChatGPT extensions.
;; Copy and adapt this configuration to your .emacs or init.el file.

;;; Code:

;; Load the ChatGPT extensions
(add-to-list 'load-path "/path/to/chat-gpt-extensions")
(require 'chat-gpt)
(require 'chat-gpt-utils)

;; Basic Configuration
(setq chat-gpt-api-key "your-openai-api-key-here")  ; Replace with your actual API key
(setq chat-gpt-model "gpt-3.5-turbo")               ; or "gpt-4" for better quality
(setq chat-gpt-temperature 0.7)                     ; Creativity level (0.0-2.0)
(setq chat-gpt-max-tokens 2048)                     ; Maximum response length
(setq chat-gpt-system-message "You are a helpful programming assistant.")

;; Enable global key bindings
(chat-gpt-enable)        ; Basic ChatGPT functions
(chat-gpt-utils-enable)  ; Additional utility functions

;; Optional: Custom key bindings
;; You can override the default bindings with your own preferences
;; (global-set-key (kbd "C-c a m") 'chat-gpt-send-message)
;; (global-set-key (kbd "C-c a r") 'chat-gpt-send-region)

;; Optional: Custom templates
(add-to-list 'chat-gpt-prompt-templates
             '("security" . "Please analyze this code for security vulnerabilities:\n\n%s"))

(add-to-list 'chat-gpt-prompt-templates
             '("performance" . "How can I improve the performance of this code?\n\n%s"))

;; Optional: Hook to enable chat-gpt-mode in programming buffers
(add-hook 'prog-mode-hook 'chat-gpt-mode)

;; Optional: Custom system messages for different modes
(defun chat-gpt-set-mode-specific-system-message ()
  "Set system message based on current major mode."
  (cond
   ((derived-mode-p 'emacs-lisp-mode)
    (setq chat-gpt-system-message "You are an expert Emacs Lisp programmer."))
   ((derived-mode-p 'python-mode)
    (setq chat-gpt-system-message "You are an expert Python developer."))
   ((derived-mode-p 'javascript-mode 'js-mode)
    (setq chat-gpt-system-message "You are an expert JavaScript developer."))
   (t
    (setq chat-gpt-system-message "You are a helpful programming assistant."))))

;; Optional: Automatically set system message when switching buffers
;; (add-hook 'buffer-list-update-hook 'chat-gpt-set-mode-specific-system-message)

;; Example usage:
;; 1. Set your API key: M-x chat-gpt-set-api-key
;; 2. Start chatting: C-c g m (or M-x chat-gpt-send-message)
;; 3. Send selected code for review: C-c g v
;; 4. Explain selected code: C-c g e
;; 5. Generate documentation: C-c g d

(message "ChatGPT extensions loaded! Use C-c g m to start chatting.")

;;; chat-gpt-example-config.el ends here