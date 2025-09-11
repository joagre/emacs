;;; chat-gpt-utils.el --- Utility functions for ChatGPT integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;;; Commentary:

;; Additional utility functions for ChatGPT integration.
;; This file provides helper functions for text processing,
;; prompt templates, and advanced features.

;;; Code:

(require 'chat-gpt)

;;; Prompt Templates

(defcustom chat-gpt-prompt-templates
  '(("explain" . "Please explain the following code:\n\n%s")
    ("review" . "Please review this code and suggest improvements:\n\n%s")
    ("document" . "Please add documentation comments to this code:\n\n%s")
    ("refactor" . "Please refactor this code to make it cleaner and more efficient:\n\n%s")
    ("debug" . "Help me debug this code. What might be wrong?\n\n%s")
    ("optimize" . "How can I optimize this code for better performance?\n\n%s")
    ("translate" . "Please translate this code to another programming language:\n\n%s")
    ("test" . "Please write unit tests for this code:\n\n%s"))
  "Predefined prompt templates for common tasks."
  :type '(alist :key-type string :value-type string)
  :group 'chat-gpt)

;;; Text Processing

(defun chat-gpt-format-code-block (text &optional language)
  "Format TEXT as a code block with optional LANGUAGE specification."
  (format "```%s\n%s\n```" (or language "") text))

(defun chat-gpt-extract-code-blocks (text)
  "Extract code blocks from ChatGPT response TEXT."
  (let ((blocks '())
        (start 0))
    (while (string-match "```\\([^\n]*\\)\n\\(\\(?:.\\|\n\\)*?\\)```" text start)
      (let ((language (match-string 1 text))
            (code (match-string 2 text)))
        (push (cons language code) blocks)
        (setq start (match-end 0))))
    (nreverse blocks)))

(defun chat-gpt-count-tokens (text)
  "Rough estimation of token count for TEXT.
This is a simple approximation: tokens ≈ words * 1.3"
  (let ((word-count (length (split-string text))))
    (ceiling (* word-count 1.3))))

;;; Template Functions

;;;###autoload
(defun chat-gpt-explain-code ()
  "Send selected code to ChatGPT for explanation."
  (interactive)
  (unless (region-active-p)
    (error "No region selected"))
  (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format (cdr (assoc "explain" chat-gpt-prompt-templates)) code)))
    (chat-gpt-send-message prompt)))

;;;###autoload
(defun chat-gpt-review-code ()
  "Send selected code to ChatGPT for review."
  (interactive)
  (unless (region-active-p)
    (error "No region selected"))
  (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format (cdr (assoc "review" chat-gpt-prompt-templates)) code)))
    (chat-gpt-send-message prompt)))

;;;###autoload
(defun chat-gpt-document-code ()
  "Send selected code to ChatGPT for documentation."
  (interactive)
  (unless (region-active-p)
    (error "No region selected"))
  (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format (cdr (assoc "document" chat-gpt-prompt-templates)) code)))
    (chat-gpt-send-message prompt)))

;;;###autoload
(defun chat-gpt-refactor-code ()
  "Send selected code to ChatGPT for refactoring suggestions."
  (interactive)
  (unless (region-active-p)
    (error "No region selected"))
  (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format (cdr (assoc "refactor" chat-gpt-prompt-templates)) code)))
    (chat-gpt-send-message prompt)))

;;;###autoload
(defun chat-gpt-debug-code ()
  "Send selected code to ChatGPT for debugging help."
  (interactive)
  (unless (region-active-p)
    (error "No region selected"))
  (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format (cdr (assoc "debug" chat-gpt-prompt-templates)) code)))
    (chat-gpt-send-message prompt)))

;;;###autoload
(defun chat-gpt-write-tests ()
  "Send selected code to ChatGPT for test generation."
  (interactive)
  (unless (region-active-p)
    (error "No region selected"))
  (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format (cdr (assoc "test" chat-gpt-prompt-templates)) code)))
    (chat-gpt-send-message prompt)))

;;;###autoload
(defun chat-gpt-use-template (template-name)
  "Use a predefined template with selected text."
  (interactive
   (list (completing-read "Choose template: "
                          (mapcar 'car chat-gpt-prompt-templates)
                          nil t)))
  (unless (region-active-p)
    (error "No region selected"))
  (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
         (template (cdr (assoc template-name chat-gpt-prompt-templates)))
         (prompt (format template code)))
    (chat-gpt-send-message prompt)))

;;; Advanced Features

;;;###autoload
(defun chat-gpt-summarize-buffer ()
  "Summarize the current buffer content."
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (format "Please provide a concise summary of the following content:\n\n%s" content)))
    (chat-gpt-send-message prompt)))

;;;###autoload
(defun chat-gpt-improve-writing ()
  "Improve writing style of selected text."
  (interactive)
  (unless (region-active-p)
    (error "No region selected"))
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format "Please improve the writing style and clarity of this text:\n\n%s" text)))
    (chat-gpt-send-message prompt)))

;;;###autoload
(defun chat-gpt-translate-text (target-language)
  "Translate selected text to TARGET-LANGUAGE."
  (interactive "sTranslate to language: ")
  (unless (region-active-p)
    (error "No region selected"))
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format "Please translate the following text to %s:\n\n%s" target-language text)))
    (chat-gpt-send-message prompt)))

;;; Conversation Management

(defvar chat-gpt-saved-conversations '()
  "List of saved conversation histories.")

;;;###autoload
(defun chat-gpt-save-conversation (name)
  "Save current conversation with NAME."
  (interactive "sConversation name: ")
  (let ((conversation (copy-sequence chat-gpt-conversation-history)))
    (setq chat-gpt-saved-conversations
          (cons (cons name conversation) chat-gpt-saved-conversations))
    (message "Conversation saved as: %s" name)))

;;;###autoload
(defun chat-gpt-load-conversation (name)
  "Load saved conversation by NAME."
  (interactive
   (list (completing-read "Load conversation: "
                          (mapcar 'car chat-gpt-saved-conversations)
                          nil t)))
  (let ((conversation (cdr (assoc name chat-gpt-saved-conversations))))
    (if conversation
        (progn
          (setq chat-gpt-conversation-history conversation)
          (message "Conversation loaded: %s" name))
      (error "Conversation not found: %s" name))))

;;;###autoload
(defun chat-gpt-list-conversations ()
  "List all saved conversations."
  (interactive)
  (if chat-gpt-saved-conversations
      (message "Saved conversations: %s"
               (mapconcat 'car chat-gpt-saved-conversations ", "))
    (message "No saved conversations")))

;;; Export Functions

;;;###autoload
(defun chat-gpt-export-conversation (filename)
  "Export current conversation to FILENAME."
  (interactive "FExport conversation to file: ")
  (let ((buffer (get-buffer chat-gpt-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (write-region (point-min) (point-max) filename)
          (message "Conversation exported to: %s" filename))
      (error "No ChatGPT conversation buffer found"))))

;;; Additional Key Bindings

;;;###autoload
(defun chat-gpt-utils-enable ()
  "Enable ChatGPT utilities key bindings."
  (interactive)
  (global-set-key (kbd "C-c g e") 'chat-gpt-explain-code)
  (global-set-key (kbd "C-c g v") 'chat-gpt-review-code)
  (global-set-key (kbd "C-c g d") 'chat-gpt-document-code)
  (global-set-key (kbd "C-c g f") 'chat-gpt-refactor-code)
  (global-set-key (kbd "C-c g x") 'chat-gpt-debug-code)
  (global-set-key (kbd "C-c g t") 'chat-gpt-write-tests)
  (global-set-key (kbd "C-c g u") 'chat-gpt-use-template)
  (global-set-key (kbd "C-c g s") 'chat-gpt-summarize-buffer)
  (global-set-key (kbd "C-c g i") 'chat-gpt-improve-writing)
  (global-set-key (kbd "C-c g l") 'chat-gpt-translate-text)
  (global-set-key (kbd "C-c g S") 'chat-gpt-save-conversation)
  (global-set-key (kbd "C-c g L") 'chat-gpt-load-conversation)
  (global-set-key (kbd "C-c g E") 'chat-gpt-export-conversation)
  (message "ChatGPT utilities key bindings enabled"))

(provide 'chat-gpt-utils)

;;; chat-gpt-utils.el ends here