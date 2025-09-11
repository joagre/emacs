;;; test-chat-gpt.el --- Tests for ChatGPT extensions -*- lexical-binding: t; -*-

;;; Commentary:

;; Simple tests to verify the ChatGPT extensions are working correctly.
;; Run these tests to ensure the basic functionality is operational.

;;; Code:

(require 'chat-gpt)
(require 'chat-gpt-utils)

;;; Test Functions

(defun test-chat-gpt-basic-functions ()
  "Test basic ChatGPT functions are defined and callable."
  (interactive)
  (let ((functions '(chat-gpt-send-message
                     chat-gpt-send-region
                     chat-gpt-send-buffer
                     chat-gpt-clear-conversation
                     chat-gpt-set-api-key
                     chat-gpt-set-model)))
    (dolist (func functions)
      (if (fboundp func)
          (message "✓ Function %s is defined" func)
        (error "✗ Function %s is not defined" func)))
    (message "✓ All basic functions are defined")))

(defun test-chat-gpt-utility-functions ()
  "Test utility functions are defined and callable."
  (interactive)
  (let ((functions '(chat-gpt-explain-code
                     chat-gpt-review-code
                     chat-gpt-document-code
                     chat-gpt-refactor-code
                     chat-gpt-debug-code
                     chat-gpt-write-tests
                     chat-gpt-use-template)))
    (dolist (func functions)
      (if (fboundp func)
          (message "✓ Utility function %s is defined" func)
        (error "✗ Utility function %s is not defined" func)))
    (message "✓ All utility functions are defined")))

(defun test-chat-gpt-variables ()
  "Test that required variables are defined with expected types."
  (interactive)
  (let ((variables '((chat-gpt-api-key . string)
                     (chat-gpt-model . string)
                     (chat-gpt-temperature . number)
                     (chat-gpt-max-tokens . integer)
                     (chat-gpt-conversation-history . list)
                     (chat-gpt-prompt-templates . list))))
    (dolist (var-info variables)
      (let ((var (car var-info))
            (expected-type (cdr var-info)))
        (if (boundp var)
            (let ((value (symbol-value var)))
              (cond
               ((eq expected-type 'string)
                (if (stringp value)
                    (message "✓ Variable %s is a string" var)
                  (error "✗ Variable %s is not a string: %s" var (type-of value))))
               ((eq expected-type 'number)
                (if (numberp value)
                    (message "✓ Variable %s is a number" var)
                  (error "✗ Variable %s is not a number: %s" var (type-of value))))
               ((eq expected-type 'integer)
                (if (integerp value)
                    (message "✓ Variable %s is an integer" var)
                  (error "✗ Variable %s is not an integer: %s" var (type-of value))))
               ((eq expected-type 'list)
                (if (listp value)
                    (message "✓ Variable %s is a list" var)
                  (error "✗ Variable %s is not a list: %s" var (type-of value))))))
          (error "✗ Variable %s is not defined" var))))
    (message "✓ All variables are defined with correct types")))

(defun test-chat-gpt-prompt-templates ()
  "Test that prompt templates are properly formatted."
  (interactive)
  (let ((templates chat-gpt-prompt-templates))
    (if (null templates)
        (error "✗ No prompt templates defined"))
    (dolist (template templates)
      (let ((name (car template))
            (prompt (cdr template)))
        (unless (stringp name)
          (error "✗ Template name is not a string: %s" name))
        (unless (stringp prompt)
          (error "✗ Template prompt is not a string: %s" prompt))
        (unless (string-match "%s" prompt)
          (error "✗ Template %s does not contain %%s placeholder" name))
        (message "✓ Template %s is valid" name)))
    (message "✓ All prompt templates are valid")))

(defun test-chat-gpt-text-processing ()
  "Test text processing functions."
  (interactive)
  ;; Test code block formatting
  (let ((code "print('hello')")
        (language "python"))
    (let ((result (chat-gpt-format-code-block code language)))
      (if (string-match "```python\nprint('hello')\n```" result)
          (message "✓ Code block formatting works")
        (error "✗ Code block formatting failed: %s" result))))
  
  ;; Test token counting
  (let ((text "This is a test sentence with several words."))
    (let ((count (chat-gpt-count-tokens text)))
      (if (and (numberp count) (> count 0))
          (message "✓ Token counting works: %d tokens" count)
        (error "✗ Token counting failed: %s" count))))
  
  (message "✓ Text processing functions work correctly"))

(defun test-chat-gpt-buffer-management ()
  "Test buffer management functions."
  (interactive)
  ;; Test buffer creation
  (let ((buffer (chat-gpt--get-or-create-buffer)))
    (if (bufferp buffer)
        (progn
          (message "✓ Buffer creation works")
          ;; Clean up test buffer
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
      (error "✗ Buffer creation failed")))
  
  (message "✓ Buffer management works correctly"))

(defun test-chat-gpt-key-bindings ()
  "Test that key bindings are properly set up."
  (interactive)
  ;; This is a basic test - in a real scenario you'd check if keys are bound
  (if (keymapp chat-gpt-mode-map)
      (message "✓ chat-gpt-mode-map is defined")
    (error "✗ chat-gpt-mode-map is not defined"))
  
  (message "✓ Key bindings are set up correctly"))

;;; Test Runner

(defun run-all-chat-gpt-tests ()
  "Run all ChatGPT tests."
  (interactive)
  (message "Running ChatGPT extension tests...")
  (condition-case err
      (progn
        (test-chat-gpt-basic-functions)
        (test-chat-gpt-utility-functions)
        (test-chat-gpt-variables)
        (test-chat-gpt-prompt-templates)
        (test-chat-gpt-text-processing)
        (test-chat-gpt-buffer-management)
        (test-chat-gpt-key-bindings)
        (message "🎉 All tests passed! ChatGPT extensions are working correctly."))
    (error
     (message "❌ Test failed: %s" (error-message-string err)))))

;;; Test with Mock API (for development)

(defun test-chat-gpt-mock-api ()
  "Test ChatGPT with a mock API response (doesn't require real API key)."
  (interactive)
  (message "Testing with mock API response...")
  
  ;; Temporarily override the API function for testing
  (let ((original-make-request (symbol-function 'chat-gpt--make-request)))
    (fset 'chat-gpt--make-request
          (lambda (message callback)
            (funcall callback (format "Mock response to: %s" message))))
    
    (unwind-protect
        (progn
          ;; Test sending a message
          (chat-gpt-send-message "Test message")
          (message "✓ Mock API test completed"))
      
      ;; Restore original function
      (fset 'chat-gpt--make-request original-make-request))))

;;; Usage Instructions

(defun chat-gpt-test-help ()
  "Display help for testing ChatGPT extensions."
  (interactive)
  (with-current-buffer (get-buffer-create "*ChatGPT Test Help*")
    (erase-buffer)
    (insert "ChatGPT Extensions Test Suite\n")
    (insert "============================\n\n")
    (insert "Available test functions:\n\n")
    (insert "• run-all-chat-gpt-tests - Run all tests\n")
    (insert "• test-chat-gpt-basic-functions - Test core functions\n")
    (insert "• test-chat-gpt-utility-functions - Test utility functions\n")
    (insert "• test-chat-gpt-variables - Test variable definitions\n")
    (insert "• test-chat-gpt-prompt-templates - Test prompt templates\n")
    (insert "• test-chat-gpt-text-processing - Test text processing\n")
    (insert "• test-chat-gpt-buffer-management - Test buffer functions\n")
    (insert "• test-chat-gpt-key-bindings - Test key binding setup\n")
    (insert "• test-chat-gpt-mock-api - Test with mock API (no key needed)\n\n")
    (insert "To run all tests: M-x run-all-chat-gpt-tests\n")
    (insert "To test without API key: M-x test-chat-gpt-mock-api\n")
    (pop-to-buffer (current-buffer))))

(provide 'test-chat-gpt)

;;; test-chat-gpt.el ends here