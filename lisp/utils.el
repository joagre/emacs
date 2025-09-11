;;; utils.el --- Utility Functions and Hacks
;;; Commentary:
;;; Collection of useful utility functions and Emacs hacks
;;; Code:

(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (save-excursion
    (let ((line-text (thing-at-point 'line)))
      (end-of-line)
      (newline)
      (insert line-text))))

(defun kill-other-buffers ()
  "Kill all other buffers except the current one."
  (interactive)
  (mapc 'kill-buffer 
        (delq (current-buffer) 
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun insert-date ()
  "Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-timestamp ()
  "Insert current timestamp at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'utils)
;;; utils.el ends here