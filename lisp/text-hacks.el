;;; text-hacks.el --- Text Manipulation Hacks
;;; Commentary:
;;; Various text manipulation utilities and hacks
;;; Code:

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun join-next-line ()
  "Join the next line to the current line."
  (interactive)
  (next-line)
  (delete-indentation))

(defun unfill-paragraph ()
  "Unfill a paragraph, joining all lines into one."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun count-words-region-or-buffer ()
  "Count words in region or buffer."
  (interactive)
  (if (use-region-p)
      (message "Region has %d words" 
               (count-words (region-beginning) (region-end)))
    (message "Buffer has %d words" 
             (count-words (point-min) (point-max)))))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq pos1 (car bds) pos2 (cdr bds))))
    
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all lower"))
         (t (put this-command 'state "init caps")))))
    
    (cond
     ((string= "all caps" (get this-command 'state))
      (upcase-region pos1 pos2)
      (put this-command 'state "all lower"))
     ((string= "all lower" (get this-command 'state))
      (downcase-region pos1 pos2)
      (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-initials-region pos1 pos2)
      (put this-command 'state "all caps")))))

(provide 'text-hacks)
;;; text-hacks.el ends here