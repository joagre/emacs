;;; keybindings.el --- Custom Key Bindings
;;; Commentary:
;;; Custom keybindings and shortcuts for improved productivity
;;; Code:

;; Better window navigation
(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window -1)))

;; Quick file operations
(global-set-key (kbd "C-c f") 'find-file)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)

;; Text editing enhancements
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Quick access to configuration
(global-set-key (kbd "C-c e") 
                (lambda () (interactive) 
                  (find-file (expand-file-name "init.el" user-emacs-directory))))

(provide 'keybindings)
;;; keybindings.el ends here