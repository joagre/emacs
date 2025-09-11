;;; init.el --- Emacs Configuration Entry Point
;;; Commentary:
;;; Main Emacs configuration file - loads all custom configurations and hacks
;;; Code:

;; Add local lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Package management setup
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Load custom configurations
(when (file-exists-p (expand-file-name "config" user-emacs-directory))
  (dolist (file (directory-files (expand-file-name "config" user-emacs-directory) t "\\.el$"))
    (load file)))

;; Load custom functions and hacks
(when (file-exists-p (expand-file-name "lisp" user-emacs-directory))
  (dolist (file (directory-files (expand-file-name "lisp" user-emacs-directory) t "\\.el$"))
    (load file)))

(provide 'init)
;;; init.el ends here