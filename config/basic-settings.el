;;; basic-settings.el --- Basic Emacs Configuration
;;; Commentary:
;;; Basic settings and preferences for a cleaner Emacs experience
;;; Code:

;; UI improvements
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Better defaults
(setq-default
 inhibit-startup-screen t
 initial-scratch-message ""
 ring-bell-function 'ignore
 use-dialog-box nil
 use-file-dialog nil
 pop-up-windows nil
 split-width-threshold 120
 custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Show line numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Better scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; Show matching parentheses
(show-paren-mode 1)

;; Enable useful minor modes
(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)

(provide 'basic-settings)
;;; basic-settings.el ends here