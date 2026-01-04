;;; ui.el --- User interface configuration -*- lexical-binding: t -*-

;; User Interface
(tool-bar-mode -1)   ; Hide toolbar
(menu-bar-mode -1)   ; Hide menu bar
(scroll-bar-mode -1) ; Hide scrollbar, use keyboard to navigate
(setq inhibit-startup-screen t) ; Skip welcome screen
(setq ring-bell-function 'ignore) ; No beeping

;; Echo
(setq echo-keystrokes 0.1) ; Echo keystrokes with configured delay (in seconds)

;; Font
(set-face-attribute 'default nil
                    :family "Lilex"
                    :height 160)

;; Title bar
(setq ns-use-proxy-icon nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;;(add-to-list 'default-frame-alist '(undecorated-rounded . t))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Modeline
(setq mode-line-position
      '((line-number-mode
         ("%l" (column-number-mode ":%c"))))) ;; Remove scroll percentage

;; Theme
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(provide 'ui)
;;; ui.el ends here
