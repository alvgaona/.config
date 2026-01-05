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

;; Modeline - Place line/column numbers on the right
;; Hide mode name, only show process when it exists
(setq-default mode-line-modes nil)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                "   "
                mode-line-modes
                mode-line-misc-info
                (:eval (propertize " " 'display '(space :align-to (- right 10))))
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))
                mode-line-end-spaces))

;; Theme
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(provide 'ui)
;;; ui.el ends here
