;;; packages.el --- Package configuration -*- lexical-binding: t -*-

;; Diminish
(use-package diminish)

;; Which-key
(use-package which-key
  :diminish
  :config
  (which-key-mode 1))

;; Company
(use-package company
  :diminish
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

;; Magit
(use-package magit
  :bind (("C-c g" . magit-status)))

;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

;; Dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))     ; Jump to current file's directory
  :config
  (setq dired-listing-switches "-alh") ; Human-readable sizes
  (setq dired-dwim-target t)           ; Smart copy/move target
  (setq delete-by-moving-to-trash t)   ; Use trash instead of rm
  (advice-add 'dired-sort-set-mode-line :override #'ignore))

(use-package dired-x
  :ensure nil
  :after dired)

;; Compile
(use-package compile
  :ensure nil
  :bind (("C-c c" . compile)
         ("C-c r" . recompile))
  :config
  (setq compilation-scroll-output t)
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t))

;; Markdown
(use-package markdown-mode)

;; Ripgrep
(use-package rg
  :config
  (rg-enable-default-bindings))

;; Pixi
(use-package pixi
  :load-path "~/git/pixi.el"
  :config
  (setq pixi-auto-activate t))

;; ROS2
(use-package ros2
  :load-path "~/git/ros2.el")

(provide 'packages)
;;; packages.el ends here
