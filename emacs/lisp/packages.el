;;; packages.el --- Package configuration -*- lexical-binding: t -*-

;; Dashboard
(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "")
  (setq dashboard-show-banner nil)
  (setq dashboard-init-info "")
  (setq dashboard-footer-messages '(""))
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-items '((recents . 10))))

;; Vertico (completion UI)
(use-package vertico
  :init
  (vertico-mode))

;; Orderless (fuzzy matching)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil))

;; Marginalia (annotations in minibuffer)
(use-package marginalia
  :init
  (marginalia-mode))

;; Consult (enhanced commands)
(use-package consult
  :bind (("C-s" . consult-line)           ; Search in buffer
         ("C-x b" . consult-buffer)        ; Better buffer switch
         ("M-g g" . consult-goto-line)     ; Go to line
         ("M-s r" . consult-ripgrep)))     ; Search in project

;; Consult-lsp (LSP symbol search)
(use-package consult-lsp
  :after (consult lsp-mode)
  :bind (:map lsp-mode-map
              ("M-s s" . consult-lsp-symbols)))  ; Search symbols in project

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
  :custom
  (pixi-auto-activate t)
  (pixi-modeline-show-project nil)
  (pixi-modeline-show-environment nil))

;; ROS2
(use-package ros2
  :load-path "~/git/ros2.el"
  :custom
  (ros2-modeline-show-icon nil)
  (ros2-modeline-prefix nil)
  (ros2-modeline-show-distro t)
  (ros2-modeline-show-workspace nil))

(provide 'packages)
;;; packages.el ends here
