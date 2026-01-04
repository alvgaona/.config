;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Initialize package system
(require 'package)

;; Add MELPA repository (the largest Emacs package archive)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Initialize packages
(package-initialize)

;; Refresh package list if it doesn't exist
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not present (makes package config cleaner)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ; Auto-install packages

;; User Interface
(tool-bar-mode -1)   ; Hide toolbar
(menu-bar-mode -1)   ; Hide menu bar
(scroll-bar-mode -1) ; Hide scrollbar, use keyboard to navigate
(setq inhibit-startup-screen t) ; Skip welcome screen
(setq ring-bell-function 'ignore) ; No beeping

;; Font
(set-face-attribute 'default nil
                    :family "Lilex"
                    :height 160)

;; Title bar
(setq ns-use-proxy-icon nil)
;;(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) 
(add-to-list 'default-frame-alist '(undecorated-rounded . t))

;; Modeline
(setq mode-line-position
      '((line-number-mode
         ("%l" (column-number-mode ":%c"))))) ;; Remove scroll percentage

;; Code editor
(setq display-line-numbers-type 'relative) ; Relative line numbers
(global-display-line-numbers-mode 1)
(column-number-mode 1) ; Show current column number
(show-paren-mode 1) ; Highlight matching parenthesis

;; Editing cap
(electric-pair-mode 1)
(delete-selection-mode 1) ; Typing deletes selection
(setq-default indent-tabs-mode nil) ; Indent with spaces, not tabs
(setq-default tab-width 4) ; 4-space indentation

;; File management
(setq auto-save-default nil) ; No #autosave# files
(setq make-backup-files nil) ; No backup~ files
(setq create-lockfiles nil)  ; No .#lock files
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)  ; Auto-reload changed files
(recentf-mode 1)             ; Remember recent files

;; Scrolling
(setq scroll-margin 3)           ; Leave 3 lines visible when scrolling
(setq scroll-conservatively 101) ; Smooth scrolling

;; Cursor memory
(save-place-mode 1) ; Remember cursor position in files

;; Settings via menus
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; Theme
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

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

;; --- Python ---
(use-package python
  :ensure nil  ; Built-in, no need to download
  :config
  (setq python-indent-offset 4))

;; --- JavaScript/TypeScript ---
(use-package js
  :ensure nil  ; Built-in
  :config
  (setq js-indent-level 2))

;; --- C/C++ ---
(use-package cc-mode
  :ensure nil  ; Built-in
  :config
  (setq c-basic-offset 4)
  (setq c-default-style "linux"))

;; --- Shell/Bash ---
(use-package sh-script
  :ensure nil  ; Built-in
  :config
  (setq sh-basic-offset 2))

;; --- Markdown ---
(use-package markdown-mode)

;; ============================================================================
;; LSP (eglot)
;; ============================================================================

(use-package eglot
  :ensure nil  ; Built-in
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format))
  :hook ((python-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider))

;; ============================================================================
;; TREESITTER
;; ============================================================================

;; Grammar sources
(setq treesit-language-source-alist
      '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
        (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
        (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
        (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
        (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (toml       . ("https://github.com/ikatyang/tree-sitter-toml"))
        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))))

;; Remap traditional modes to treesit modes
(setq major-mode-remap-alist
      '((bash-mode       . bash-ts-mode)
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (css-mode        . css-ts-mode)
        (go-mode         . go-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-mode         . js-ts-mode)
        (json-mode       . json-ts-mode)
        (python-mode     . python-ts-mode)
        (rust-mode       . rust-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode       . yaml-ts-mode)))

;; ============================================================================
;; PIXI
;; ============================================================================

(use-package pixi
  :load-path "~/git/pixi.el"
  :config
  (setq pixi-auto-activate t))

(use-package ros2
  :load-path "~/git/ros2.el")

;;; init.el ends here
