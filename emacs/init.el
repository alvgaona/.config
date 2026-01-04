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

;; Settings via menus
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; Load configuration modules
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'ui)
(require 'editor)
(require 'packages)
(require 'languages)
(require 'lsp)
(require 'treesitter)
(require 'keybindings)

;;; init.el ends here
