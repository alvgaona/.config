;;; editor.el --- Editor configuration -*- lexical-binding: t -*-

;; Code editor
(setq display-line-numbers-type 'relative) ; Relative line numbers
(global-display-line-numbers-mode 1)
(add-hook 'xwidget-webkit-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq header-line-format nil)
            (setq mode-line-format nil)))
(column-number-mode 1) ; Show current column number
(show-paren-mode 1) ; Highlight matching parenthesis

;; Editing capabilities
(electric-pair-mode 1)
(delete-selection-mode 1) ; Typing deletes selection
(setq-default indent-tabs-mode nil) ; Indent with spaces, not tabs
(setq-default tab-width 4) ; 4-space indentation
(setq-default standard-indent 4) ; Default indent for all modes
(setq-default truncate-lines t) ; No line wrapping

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

;; Other
(setq vc-follow-symlinks t) ; Follow symlinks without prompting

(provide 'editor)
;;; editor.el ends here
