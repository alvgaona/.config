;;; languages.el --- Language-specific configuration -*- lexical-binding: t -*-

;; Python
(use-package python
  :ensure nil  ; Built-in, no need to download
  :config
  (setq python-indent-offset 4))

;; JavaScript/TypeScript
(use-package js
  :ensure nil  ; Built-in
  :config
  (setq js-indent-level 4))

(setq typescript-ts-mode-indent-offset 4)

;; C/C++
(use-package cc-mode
  :ensure nil  ; Built-in
  :config
  (setq c-basic-offset 4)
  (setq c-default-style "linux"))

;; Shell/Bash
(use-package sh-script
  :ensure nil  ; Built-in
  :config
  (setq sh-basic-offset 4))

(provide 'languages)
;;; languages.el ends here
