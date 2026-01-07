;;; lsp.el --- LSP configuration -*- lexical-binding: t -*-

;; ============================================================
;; lsp-mode (experimental - comment out to revert to eglot)
;; ============================================================

(use-package lsp-mode
  :hook ((python-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred))
  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l f" . lsp-format-buffer))
  :custom
  ;; Performance - aggressive
  (lsp-idle-delay 0.0)
  (lsp-log-io nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  ;; Features
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  ;; Diagnostics - real-time
  (lsp-diagnostics-provider :flycheck)
  ;; Modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  ;; Completion
  (lsp-completion-provider :none)
  ;; Signature help (function params as you type)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation nil))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 0.1))

;; basedpyright for Python
(use-package lsp-pyright
  :after lsp-mode
  :custom
  (lsp-pyright-langserver-command "basedpyright"))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-position 'at-point)
  ;; Inline errors (like Neovim)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-delay 0.1))

;; ============================================================
;; eglot (original - uncomment to revert)
;; ============================================================

;; (use-package eglot
;;   :ensure nil  ; Built-in
;;   :bind (:map eglot-mode-map
;;               ("C-c l r" . eglot-rename)
;;               ("C-c l a" . eglot-code-actions)
;;               ("C-c l f" . eglot-format))
;;   :hook ((python-ts-mode . eglot-ensure)
;;          (rust-ts-mode . eglot-ensure)
;;          (go-ts-mode . eglot-ensure)
;;          (c-ts-mode . eglot-ensure)
;;          (c++-ts-mode . eglot-ensure)
;;          (js-ts-mode . eglot-ensure)
;;          (typescript-ts-mode . eglot-ensure)
;;          (tsx-ts-mode . eglot-ensure)
;;          (astro-ts-mode . eglot-ensure))
;;   :config
;;   (setq eglot-report-progress nil)
;;   (setq eglot--mode-line-format '(""))
;;   (setq flymake-mode-line-format '(""))
;;   (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
;;   (add-to-list 'eglot-server-programs
;;                `(astro-ts-mode . ("astro-ls" "--stdio"
;;                                   :initializationOptions
;;                                   (:typescript (:tsdk ,(expand-file-name "typescript/lib"
;;                                                         (string-trim (shell-command-to-string "npm root -g")))))))))

(provide 'lsp)
;;; lsp.el ends here
