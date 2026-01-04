;;; lsp.el --- LSP configuration with eglot -*- lexical-binding: t -*-

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
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (astro-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  (add-to-list 'eglot-server-programs
               `(astro-ts-mode . ("astro-ls" "--stdio"
                                  :initializationOptions
                                  (:typescript (:tsdk ,(expand-file-name "typescript/lib"
                                                        (string-trim (shell-command-to-string "npm root -g")))))))))

(provide 'lsp)
;;; lsp.el ends here
