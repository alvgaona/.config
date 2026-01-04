;;; keybindings.el --- Custom keybindings -*- lexical-binding: t -*-

(defun indent-region-or-line ()
  "Indent the region if active, otherwise the current line."
  (interactive)
  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) tab-width)
    (indent-for-tab-command)))

(defun unindent-region-or-line ()
  "Unindent the region if active, otherwise the current line."
  (interactive)
  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) (- tab-width))
    (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width))))

(global-set-key (kbd "TAB") 'indent-region-or-line)
(global-set-key (kbd "<backtab>") 'unindent-region-or-line)

(provide 'keybindings)
;;; keybindings.el ends here
