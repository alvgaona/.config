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

(defun duplicate-line-or-region ()
  "Duplicate the current line or region."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
        (goto-char (region-end))
        (insert text))
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (end-of-line)
      (newline)
      (insert line))))

(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "TAB") 'indent-region-or-line)
(global-set-key (kbd "<backtab>") 'unindent-region-or-line)

;; Buffer navigation
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-g") 'beginning-of-buffer)
(global-set-key (kbd "s-G") 'end-of-buffer)

;; File management
(global-set-key (kbd "s-o") 'find-file)

;; Window management
(global-set-key (kbd "s-,") (lambda () (interactive) (split-window-below) (other-window 1)))
(global-set-key (kbd "s-.") (lambda () (interactive) (split-window-right) (other-window 1)))
(global-set-key (kbd "s-w") 'delete-window)

;; Command execution
(global-set-key (kbd "M-;") 'execute-extended-command)

;; Commenting
(global-set-key (kbd "s-/") 'comment-line)

;; Editing
(global-set-key (kbd "s-;") 'duplicate-line-or-region)
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "s-d") 'mc/mark-next-like-this)

;; Word selection with movement
(defun select-forward-word ()
  "Move forward by word and extend selection."
  (interactive)
  (unless (region-active-p)
    (set-mark (point)))
  (forward-word))

(defun select-backward-word ()
  "Move backward by word and extend selection."
  (interactive)
  (unless (region-active-p)
    (set-mark (point)))
  (backward-word))

(global-set-key (kbd "M-S-<right>") 'select-forward-word)
(global-set-key (kbd "M-S-<left>") 'select-backward-word)

(provide 'keybindings)
;;; keybindings.el ends here
