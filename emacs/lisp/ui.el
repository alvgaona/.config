;;; ui.el --- User interface configuration -*- lexical-binding: t -*-

;; User Interface
(tool-bar-mode -1)   ; Hide toolbar
(menu-bar-mode -1)   ; Hide menu bar
(scroll-bar-mode -1) ; Hide scrollbar, use keyboard to navigate
(setq inhibit-startup-screen t) ; Skip welcome screen
(setq ring-bell-function 'ignore) ; No beeping

;; Echo
(setq echo-keystrokes 0.1) ; Echo keystrokes with configured delay (in seconds)

;; Font
(set-face-attribute 'default nil
                    :family "Lilex Nerd Font"
                    :height 160
                    :weight 'light)

;; Title bar
(setq ns-use-proxy-icon nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;;(add-to-list 'default-frame-alist '(undecorated-rounded . t))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Modeline (lualine-style) - Pure elisp
;; Single modeline: only the active window shows the modeline

;; Modeline inherits font from default face + scales with zoom
(set-face-attribute 'mode-line nil :inherit 'default :box '(:line-width 4 :style flat-button))
(set-face-attribute 'mode-line-inactive nil :inherit 'default :box '(:line-width 4 :style flat-button))

;; Hide modeline on inactive windows using window-local mode-line-format
(defvar my-modeline-format nil
  "Storage for the actual modeline format.")

(defvar my-modeline-hidden-modes '(compilation-mode
                                   magit-status-mode
                                   magit-log-mode
                                   magit-diff-mode
                                   magit-revision-mode)
  "Major modes where modeline should never be shown.")

(defun my-modeline-update-windows ()
  "Show modeline only on the selected window, unless in a hidden mode."
  (let ((current (selected-window)))
    (dolist (window (window-list))
      (let ((dominated (with-current-buffer (window-buffer window)
                         (memq major-mode my-modeline-hidden-modes))))
        (set-window-parameter window 'mode-line-format
                              (cond
                               (dominated 'none)
                               ((eq window current) my-modeline-format)
                               (t 'none))))))
  (force-mode-line-update t))

;; Update modeline on window focus change (Emacs 27+)
(add-hook 'window-selection-change-functions
          (lambda (_frame) (my-modeline-update-windows)))

;; Update modeline when window configuration changes (split/delete/create)
(add-hook 'window-configuration-change-hook #'my-modeline-update-windows)

(defun my-modeline-scale-with-zoom ()
  "Scale modeline with text-scale zoom."
  (let* ((scale (expt text-scale-mode-step text-scale-mode-amount))
         (height (round (* (face-attribute 'default :height) scale))))
    (set-face-attribute 'mode-line nil :height height)
    (set-face-attribute 'mode-line-inactive nil :height height)))

(add-hook 'text-scale-mode-hook #'my-modeline-scale-with-zoom)

;; Modeline faces (lualine-style colors)
(defface my-modeline-section-a
  '((t :background "#96a6c8" :foreground "#181818" :weight normal))
  "Face for section A (outer left/right).")

(defface my-modeline-section-b
  '((t :background "#453d41" :foreground "#e4e4e4" :weight normal))
  "Face for section B (middle).")

(defface my-modeline-section-c
  '((t :background "#282828" :foreground "#95a99f" :weight normal))
  "Face for section C (inner).")

;; Separator characters (Powerline/Nerd Font)
(defvar my-sep-right "\ue0b0")
(defvar my-sep-left "\ue0b2")

;; Extensible segment lists
(defvar my-modeline-left-segments '()
  "List of functions to call for extra left segments.
Each function should return a propertized string or nil.")

(defvar my-modeline-right-segments '()
  "List of functions to call for extra right segments.
Each function should return a propertized string or nil.")

(defun my-modeline-add-segment (side fn)
  "Add a segment function FN to SIDE (:left or :right)."
  (pcase side
    (:left (add-to-list 'my-modeline-left-segments fn t))
    (:right (add-to-list 'my-modeline-right-segments fn t))))

(defun my-modeline-remove-segment (side fn)
  "Remove segment function FN from SIDE (:left or :right)."
  (pcase side
    (:left (setq my-modeline-left-segments (delete fn my-modeline-left-segments)))
    (:right (setq my-modeline-right-segments (delete fn my-modeline-right-segments)))))

(defun my-modeline-render-extra-left ()
  "Render extra left segments."
  (mapconcat (lambda (fn) (or (funcall fn) "")) my-modeline-left-segments ""))

(defun my-modeline-render-extra-right ()
  "Render extra right segments."
  (mapconcat (lambda (fn) (or (funcall fn) "")) my-modeline-right-segments ""))

;; Helper functions
(defun my-modeline-git-branch ()
  "Get current git branch name."
  (ignore-errors
    (let* ((dir default-directory)
           (root (locate-dominating-file dir ".git")))
      (when root
        (let ((head-file (expand-file-name ".git/HEAD" root)))
          (when (file-exists-p head-file)
            (with-temp-buffer
              (insert-file-contents head-file)
              (if (looking-at "ref: refs/heads/\\(.+\\)")
                  (match-string 1)
                (substring (buffer-string) 0 7)))))))))

(defun my-modeline-segment-branch ()
  "Git branch segment."
  (when-let ((branch (my-modeline-git-branch)))
    (propertize (concat " \ue0a0 " branch " ") 'face 'my-modeline-section-a)))

(defun my-modeline-segment-sep-ab ()
  "Separator after branch."
  (if (my-modeline-git-branch)
      (propertize my-sep-right 'face `(:foreground ,(face-background 'my-modeline-section-a)
                                       :background ,(face-background 'my-modeline-section-b)))
    (propertize my-sep-right 'face `(:foreground ,(face-background 'my-modeline-section-b)
                                     :background ,(face-background 'mode-line)))))

(defun my-modeline-segment-filename ()
  "Buffer name segment."
  (propertize (concat " " (buffer-name) " ") 'face 'my-modeline-section-b))

(defun my-modeline-segment-sep-b ()
  "Separator after filename."
  (propertize my-sep-right 'face `(:foreground ,(face-background 'my-modeline-section-b)
                                   :background ,(face-background 'mode-line))))

(defun my-modeline-segment-sep-c ()
  "Separator before percentage."
  (propertize my-sep-left 'face `(:foreground ,(face-background 'my-modeline-section-c)
                                  :background ,(face-background 'mode-line))))

(defun my-modeline-segment-percent ()
  "Percentage position segment."
  (let* ((current (line-number-at-pos))
         (total (line-number-at-pos (point-max)))
         (percent (if (= total 1) 0 (/ (* 100 current) total)))
         (text (cond
                ((= current 1) "Top")
                ((>= current total) "Bottom")
                (t (format "%d%%%%" percent)))))
    (propertize (format " %s " text) 'face 'my-modeline-section-c)))

(defun my-modeline-segment-sep-cb ()
  "Separator between percentage and line:col."
  (propertize my-sep-left 'face `(:foreground ,(face-background 'my-modeline-section-b)
                                  :background ,(face-background 'my-modeline-section-c))))

(defun my-modeline-segment-linecol ()
  "Line and column segment."
  (propertize (format-mode-line " %l:%c ") 'face 'my-modeline-section-b))

(defun my-modeline-segment-sep-ba ()
  "Separator between line:col and time."
  (propertize my-sep-left 'face `(:foreground ,(face-background 'my-modeline-section-a)
                                  :background ,(face-background 'my-modeline-section-b))))

(defun my-modeline-segment-time ()
  "Time segment."
  (propertize (format-time-string " %H:%M ") 'face 'my-modeline-section-a))

(defun my-modeline-segment-fill-right ()
  "Fill remaining space with section-a background."
  (propertize "   " 'face 'my-modeline-section-a))

(defun my-modeline-segment-misc-info ()
  "Misc info segment with styling."
  (let* ((info (format-mode-line mode-line-misc-info))
         (info (string-trim info))
         (info (replace-regexp-in-string "\\[\\]" "" info))
         (info (string-trim info)))
    (unless (string-blank-p info)
      (propertize (concat " " info " ") 'face 'my-modeline-section-c))))

;; Right-align helper
(defun my-modeline-right-align ()
  "Return a space that pushes content to the right."
  (let* ((right-segments (format-mode-line
                          '((:eval (my-modeline-render-extra-right))
                            (:eval (my-modeline-segment-sep-c))
                            (:eval (my-modeline-segment-percent))
                            (:eval (my-modeline-segment-sep-cb))
                            (:eval (my-modeline-segment-linecol))
                            (:eval (my-modeline-segment-sep-ba))
                            (:eval (my-modeline-segment-time)))))
         (width (string-width right-segments)))
    (propertize " " 'display `(space :align-to (- right ,width)))))

;; Build the mode-line
(setq my-modeline-format
      '((:eval (my-modeline-segment-branch))
        (:eval (my-modeline-segment-sep-ab))
        (:eval (my-modeline-segment-filename))
        (:eval (my-modeline-segment-sep-b))
        (:eval (my-modeline-render-extra-left))
        (:eval (my-modeline-segment-misc-info))
        (:eval (my-modeline-right-align))
        (:eval (my-modeline-render-extra-right))
        (:eval (my-modeline-segment-sep-c))
        (:eval (my-modeline-segment-percent))
        (:eval (my-modeline-segment-sep-cb))
        (:eval (my-modeline-segment-linecol))
        (:eval (my-modeline-segment-sep-ba))
        (:eval (my-modeline-segment-time))
        (:eval (my-modeline-segment-fill-right))))

;; Initialize modeline
(setq-default mode-line-format my-modeline-format)

;; Ensure modeline is set after init
(add-hook 'emacs-startup-hook #'my-modeline-update-windows)

;; Theme
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t)
  ;; Remove bold from common faces
  (set-face-attribute 'font-lock-keyword-face nil :weight 'normal)
  (set-face-attribute 'font-lock-builtin-face nil :weight 'normal)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'normal))

(with-eval-after-load 'dired
  (set-face-attribute 'dired-directory nil :weight 'normal))

(provide 'ui)
;;; ui.el ends here
