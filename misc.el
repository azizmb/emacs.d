(electric-pair-mode)
(electric-indent-mode 1)
(show-paren-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(column-number-mode 1)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq debug-on-error t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)

(setq display-time-24hr-format t display-time-day-and-date nil)
(display-time)


;; https://github.com/ghoseb/dotemacs/blob/f72cd04aa09f82551c217f85873a7cc3506fec15/misc.el#L14
(defun th-activate-mark-init ()
  (setq cursor-type 'bar))

(add-hook 'activate-mark-hook 'th-activate-mark-init)

(defun th-deactivate-mark-init ()
  (setq cursor-type 'box))

(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)


;; http://stackoverflow.com/a/27749009/285614
(global-set-key (kbd "<f5>") 'set-selective-display-dlw)

(defun set-selective-display-dlw (&optional level)
"Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))


;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
		'my/smarter-move-beginning-of-line)


;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline21
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
	    (line-beginning-position 2)))))


(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


(add-hook 'after-init-hook 'server-start t)
