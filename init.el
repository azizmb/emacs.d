;;; init.el - Aziz M. Bookwala -*- lexical-binding: t; -*-

(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(tool-bar-mode scroll-bar-mode))


(setq root-dir (file-name-directory
		(or (buffer-file-name) load-file-name)))

(setq etc-dir (file-name-as-directory (concat root-dir "etc")))

(make-directory etc-dir t)


(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(require 'f)
(require 'use-package)

(setq default-directory (f-full (getenv "HOME")))

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(use-package zenburn-theme
  :config (load-theme 'zenburn :no-confirm))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :init (progn
	  (add-hook 'after-init-hook 'projectile-global-mode)
	  (setq projectile-cache-file (f-join etc-dir "projectile.cache"))
	  (setq projectile-known-projects-file (f-join etc-dir "projectile-bookmarks.eld")))
  :config (progn
	    (setq projectile-enable-caching t)
	    (setq projectile-require-project-root nil)

	    (add-to-list 'projectile-globally-ignored-files "node_modules")

	    (define-key projectile-mode-map (kbd "s-d") 'projectile-find-dir)
	    (define-key projectile-mode-map (kbd "s-f") 'projectile-find-file))
  :bind
    ("s-/" . projectile-switch-project)
)


(use-package helm
  :diminish helm-mode
  :config (progn
	    (use-package helm-config
	      :demand
	      :bind-keymap ("C-c h" . helm-command-map))

	    ;; Some custom helm bindings
	    (define-key helm-command-map (kbd "a") 'helm-apropos)
	    ;; c helm-colors
	    (define-key helm-command-map (kbd "d") 'helm-browse-project)
	    ;; e helm-etags-select
	    ;; f helm-multi-files
	    ;; h help
	    ;; i helm-semantic-or-imenu
	    ;; l helm-locate
	    ;; m helm-man-woman
	    (define-key helm-command-map (kbd "SPC") 'helm-all-mark-rings)
	    ;; r helm-regexp
	    ;; s helm-surfraw
	    ;; t helm-top

	    ;; rebind tab to run persistent action
	    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
	    ;; make TAB works in terminal
	    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
	    ;; list actions using C-z
	    (define-key helm-map (kbd "C-z")  'helm-select-action)

	    (setq
	     helm-M-x-fuzzy-match t
	     helm-buffers-fuzzy-matching t
	     helm-completion-in-region-fuzzy-match t
	     helm-ff-file-name-history-use-recentf t
	     helm-ff-search-library-in-sexp t
	     helm-imenu-fuzzy-match    t
	     helm-mode-fuzzy-match t
	     helm-scroll-amount 8
	     helm-semantic-fuzzy-match t
	     helm-split-window-in-side-p t
	     helm-recentf-fuzzy-match t
	     helm-autoresize-max-height 25
	     helm-autoresize-min-height 25
	     )
	    (helm-mode +1)
	    (helm-autoresize-mode t)

	    (use-package helm-ag
	      :config
	      (setq helm-ag-fuzzy-match t))

	    (use-package helm-projectile
	      :config
	      (helm-projectile-on)
	      (setq projectile-completion-system 'helm
		    helm-projectile-fuzzy-match t)

	      (setq projectile-switch-project-action 'projectile-vc)

	      (define-key projectile-mode-map (kbd "s-g") 'helm-projectile-ag)
	      (define-key helm-command-map (kbd "p") 'helm-projectile))

	    (use-package helm-flycheck
	      :config
	      (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

	    (use-package helm-swoop
	      :bind
	      (("C-S-s" . helm-swoop)
	       ("M-i" . helm-swoop)
	       ("M-I" . helm-swoop-back-to-last-point)
	       ("C-c M-i" . helm-multi-swoop)
	       ("C-x M-i" . helm-multi-swoop-all))
	      :config
	      (progn
		(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
		(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

	    (use-package helm-descbinds
	      :defer t
	      :bind (("C-h b" . helm-descbinds)
		     ("C-h w" . helm-descbinds)))))


(use-package magit
  :bind ("C-x g" . magit-status)
  :init (progn
	 (use-package fullframe
	    :config (fullframe projectile-vc magit-mode-quit-window)))
  :config (progn
	    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
	    (setq magit-set-upstream-on-push t)))


(use-package flycheck
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
  :config (progn
	    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
	    (setq flycheck-idle-change-delay 0.8)
	    (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)))


;; (use-package diff-hl
;;   :defer t
;;   :ensure t
;;   :config (progn
;;          (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;;          (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
;;          (add-hook 'vc-checkin-hook 'diff-hl-update)
;;          (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))


(electric-pair-mode)
(electric-indent-mode 1)
(show-paren-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(use-package golden-ratio
  :defer t
  :diminish golden-ratio-mode
  :init (golden-ratio-mode 1)
  :config (progn (setq golden-ratio-auto-scale t)
		 (setq split-width-threshold nil)
		 (setq golden-ratio-exclude-modes
		       '("ediff-mode"
			 "eshell-mode"
			 "dired-mode"))

		 ;; http://tuhdo.github.io/helm-intro.html#sec-4
		 (defun my/helm-alive-p ()
		   (if (boundp 'helm-alive-p)
		       (symbol-value 'helm-alive-p)))

		 (add-to-list 'golden-ratio-inhibit-functions 'my/helm-alive-p)))


(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode t))


(use-package expand-region
  :bind ("C-=" . er/expand-region))


(use-package keyfreq
  :init (keyfreq-mode 1)
  :config (progn
	    (setq keyfreq-excluded-commands
		  '(self-insert-command
		    previous-line
		    next-line))
	    (keyfreq-autosave-mode 1)))


(use-package uniquify
  :config (progn
	    (setq uniquify-buffer-name-style 'reverse)
	    (setq uniquify-separator " • ")
	    (setq uniquify-after-kill-buffer-p t)
	    (setq uniquify-ignore-buffers-re "^\\*")))


(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :config (progn
	    (setq undo-tree-visualizer-timestamps t)
	    (setq undo-tree-visualizer-diff t))
  :bind ("C-c u" . undo-tree-visualize))


(use-package guide-key
  :diminish guide-key-mode
  :init (guide-key-mode 1)
  :config (progn
	    (setq guide-key/guide-key-sequence '("C-x" "C-x r" "C-x 4" "C-c" "C-c p" "C-c h" "C-h"))))


(use-package drag-stuff
  :diminish drag-stuff-mode
  :init (drag-stuff-global-mode t))


(use-package company
  :defer t
  :diminish company-mode " Ⓒ"
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (progn
	    (use-package company-quickhelp
	      :init (company-quickhelp-mode t))

	    (setq company-idle-delay 0)
	    (setq company-minimum-prefix-length 1)))


(use-package emacs-lisp-mode
  :defer t
  :init (progn
    (use-package eldoc
      :diminish eldoc-mode
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)))
  :bind (("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))


(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :config (progn
	    (setq web-mode-markup-indent-offset 4)
	    (setq web-mode-css-indent-offset 4)
	    (setq web-mode-code-indent-offset 4)
	    (setq web-mode-enable-current-column-highlight t)
	    (setq web-mode-enable-current-element-highlight t)
	    (setq web-mode-enable-auto-closing t)
	    (setq web-mode-enable-auto-pairing t)
	    (setq web-mode-engines-alist '(("django"    . ".*/templates/.*\\.html\\'")))))


(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))


(use-package popwin
  :init (popwin-mode 1))


(use-package saveplace
  :init (progn
	    (setq-default save-place t)
	    (setq save-place-file (f-join etc-dir "saved-places"))))


(use-package discover
  :init (global-discover-mode 1))


(use-package git-gutter
  :defer t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode +1)
  :bind (("C-x q" . git-gutter:revert-hunk)
	 ("C-x x" . git-gutter:popup-hunk)
	 ("C-c C-s" . git-gutter:stage-hunk)
	 ("C-x p" . git-gutter:previous-hunk)
	 ("C-x n" . git-gutter:next-hunk)))


(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init (yas-global-mode 1)
  :config (progn
	    ;; Add yasnippet support for all company backends
	    ;; https://github.com/syl20bnr/spacemacs/pull/179
	    (defvar company-mode/enable-yas t
	      "Enable yasnippet for all backends.")

	    (defun company-mode/backend-with-yas (backend)
	      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
		  backend
		(append (if (consp backend) backend (list backend))
			'(:with company-yasnippet))))

	    (with-eval-after-load 'company
	      (add-to-list 'company-backends 'company-mode/backend-with-yas))))


(load-local "python")


(column-number-mode 1)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq debug-on-error t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)


(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key [remap find-file] #'helm-find-files)
(global-set-key [remap list-buffers] #'helm-mini)
(global-set-key [remap switch-to-buffer] #'helm-mini)
(global-set-key [remap yank-pop] #'helm-show-kill-ring)


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


(provide 'init)
