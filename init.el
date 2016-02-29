(require 'package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmarking)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)


(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(tool-bar-mode scroll-bar-mode))


(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn :no-confirm))


(use-package projectile
  :ensure t :pin melpa-stable
  :defer t
  :diminish projectile-mode
  :init (add-hook 'after-init-hook 'projectile-global-mode)
  :config
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)

    (add-to-list 'projectile-globally-ignored-files "node_modules")

    (define-key projectile-mode-map (kbd "s-d") 'projectile-find-dir)
    (define-key projectile-mode-map (kbd "s-f") 'projectile-find-file)
  :bind
    ("s-/" . projectile-switch-project)
)


(use-package helm
  :ensure t :pin melpa-stable
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
	     )
	    (helm-mode +1)
	    (helm-autoresize-mode t)

	    (use-package helm-ag
	      :ensure t
	      :config
	      (setq helm-ag-fuzzy-match t))

	    (use-package helm-projectile
	      :ensure t
	      :config
	      (helm-projectile-on)
	      (setq projectile-completion-system 'helm
		    helm-projectile-fuzzy-match t)

	      (setq projectile-switch-project-action 'projectile-vc)

	      (define-key projectile-mode-map (kbd "s-g") 'helm-projectile-ag)
	      (define-key helm-command-map (kbd "p") 'helm-projectile))

	    (use-package helm-flycheck
	      :ensure t
	      :config
	      (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

	    (use-package helm-swoop
	      :ensure t
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
	      :ensure t
	      :bind (("C-h b" . helm-descbinds)
		     ("C-h w" . helm-descbinds)))))


(use-package magit
  :ensure t :pin melpa-stable
  :bind ("C-x g" . magit-status)
  :config (progn
	    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
	    (setq magit-set-upstream-on-push t)))


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


(use-package dired+
  :ensure t)


(use-package git-timemachine
  :ensure t)


(use-package golden-ratio
  :defer t
  :diminish golden-ratio-mode
  :ensure t
  :init (golden-ratio-mode 1)
  :config (progn (setq golden-ratio-auto-scale t)
		 (setq split-width-threshold nil)
		 (setq golden-ratio-exclude-modes
		       '("ediff-mode"
			 "eshell-mode"
			 "dired-mode"))))


(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode t))


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-excluded-commands
		  '(self-insert-command
		    previous-line
		    next-line))
	    (keyfreq-mode 1)
	    (keyfreq-autosave-mode 1))


(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))


(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t))
  :bind ("C-c u" . undo-tree-visualize))


(use-package guide-key
  :diminish guide-key-mode
  :ensure t
  :config
  (progn
  (setq guide-key/guide-key-sequence '("C-x" "C-x r" "C-x 4" "C-c" "C-c p" "C-c h" "C-h"))
  (guide-key-mode 1)))


(use-package drag-stuff
  :ensure t
  :config (drag-stuff-mode t))


(use-package company
  :ensure t
  :defer t
  :diminish company-mode " Ⓒ"
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (progn
	    (use-package company-quickhelp
	      :ensure t
	      :init (company-quickhelp-mode t))

	    (setq company-idle-delay 1)
	    (setq company-minimum-prefix-length 1)

	    ;; https://github.com/glucas/emacs.d/blob/fd605ca55435be4c97a43286df5f0b7257097ab5/init.el#L959
	    ;; Integrate with indent-for-tab-command
	    (progn
	      (defvar completion-at-point-functions-saved nil)

	      (defun company-indent-for-tab-command (&optional arg)
		(interactive "P")
		(let ((completion-at-point-functions-saved completion-at-point-functions)
		      (completion-at-point-functions '(company-complete-common-wrapper)))
		  (indent-for-tab-command arg)))

	      (defun company-complete-common-wrapper ()
		(let ((completion-at-point-functions completion-at-point-functions-saved))
		  (company-complete-common)))

	      (bind-key [remap indent-for-tab-command] #'company-indent-for-tab-command company-mode-map))
	    ))


(use-package emacs-lisp-mode
  :defer t
  :init
  (progn
    (use-package eldoc
      :diminish eldoc-mode
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)))
  :bind (("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))


;; (require 'init-yasnippet)
(require 'init-python-mode)


(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq debug-on-error t)
(delete-selection-mode t)

(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key [remap switch-to-buffer] #'helm-mini)
(global-set-key [remap find-file] #'helm-find-files)
(global-set-key [remap list-buffers] #'helm-mini)
(global-set-key [remap yank-pop] #'helm-show-kill-ring)

(setq display-time-24hr-format t display-time-day-and-date nil)
(display-time)


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


(add-hook 'after-init-hook 'server-start t)


(add-hook 'after-init-hook
	  (lambda ()
	    (message "init completed in %.2fms"
		     (my/time-subtract-millis after-init-time before-init-time))))


(provide 'init)
