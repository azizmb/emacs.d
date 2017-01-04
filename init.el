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

(load-local "misc")


(use-package zenburn-theme
  :config (load-theme 'zenburn :no-confirm))


(use-package projectile
  :ensure projectile
  :diminish projectile-mode
  :init (progn
	  (setq projectile-cache-file (f-join etc-dir "projectile.cache"))
	  (setq projectile-known-projects-file (f-join etc-dir "projectile-bookmarks.eld"))
	  (projectile-mode 1))
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
  :ensure t
  :diminish drag-stuff-mode
  :init (progn
	  (drag-stuff-global-mode t)
	  (drag-stuff-define-keys)))


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
	    (setq web-mode-engines-alist '(("django" . ".*/templates/.*\\.html\\'")))))


(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))


(use-package popwin
  :init (popwin-mode 1))


(use-package discover
  :init (global-discover-mode 1))


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


(use-package markdown-mode
  :defer t
  ;; :config
  ;; (progn
  ;;   (bind-key "M-n" 'open-line-below markdown-mode-map)
  ;;   (bind-key "M-p" 'open-line-above markdown-mode-map))
  :mode (("\\.markdown$" . markdown-mode)
	 ("\\.md$" . markdown-mode)))


(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :init (progn (add-hook 'text-mode-hook 'flyspell-mode)
	       (add-hook 'markdown-mode-hook 'flyspell-mode)
	       (add-hook 'prog-mode-hook 'flyspell-prog-mode)
	       (use-package flyspell-lazy
		 :init (flyspell-lazy-mode 1))
	       (use-package helm-flyspell
		 :config (define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)))
  :config (progn
	    ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

	    ;; if (aspell installed) { use aspell}
	    ;; else if (hunspell installed) { use hunspell }
	    ;; whatever spell checker I use, I always use English dictionary
	    ;; I prefer use aspell because:
	    ;; 1. aspell is older
	    ;; 2. looks Kevin Atkinson still get some road map for aspell:
	    ;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
	    (defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
	      "if RUN-TOGETHER is true, spell check the CamelCase words"
	      (let (args)
		(cond
		 ((string-match  "aspell$" ispell-program-name)
		  ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
		  (setq args (list "--sug-mode=ultra" "--lang=en_US"))
		  (if RUN-TOGETHER
		      (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
		 ((string-match "hunspell$" ispell-program-name)
		  (setq args nil)))
		args
		))

	    (cond
	     ((executable-find "aspell")
	      (setq ispell-program-name "aspell"))
	     ((executable-find "hunspell")
	      (setq ispell-program-name "hunspell")
	      ;; just reset dictionary to the safe one "en_US" for hunspell.
	      ;; if we need use different dictionary, we specify it in command line arguments
	      (setq ispell-local-dictionary "en_US")
	      (setq ispell-local-dictionary-alist
		    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
	     (t (setq ispell-program-name nil)))

	    ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
	    ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
	    (setq ispell-extra-args (flyspell-detect-ispell-args t))
	    ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
	    (defadvice ispell-word (around my-ispell-word activate)
	      (let ((old-ispell-extra-args ispell-extra-args))
		(ispell-kill-ispell t)
		(setq ispell-extra-args (flyspell-detect-ispell-args))
		ad-do-it
		(setq ispell-extra-args old-ispell-extra-args)
		(ispell-kill-ispell t)
		))

	    (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
	      (let ((old-ispell-extra-args ispell-extra-args))
		(ispell-kill-ispell t)
		;; use emacs original arguments
		(setq ispell-extra-args (flyspell-detect-ispell-args))
		ad-do-it
		;; restore our own ispell arguments
		(setq ispell-extra-args old-ispell-extra-args)
		(ispell-kill-ispell t)
		))
	    ))


(use-package dired
  :commands dired
  :init (progn
	  (setq dired-recursive-deletes 'top)
	  (setq dired-listing-switches "--group-directories-first -alh")
	  (setq diredp-hide-details-initially-flag nil)

	  (define-key dired-mode-map [mouse-2] 'dired-find-file)
	  (guide-key/add-local-guide-key-sequence "%"))
  :config (use-package dired+))


;; https://github.com/jcf/emacs.d/blob/master/init-packages.org#fill-column-indicator
(use-package fill-column-indicator
  :init (progn
	  (add-hook 'prog-mode-hook 'fci-mode)
	  (add-hook 'text-mode-hook 'fci-mode)
	  (setq fci-rule-column 80)

	  (defun ziz/fci-enabled-p ()
	    (and (boundp 'fci-mode) fci-mode))

	  (defvar ziz/fci-mode-suppressed nil)

	  (defadvice popup-create (before suppress-fci-mode activate)
	    "Suspend fci-mode while popups are visible"
	    (let ((fci-enabled (ziz/fci-enabled-p)))
	      (when fci-enabled
		(set (make-local-variable 'ziz/fci-mode-suppressed) fci-enabled)
		(turn-off-fci-mode))))

	  (defadvice popup-delete (after restore-fci-mode activate)
	    "Restore fci-mode when all popups have closed"
	    (when (and ziz/fci-mode-suppressed
		       (null popup-instances))
	      (setq ziz/fci-mode-suppressed nil)
	      (turn-on-fci-mode))

	    (defadvice enable-theme (after recompute-fci-face activate)
	      "Regenerate fci-mode line images after switching themes"
	      (dolist (buffer (buffer-list))
		(with-current-buffer buffer
		  (turn-on-fci-mode))))))
  :config (setq fci-rule-width 2))


(load-local "python")


(global-set-key [remap kill-buffer] #'kill-this-buffer)
(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key [remap find-file] #'helm-find-files)
(global-set-key [remap list-buffers] #'helm-mini)
(global-set-key [remap switch-to-buffer] #'helm-mini)
(global-set-key [remap yank-pop] #'helm-show-kill-ring)


(setq backup-directory-alist '(("." . "~/.emacs-backups"))) ; stop leaving backup~ turds scattered everywhere
(setq backup-by-copying t
      delete-old-versions -1
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))

(provide 'init)
