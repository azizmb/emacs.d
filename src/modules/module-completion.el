;; Auto completion
(use-package company-anaconda
  :after (anaconda-mode company)
  :commands company-anaconda
  :config (add-to-list 'company-backends 'company-anaconda))


(use-package company
  :bind (("<C-tab>" . company-complete))
  :diminish company-mode " Ⓒ"
  :init
  (setq company-idle-delay 0.5
    company-show-numbers t
    company-tooltip-limit 10
    company-minimum-prefix-length 1
    company-tooltip-flip-when-above t)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-emoji
    :init
    (add-to-list 'company-backends 'company-emoji))
  (use-package company-web-html
    :ensure company-web
    :init
    (add-to-list 'company-backends 'company-web-html))
  (use-package company-quickhelp
    :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))
  (use-package company-statistics
    :init (add-hook 'global-company-mode-hook #'company-statistics-mode))
  (company-tng-configure-default)
  (global-company-mode 1))


;; Helm
(use-package helm
  :diminish helm-mode
  :config
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
  )

  
(use-package helm-ag
  :after helm
  :config
  (setq helm-ag-fuzzy-match t))


(use-package helm-projectile
  :after helm
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm
	helm-projectile-fuzzy-match t)
  
  (define-key projectile-mode-map (kbd "s-g") 'helm-projectile-ag)
  (define-key helm-command-map (kbd "p") 'helm-projectile))


(use-package helm-flycheck
  :after helm
  :config
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))


(use-package helm-descbinds
  :after helm
  :bind (("C-h b" . helm-descbinds)
	 ("C-h w" . helm-descbinds)))


(use-package helm-swoop
  :after helm
  :bind
  (("C-s" . helm-swoop)
   ("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))
  :config
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))


;; YaSnippet
(use-package yasnippet
  :ensure t
  :after company
  :config
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t
  :after yasnipet
  )

(use-package auto-yasnippet
  :ensure t
  :after yasnippet
  :commands
  (aya-create
   aya-expand
   aya-open-line)
  )

;; ;; Add yasnippet support for all company backends
;; ;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; ;; FIXME
;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas)
;;         (and (listp backend) (member 'company-yasnippet backend)))
;;     backend
;;     (append (if (consp backend) backend (list backend))
;;       '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(provide 'module-completion)
