(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config (progn
	    (use-package pip-requirements :ensure t)

	    (use-package virtualenvwrapper
	      :ensure t
	      :config
	      (progn
		(venv-initialize-interactive-shells)
		(venv-initialize-eshell)
		(add-hook 'python-mode-hook 'projectile-pyenv-mode-set)))

	    (use-package python-django :ensure t)

	    (use-package pony-mode
	      :ensure t
	      :config
	      (add-hook 'python-mode-hook 'pony-reload-mode))

	    (use-package anaconda-mode
	      :ensure t
	      :config (progn
			(use-package company-anaconda
			  :ensure t
			  :config
			  (with-eval-after-load 'company
			    (add-to-list 'company-backends 'company-anaconda)))
			(add-hook 'python-mode-hook 'anaconda-mode)
			(add-hook 'python-mode-hook 'anaconda-eldoc-mode)))
	    ))


(use-package django-snippets :ensure t)

(setq venv-location (concat (getenv "HOME") "/.virtualenvs/"))

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name.
Version must be already installed."
  (if (boundp 'projectile-project-name)
      (venv-workon (projectile-project-name))))
    (let* ((project-name (projectile-project-name))
	   (virtualenv-path
	    (file-truename
	     (concat venv-location project-name))))
      (when (file-directory-p virtualenv-path)
	(setq python-shell-virtualenv-path virtualenv-path)))


;; ;;----------------------------------------------------------------------------
;; ;; Insert breakpoint
;; ;;----------------------------------------------------------------------------

;; (defun python-add-breakpoint ()
;;   "Add a break point"
;;   (interactive)
;;   (newline-and-indent)
;;   (insert "import ipdb; ipdb.set_trace()")
;;   (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

;; (add-hook 'python-mode-hook (lambda ()
;;                               (define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)))

;; ;;----------------------------------------------------------------------------
;; ;; Python - fill column indicator
;; ;;----------------------------------------------------------------------------

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (set-fill-column 79)))

;; (add-hook 'python-mode-hook 'fci-mode)

(provide 'init-python-mode)
