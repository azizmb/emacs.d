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

		(if (getenv "WORKON_HOME")
		    (setq venv-location (getenv "WORKON_HOME"))
		  (message "WORKON_HOME env variable not set."))

		(defun project-directory (buffer-name)
		  "Returns the root directory of the project that
		  contains the given buffer. Any directory with a .git
		  file/directory is considered to be a project root."
		  (interactive)
		  (let ((root-dir (file-name-directory buffer-name)))
		    (while (and root-dir
				(not (file-exists-p (concat root-dir ".git"))))
		      (setq root-dir
			    (if (equal root-dir "/")
				nil
			      (file-name-directory (directory-file-name root-dir)))))
		    root-dir))

		(defun project-name (buffer-name)
		  "Returns the name of the project that contains the given buffer."
		  (let ((root-dir (project-directory buffer-name)))
		    (if root-dir
			(file-name-nondirectory
			 (directory-file-name root-dir))
		      nil)))

		(defun setup-venv ()
		  "Activates the virtualenv of the current buffer."
		  (let ((project-name (if (boundp 'projectile-project-name)
					  (projectile-project-name)
					(project-name buffer-file-name))))
		    (if (and project-name (file-exists-p (concat venv-location "/" project-name)))
			(venv-workon project-name)
		      (message "Failed to activate virtualenv")
		      (venv-deactivate))))

		(add-hook 'python-mode-hook 'setup-venv)

		(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))))

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
