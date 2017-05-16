(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config (progn

	    (use-package py-isort
	      :config (setq py-isort-options '("-sl")))

	    (use-package py-autopep8
	      ;; :init (progn
	      ;;	      (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))
	      )

	    (use-package virtualenvwrapper
	      :config
	      (progn
		(venv-initialize-interactive-shells)
		(venv-initialize-eshell)

		(if (getenv "WORKON_HOME")
		    (setq venv-location (getenv "WORKON_HOME"))
		  (message "WORKON_HOME env variable not set."))

		(defun project-directory (dir)
		  (f-traverse-upwards (f-dir? (f-expand ".git" it)) dir))

		(defun project-name (buffer-name)
		  "Returns the name of the project that contains the given buffer."
		  (let ((root-dir (project-directory (file-name-directory buffer-name))))
		    (if root-dir
			(f-filename root-dir)
		      nil)))

		(defun setup-venv ()
		  "Activates the virtualenv of the current buffer."
		  (let ((project-name (if (boundp 'projectile-project-name)
					  (projectile-project-name)
					(project-name buffer-file-name))))
		    (if (and project-name (f-directory? (f-join venv-location project-name)))
			(venv-workon project-name)
		      (message "Failed to activate virtualenv")
		      (venv-deactivate))))

		(add-hook 'python-mode-hook 'setup-venv)

		(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))))

	    (use-package pony-mode
	      :config
	      (add-hook 'python-mode-hook 'pony-reload-mode))

	    (use-package anaconda-mode
	      :config (progn
			(use-package company-anaconda
			  :config
			  (with-eval-after-load 'company
			    (add-to-list 'company-backends 'company-anaconda)))
			(add-hook 'python-mode-hook 'anaconda-mode)
			(add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

	    (defun annotate-pdb ()
	      (interactive)
	      (highlight-lines-matching-regexp "import pdb")
	      (highlight-lines-matching-regexp "pdb.set_trace()"))

	    (add-hook 'python-mode-hook 'annotate-pdb)))
