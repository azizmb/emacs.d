(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(require-package 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "/home/aziz/.virtualenvs/")

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name.
Version must be already installed."
  (venv-workon (projectile-project-name))
  (let* ((project-name (projectile-project-name))
         (virtualenv-path
          (file-truename
           (concat venv-location project-name))))
    (when (file-directory-p virtualenv-path)
      (setq python-shell-virtualenv-path virtualenv-path)))
  )

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

(require-package 'anaconda-mode)
(require-package 'company-anaconda)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-anaconda))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

(require-package 'pony-mode)
(add-hook 'python-mode-hook 'pony-reload-mode)


;;----------------------------------------------------------------------------
;; Django
;;----------------------------------------------------------------------------
(require-package 'python-django)
(require-package 'django-snippets)

;;----------------------------------------------------------------------------
;; Insert breakpoint
;;----------------------------------------------------------------------------

(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(add-hook 'python-mode-hook (lambda ()
                              (define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)))

;;----------------------------------------------------------------------------
;; Python - fill column indicator
;;----------------------------------------------------------------------------

(add-hook 'python-mode-hook
          (lambda ()
            (set-fill-column 79)))

(add-hook 'python-mode-hook 'fci-mode)

(provide 'init-python-mode)
