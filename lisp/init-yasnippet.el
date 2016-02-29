(use-package yasnippet
  :ensure t
  :defer t
  :config (progn
	    (yas-global-mode 1)

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
	      (add-to-list 'company-backends 'company-mode/backend-with-yas))

	    (setq yas-snippet-dirs
		  '((expand-file-name "snippets" user-emacs-directory)
		    (expand-file-name "lisp/snippets" user-emacs-directory)))))


(provide 'init-yasnippet)
