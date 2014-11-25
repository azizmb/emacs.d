(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

;;----------------------------------------------------------------------------
;; Python - fill column indicator
;;----------------------------------------------------------------------------

(add-hook 'python-mode-hook
          (lambda ()
            (set-fill-column 79)))

(add-hook 'python-mode-hook 'fci-mode)

(provide 'init-python-mode)
