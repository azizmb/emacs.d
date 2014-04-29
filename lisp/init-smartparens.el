(require-package 'smartparens)

(defun enable-smartparens ()
  (smartparens-mode +1)
  (show-smartparens-global-mode +1)
  )

(add-hook 'prog-mode-hook 'enable-smartparens)

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(provide 'init-smartparens)
