;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(dolist (hook (if (fboundp 'prog-mode)
                  '(prog-mode-hook ruby-mode-hook)
                '(find-file-hooks)))
  (add-hook hook 'goto-address-prog-mode))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)

(setq-default regex-tool-backend 'perl)

(add-auto-mode 'conf-mode "Procfile")

(require-package 'twittering-mode)
(setq twittering-icon-mode t)

(require-package 'keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
        previous-line
        next-line))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(provide 'init-misc)
