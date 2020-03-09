;; flycheck
(use-package flycheck
  :commands flycheck-mode
  :diminish " ✓"
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))


;; flyspell spell checking.
(use-package flyspell
  :init (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (setq ispell-extra-args '("--sug-mode=fast"))
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil))


;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :commands restclient-mode
  :mode
  (("\\.http$" . restclient-mode)))

;; yaml
(use-package yaml-mode
  :mode
  ((("\\.yml$" . yaml-mode)
    ("\\.yaml$" . yaml-mode))))

(use-package restart-emacs)

(provide 'module-coding-general)
