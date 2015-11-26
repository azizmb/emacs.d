(require-package 'multi-term)

;; http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/


(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (yas-minor-mode -1)
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (setq term-buffer-maximum-size 10000)
            ))


(provide 'init-shell)
