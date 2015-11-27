(require-package 'multi-term)

;; http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/

(setq multi-term-program "/usr/bin/zsh")

(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (yas-minor-mode -1)
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (setq term-buffer-maximum-size 10000)
            ))

(global-set-key (kbd "s-s") 'multi-term)

(provide 'init-shell)
