(require-package 'helm)

(setq
 helm-M-x-requires-pattern             0
 helm-autoresize-mode                  t
 helm-candidate-number-limit 200
 helm-completion-in-region-fuzzy-match t
 helm-ff-file-name-history-use-recentf t
 helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
 helm-google-suggest-use-curl-p        t
 helm-mode-fuzzy-match                 t
 helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
 helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
 helm-split-window-default-side 'other
 helm-split-window-in-side-p           t
 )

(require-package 'helm-ag)
(require-package 'helm-company)
(require-package 'helm-gitignore)
(require-package 'swiper)
(require-package 'swiper-helm)

(global-set-key (kbd "C-s") 'swiper-helm)
(global-set-key (kbd "C-c h o") 'helm-occur)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)


(provide 'init-helm)
