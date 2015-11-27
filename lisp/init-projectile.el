(require-package 'projectile)

(projectile-global-mode)

(require-package 'helm-projectile)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-/] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-g] 'projectile-ag)

(setq projectile-switch-project-action 'helm-projectile)
;; (setq projectile-switch-project-action 'projectile-vc)


(provide 'init-projectile)
