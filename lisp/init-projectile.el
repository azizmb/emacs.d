(require-package 'projectile)

(projectile-global-mode)

(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-/] 'projectile-persp-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-g] 'projectile-ag)

(setq projectile-switch-project-action 'projectile-vc)

(require-package 'perspective)
(require-package 'persp-projectile)
(persp-mode)


(provide 'init-projectile)
