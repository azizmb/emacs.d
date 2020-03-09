(require 'core-vars)

(use-package projectile
  :bind-keymap
  ("C-c q" . projectile-command-map)
  :init
  (setq projectile-keymap-prefix (kbd "C-c q"))
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (setq projectile-switch-project-action 'projectile-vc)
  :config
  (add-to-list 'projectile-globally-ignored-files "node_modules")
  (projectile-mode)
  :delight
  projectile-mode 1)

(provide 'module-projects)
