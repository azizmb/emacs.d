;; https://github.com/luismayta/emacs.d/blob/815ce27a82c716a64ddffff0a8b37ea693c6141d/src/modules/module-editing.el

;; Revert buffers automatically when underlying files are changed externally.
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))


;; multiple-cursors
(use-package multiple-cursors
  ;; :init (setq mc/list-file (core/emacs.d "etc/.mc-lists.el"))
  :bind (("C->" . mc/mark-next-like-this)
	  ("C-<" . mc/mark-previous-like-this)
	  ("C-c C->" . mc/mark-all-like-this)))


;; expand-region
(use-package expand-region
  ;; FIXME
  ;; :init
  ;; (evil-leader/set-key "xx" 'er/expand-region)
  :bind ("C-=" . er/expand-region))


;; intelligently cleanup whitespace on save
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode t)
  :config
  (add-hook 'after-init-hook 'whitespace-cleanup-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup-mode))


;; undo-tree
;; Treat undo history as a tree.
(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  ;; :init
  ;; (evil-leader/set-key "u" 'undo-tree-visualize)
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))


;; embrace
;; Add/Change/Delete pairs based on expand-region.
(use-package embrace
  :bind ("C-," . embrace-commander))


(provide 'module-editing)
