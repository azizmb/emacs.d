(require-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)

(setq web-mode-engines-alist
      '(("django"    . ".*/templates/.*\\.html\\'")))

(provide 'init-html)
