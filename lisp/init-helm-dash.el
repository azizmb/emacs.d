;; from http://jwintz.me/blog/2014/02/16/helm-dash-makes-you-efficient/
;; and https://github.com/glynnforrest/emacs.d/blob/e97021798f4ddbe849bc7aaeace03b463b3043b5/site-lisp/setup-helm.el

(require-package 'helm-dash)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-dash-python ()
  (interactive)
  (setq-local helm-dash-docsets '("Python 2" "Python 3" "Django")))
(add-hook 'python-mode-hook 'helm-dash-python)

(defun helm-dash-js ()
  (interactive)
  (setq-local helm-dash-docsets '("JavaScript" "BackboneJS" "jQuery")))
(add-hook 'js2-mode-hook 'helm-dash-js)

(defun helm-dash-html ()
  (interactive)
  (setq-local helm-dash-docsets '("Html" "Font_Awesome")))
(add-hook 'html-mode-hook 'helm-dash-html)
(add-hook 'web-mode-hook 'helm-dash-html)

(defun helm-dash-css ()
  (interactive)
  (setq-local helm-dash-docsets '("CSS" "Bootstrap 2" "Bootstrap 3" "Bootstrap 4" "Sass")))
(add-hook 'css-mode-hook 'helm-dash-css)

(defun helm-dash-shell ()
  (interactive)
  (setq-local helm-dash-docsets '("Bash")))
(add-hook 'sh-mode-hook 'helm-dash-shell)

(defun helm-dash-docker ()
  (interactive)
  (setq-local helm-dash-docsets '("Docker")))
(add-hook 'dockerfile-mode-hook 'helm-dash-docker)

(defun helm-dash-yaml ()
  (interactive)
  (if (s-ends-with? ".sls" (buffer-file-name))
      (setq-local helm-dash-docsets '("Saltstack"))
    (setq-local helm-dash-docsets '("Ansible"))))
(add-hook 'yaml-mode-hook 'helm-dash-yaml)

(defun helm-dash-emacs-lisp ()
  (interactive)
  (setq-local helm-dash-docsets '("Emacs Lisp")))
(add-hook 'emacs-lisp-mode-hook 'helm-dash-emacs-lisp)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq helm-dash-docsets-path (format "%s/.emacs.d/docsets" (getenv "HOME")))

(defvar helm-dash-required-official-docsets '() "A list of required helm-dash-docsets")
(defvar helm-dash-required-user-docsets '() "A list of required helm-dash-docsets")

(setq helm-dash-required-official-docsets
      '(
        Ansible
        Bash
        CSS
        Sass
        Bootstrap_2
        Bootstrap_3
        Bootstrap_3
        Font_Awesome
        HTML
        JavaScript
        Markdown
        jQuery
        Python_2
        Python_3
        Django
        Emacs_Lisp
        ))

(setq helm-dash-required-user-docsets
      '(
        PEPs
        Pillow
        Six
        uWSGI
        iPython
        Django_REST_Framework
        ))

(defun ziz/helm-dash-install-official-docsets ()
  "Install required docsets"
  (interactive)
  (dolist (doc (mapcar 'symbol-name helm-dash-required-official-docsets))
    (when (not (file-exists-p (ziz/dash-path doc)))
      (message (format "Installing helm-dash docset '%s'" doc))
      (helm-dash-install-docset doc)))
  )

(defun ziz/helm-dash-install-user-docsets ()
  "Install required docsets"
  (interactive)
  (dolist (doc (mapcar 'symbol-name helm-dash-required-user-docsets))
    (when (not (file-exists-p (ziz/dash-path doc)))
      (message (format "Installing helm-dash docset '%s'" doc))
      (helm-dash-install-user-docset doc)))
  )

(defun ziz/helm-dash-upgrade-docsets ()
  "Upgrade installed docsets"
  (interactive)
  (dolist (doc (helm-dash-installed-docsets))
    (message (format "Upgrading helm-dash docset '%s'" doc))
    (helm-dash-update-docset doc)))

(setq helm-dash-min-length 2)
(setq helm-dash-browser-func 'eww)

(global-set-key (kbd "s-.") 'helm-dash-at-point)


(provide 'init-helm-dash)
