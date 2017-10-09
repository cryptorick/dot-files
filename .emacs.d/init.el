(setq custom-file (concat user-emacs-directory "custom.el"))

(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq ring-bell-function #'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(menu-bar-mode -1)

;; Put the following in ~/.Xresources:
;;
;;   Emacs.font: terminus-12
;;   Emacs.toolBar: off
;;   Emacs.verticalScrollBars: off
;;   Emacs.horizontalScrollBars: off
;;
;; or uncomment the following:
;;
;; (when (display-graphic-p)
;;   (set-face-attribute 'default nil :font "monospace:antialias=standard")
;;   ;; or replace "monospace" in the above with your fav font.
;;   (tool-bar-mode -1)
;;   (scroll-bar-mode -1)
;;   (and (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1)))

(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
(setq load-prefer-newer t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package cyberpunk-theme :ensure t)

(use-package org
  :ensure org-plus-contrib
  :commands (org-agenda org-agenda-list)
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-c l" . org-store-link)
         ("\C-c a" . org-agenda)
         ("\C-c c" . org-capture)
	 ("\C-c b" . org-iswitchb))
  :config (load-file (concat user-emacs-directory "lisp/setup-org.el")))

(use-package markdown-mode
  :mode "\\.md\\'"
  :ensure t)
