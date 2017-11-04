(setq custom-file (concat user-emacs-directory "custom.el"))

(setq inhibit-startup-screen t
      auto-save-default      nil
      make-backup-files      nil
      ring-bell-function     #'ignore)

(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)

;; If on a Windoze system, wire in helpful programs that Windoze
;; doesn't have by default.
(when (eq system-type 'windows-nt)
  (setenv "PATH"
    (concat (getenv "PATH")
      "\;C:\\msys64\\usr\\bin")))

;; Here's where the `setup-*` (and my other) packages are located.
(add-to-list 'load-path
  (file-name-as-directory (concat user-emacs-directory "lisp")))

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

(use-package hydra :ensure t)

(use-package setup-look)
(use-package setup-theme)
(use-package setup-browser)

(use-package org
  :ensure org-plus-contrib
  :commands (org-agenda org-agenda-list)
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-c l" . org-store-link)
         ("\C-c a" . org-agenda)
         ("\C-c c" . org-capture)
         ("\C-c b" . org-iswitchb))
  :config
  (use-package setup-org))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-wrap t)
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind* (("C-x C-f" . counsel-find-file)
          ("C-s"     . swiper))
  :config
  (ivy-add-actions
   'counsel-find-file
   '(("f" find-file-other-frame "other frame")
     ("d" delete-file "delete"))))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-background t
        aw-keys       '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-flip-keys  '("n" "ν"))
  (add-to-list 'aw-dispatch-alist '(?ν aw-flip-window)))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  ;; From https://github.com/wasamasa/eyebrowse
  ;;   The default key bindings are:
  ;;   | Key bind  | Function                         |
  ;;   |-----------+----------------------------------|
  ;;   | C-c C-w < | Switch to previous window config |
  ;;   | C-c C-w > | Switch to next window config     |
  ;;   | C-c C-w ' | Switch to last window config     |
  ;;   | C-c C-w " | Close current window config      |
  ;;   | C-c C-w , | Rename current window config     |
  ;;   | C-c C-w 0 | Switch to window config 0        |
  ;;   | ...       | ...                              |
  ;;   | C-c C-w 9 | Switch to window config 9        |
  t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; The command =emacsclient -a "" -c= seems to start a server for you,
;; i.e., no need for the following lines.  (Check if this is true on
;; Windoze.)
;; (use-package server :demand
;;   :config
;;   (or (eq t (server-running-p)) (server-start)))
