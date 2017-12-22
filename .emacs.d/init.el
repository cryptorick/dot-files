(setq custom-file (concat user-emacs-directory "custom.el"))

(setq inhibit-startup-screen t
      auto-save-default      nil
      make-backup-files      nil
      ring-bell-function     #'ignore)

(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)

;; Redefine this terrible-terrible standard function as a noop.
(defun suspend-frame ()
  (interactive)
  (message "suspend-frame is suspended! :)"))

;; Here's where the `setup-*` (and my other) packages are located.
(add-to-list 'load-path
  (file-name-as-directory (concat user-emacs-directory "lisp")))

(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/"))

      package-pinned-packages
      '((use-package . "melpa-stable")
        (haskell-mode . "melpa-stable")
        (cider . "melpa-stable")))

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

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode 1))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package setup-windoze
  :if (eq 'windows-nt system-type))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line.

Source: https://www.emacswiki.org/emacs/UnfillRegion"
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(define-key global-map "\C-\M-Q" 'unfill-region)

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'")

(use-package cider
  :ensure t
  :commands (cider-jack-in cider-connect))

(use-package haskell-mode
  :ensure t
  :bind* (("C-`"     . haskell-interactive-bring)  ; this "brings up" the REPL
          ("C-c C-l" . haskell-process-load-or-reload))
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-process-type 'stack-ghci))

(use-package newlisp-mode
  :ensure t
  :mode "\\.lsp\\'"
  :interpreter "newlisp"
  :config
  (setq newlisp-command ; <-- default is "newlisp".
        (or (executable-find "newlisp")
            ;; Intrinsic Anaphoric If, where are you?
            (let ((nldir (getenv "NEWLISPDIR")))
              (if nldir (concat nldir "/newlisp") "newlisp"))))
  ;; Fix potential tab issue + ensure font lock is enabled.
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (turn-on-font-lock))

(use-package picolisp
  ;; Source: https://github.com/tj64/picolisp-mode.git
  :load-path "~/.emacs.d/contrib/picolisp-mode"
  :mode ("\\.l\\'" . picolisp-mode)
  :config
  (require 'inferior-picolisp)
  (add-hook 'picolisp-mode-hook
    (lambda ()
      (setq picolisp-program-name "/opt/bin/pil")
      ;;(tsm-mode)
      (rainbow-delimiters-mode)
      ;; Restore sane code formatting.
      (setq picolisp-body-indent 2)
      (setq picolisp-parsep nil)
      (defun picolisp-indent-function (indent-point state)
        ;; For me `picolisp-indent-function' should have the same definition
        ;; as `lisp-indent-function' except that the properties for
        ;; indentation should be kept in `picolisp-indent-function', not
        ;; `lisp-indentation-function', so as not to tinker with the
        ;; indentation of other lisps (especially elisp).  I can't use TJ's
        ;; `picolisp-indent-function' because the default indentation is to
        ;; use the "defform" indent, and I don't like that -- I want the
        ;; default normal indentation (like for function calls).
        (let ((normal-indent (current-column)))
          (goto-char (1+ (elt state 1)))
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (if (and (elt state 2)
                   (not (looking-at "\\sw\\|\\s_")))
              ;; car of form doesn't seem to be a symbol
              (progn
                (if (not (> (save-excursion (forward-line 1) (point))
                            calculate-lisp-indent-last-sexp))
                    (progn (goto-char calculate-lisp-indent-last-sexp)
                           (beginning-of-line)
                           (parse-partial-sexp (point)
                                               calculate-lisp-indent-last-sexp 0 t)))
                (backward-prefix-chars)
                (current-column))
              (let ((function (buffer-substring (point)
                                                (progn (forward-sexp 1) (point))))
                    method)
                (setq method (function-get (intern-soft function)
                                           'picolisp-indent-function))
                (cond ((or (eq method 'defun)
                           (and (null method)
                                (> (length function) 3)
                                (string-match "\\`def" function)))
                       (lisp-indent-defform state indent-point))
                      ((integerp method)
                       (lisp-indent-specform method state
                                             indent-point normal-indent))
                      (method
                       (funcall method indent-point state)))))))
      (defmacro def-pil-indent (operator indentation)
        `(put ',operator 'picolisp-indent-function ',indentation))
      (def-pil-indent def defun)
      (def-pil-indent de defun)
      (def-pil-indent dm defun)
      (def-pil-indent for defun)
      (def-pil-indent let 1)
      (def-pil-indent use 1)
      (def-pil-indent recur 1)
      (def-pil-indent until 1)
      (def-pil-indent unless 1)
      (def-pil-indent let? 2))))

;; The command =emacsclient -a "" -c= seems to start a server for you,
;; i.e., no need for the following lines.
;; (use-package server :demand
;;   :config
;;   (or (eq t (server-running-p)) (server-start)))
