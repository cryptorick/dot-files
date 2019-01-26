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

(setq load-prefer-newer t)

(defmacro comment (&rest body) nil)


;;----------------------------------------------------------------------
;; straight

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;;(setq straight-use-package-by-default t) ; not sure about this one.

;;----------------------------------------------------------------------
;; Here's where the `setup-*` (and my other) packages are located.
(add-to-list 'load-path
  (file-name-as-directory (concat user-emacs-directory "lisp")))
(use-package setup-look)
(use-package setup-theme)
(use-package setup-browser)


;;----------------------------------------------------------------------

(use-package hydra :straight t)

(defhydra hydra-straight-helper (:hint nil)
  "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |pru_n_e build"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("W" straight-watcher-quit)
  ("g" straight-get-recipe)
  ("n" straight-prune-build)
  ("q" nil))


;;----------------------------------------------------------------------
;; org

(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(use-package org
  :straight org-plus-contrib
  :commands (org-agenda org-agenda-list)
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-c l" . org-store-link)
         ("\C-c a" . org-agenda)
         ("\C-c c" . org-capture)
         ("\C-c b" . org-iswitchb))
  :config
  (require 'org-tempo)  ; for insertion of blocks, a la type "<s" then hit TAB.
  (use-package setup-org))

(use-package org-super-agenda
  :straight t
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          ;; Set order of multiple groups at once
          (:order-multi (2 (:name "Work"
                                  :tag "WORK")
                           (:name "Personal"
                                  :tag "HOME")
                           ))
          ;; Groups supply their own section names when none are given
          (:todo "WAITING" :order 8)  ; Set order of this section
          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                 ;; Show this group at the end of the agenda (since it has the
                 ;; highest number). If you specified this group last, items
                 ;; with these todo keywords that e.g. have priority A would be
                 ;; displayed in that group instead, because items are grouped
                 ;; out in the order the groups are listed.
                 :order 9)
          (:priority<= "B"
                       ;; Show this section after "Today" and "Important", because
                       ;; their order is unspecified, defaulting to 0. Sections
                       ;; are displayed lowest-number-first.
                       :order 1)
          ;; After the last group, the agenda will display items that didn't
          ;; match any of these groups, with the default order position of 99
          )))

;; Maybe move this to setup-org.el.
(use-package ox-hugo
  :straight t
  :config
  (use-package ox-hugo-auto-export)
  (org-hugo-auto-export-mode))

(use-package ox-mediawiki :straight t)


;;----------------------------------------------------------------------

(use-package ivy
  :straight t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-wrap t)
  :config
  (ivy-mode 1))

(use-package counsel :disabled
  :straight t
  :bind* (("C-x C-f" . counsel-find-file)
          ("C-s"     . swiper))
  :config
  (ivy-add-actions
   'counsel-find-file
   '(("f" find-file-other-frame "other frame")
     ("d" delete-file "delete"))))

(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-background t
        aw-keys       '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-flip-keys  '("n" "ν"))
  (add-to-list 'aw-dispatch-alist '(?ν aw-flip-window)))

(use-package eyebrowse
  :straight t
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
  :straight t
  :config (which-key-mode))

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :straight t
  :config (global-git-gutter-mode 1))

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'")

(use-package rainbow-delimiters
  :straight t
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
  :straight t
  :mode "\\.clj\\'")

(use-package cider
  :straight t
  :commands (cider-jack-in cider-connect))

(use-package haskell-mode :disabled
  :straight t
  :bind* (("C-`"     . haskell-interactive-bring)  ; this "brings up" the REPL
          ("C-c C-l" . haskell-process-load-or-reload))
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-process-type 'stack-ghci)
  ;; (use-package intero :straight t)
  ;; (add-hook 'haskell-mode-hook 'intero-mode)
  )

(use-package newlisp-mode
  :straight t
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

;; Source: https://github.com/tj64/picolisp-mode.git
(use-package picolisp
  :straight (picolisp :type git :host github :repo "tj64/picolisp-mode")
  :mode ("\\.l\\'" . picolisp-mode)
  :config
  (require 'inferior-picolisp)
  (add-hook 'picolisp-mode-hook
    (lambda ()
      (setq picolisp-program-name "~/local/bin/pil")
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

(use-package lsp-mode :straight t)

(use-package lsp-julia
  :straight (lsp-julia :type git :host github :repo "non-Jedi/lsp-julia"))

;; (use-package julia-mode
;;   :straight (julia-mode :type git :host github :repo "JuliaEditorSupport/julia-emacs")
;;   :config
;;   (add-hook 'julia-mode-hook #'lsp-mode))

;; (use-package julia-mode
;;   :straight t)

;; (use-package ess
;;   :straight t
;;   :requires (julia-mode))

(use-package ess-julia 
  :straight ess
  :config
  (add-hook 'ess-julia-mode-hook #'lsp-mode))

;; test: (assoc "\\.jl\\'" auto-mode-alist)
;; should say: ("\\.jl\\'" . ess-julia-mode)

;; Doesn't seem to work. I seems to contact the server but nothing
;; gets pulled down. Idk why.
(use-package org-caldav :disabled
  :config
  (setq
   ;; Set org-caldav-url to the base address of your CalDAV server:
   org-caldav-url     "https://caldav.fastmail.com/dav/calendars/user/cryptorick@fastmail.com"
   ;; Set org-caldav-calendar-id to the calendar-id of your new calendar:
   org-caldav-calendar-id "f33f94cd-03cd-4f20-820f-efa4033c82b7"
   ;; Set org-caldav-inbox to an org filename where new entries from
   ;; the calendar should be stored. Just to be safe, I suggest using
   ;; an empty, dedicated Org file for that.
   org-caldav-inbox   (expand-file-name "~/org-test-from.org")
   ;; Set org-caldav-files to the list of org files you would like to
   ;; sync. The above org-caldav-inbox will be automatically added, so
   ;; you don't have to add it here.
   org-caldav-files   `(,(expand-file-name "~/org-test-to.org")) ;`(,org-caldav-inbox))
  )
   ;; Call org-caldav-sync to start the sync. The URL package will ask
   ;; you for username/password for accessing the calendar. (See below
   ;; on how to store that password in an authinfo file.)
)

;; The command =emacsclient -a "" -c= seems to start a server for you,
;; i.e., no need for the following lines.
;; (use-package server :demand
;;   :config
;;   (or (eq t (server-running-p)) (server-start)))
