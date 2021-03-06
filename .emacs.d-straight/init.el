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
(use-package setup-eshell)


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
(define-key global-map (kbd "<f8>") 'hydra-straight-helper/body)

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

;; All functions with prefix =efs/= stolen from "Emacs from Scratch". :D
(defun efs/org-mode-setup ()
  (interactive)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun rkh/disable-org-mode-setup ()
  (interactive)
  (variable-pitch-mode 0)
  (visual-line-mode 0)
  (visual-fill-column-mode 0))

(use-package org
  :straight org-plus-contrib
  :commands (org-agenda org-agenda-list)
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-c l" . org-store-link)
         ("\C-c a" . org-agenda)
         ("\C-c c" . org-capture)
         ("\C-c b" . org-iswitchb)
         ("<f9>"   . org-agenda)
         ("<f12>"  . org-capture))
  :hook (org-mode . efs/org-mode-setup)
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
  :after ox)

(use-package org-bullets
  :straight (org-bullets :type git :host github :repo "Kaligule/org-bullets")
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-hide-leading-stars t))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package org-indent-mode :hook org-mode)

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

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file))
  ;; :bind* (("C-x C-f" . counsel-find-file)
  ;;         ("C-s"     . swiper))
  :config
  (ivy-add-actions
   'counsel-find-file
   '(("f" find-file-other-frame "other frame")
     ("d" delete-file "delete"))))

(use-package ivy-rich
  :straight t
  :after counsel
  :init
  ;;(use-package counsel :straight t)
  (ivy-rich-mode 1))

(use-package helpful
  :straight t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ace-window :disabled ; in favor of exwm workflow
  :straight t
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-background t
        aw-keys       '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-flip-keys  '("n" "ν"))
  (add-to-list 'aw-dispatch-alist '(?ν aw-flip-window)))

(use-package eyebrowse :disabled ; in favor of exwm workflow
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

(use-package ranger
  :straight t)

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status)
  :custom
  ((magit-auto-revert t)
   (transient-show-popup 1)))

;; (use-package autorevert
;;   :custom
;;   ((auto-revert-check-vc-info t)
;;    (auto-revert-interval 2)))

(use-package git-gutter
  :straight t
  :config (global-git-gutter-mode 1))

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'")

(use-package display-line-numbers
  :config (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(use-package rainbow-delimiters
  :straight t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package setup-windoze
  :if (eq 'windows-nt system-type))

(use-package ws-butler
  :straight (ws-butler :type git :host github :repo "lewang/ws-butler")
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package htmlize
  :straight (htmlize :type git :host github :repo "hniksic/emacs-htmlize"))

(use-package csv-mode
  :straight t
  :mode "\\.[Cc][Ss][Vv]\\'")

(use-package visual-fill-column
  :straight t)

;; This is from Protesilaos, but I can't tell the difference visually.
(use-package emacs
  :config
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'right-only)
  :hook (after-init-hook . window-divider-mode))

;; From Protesilaos
(use-package windmove
  :config
  (setq windmove-create-window nil)     ; Emacs 27.1
  )

;; From Protesilaos
(use-package transpose-frame
  :straight t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise))

(use-package tab-bar
  :init
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  :config
  (tab-bar-mode -1)
  (tab-bar-history-mode -1))

(use-package window
  :init
  (setq display-buffer-alist
        '(;; top side window (NONE for now. --rkh)
          ;; bottom side window
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.16)       ; See the :hook
           (side . bottom)
           (slot . -1)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Completions\\|Embark Live Occur\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ;; left side window
          ;; right side window
          ("\\(\\*R:\\|magit: \\).+"
           (display-buffer-in-side-window)
           (window-width . 0.45)
           (side . right)
           (slot . 0)
           (window-parameters
            . ((no-other-window . t)
               (mode-line-format
                . (" "
                   mode-line-buffer-identification)))))
          (".*\\(e?shell\\|vterm\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.45)
           (window-height . 0.30)
           (side . right)
           (slot . 1))
          ;; bottom buffer (NOT side window)
          ("\\*\\vc-\\(incoming\\|outgoing\\).*"
           (display-buffer-at-bottom))))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  ;; Hooks' syntax is controlled by the `use-package-hook-name-suffix'
  ;; variable.  The "-hook" suffix is intentional.
  :hook ((help-mode-hook . visual-line-mode)
         (custom-mode-hook . visual-line-mode)))

(defun rkh/press-return-and-move-up ()
  "Use this when you need to run a some command in the
shell (like curl) and quickly get back to the inferior R buffer,
to wait on a breakpoint when debugging (which could occur very
quickly, fsater than you/I can switch to the R window manually."
  (interactive)
  (eshell-send-input)
  (windmove-up))

(defhydra hydra-winders-helper (global-map "C-z")
  "windmove"
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("<down>" windmove-down)
  ("<left>" windmove-left)
  ("t" flop-frame) ; what I consider "transpose" in this context
  ("r" rotate-frame-clockwise)
  ("R" rotate-frame-anticlockwise)
  ("<tab>" tab-next)
  ("<backtab>" tab-previous)
  ;; ("C-x _" balance-windows)
  ;; ("C-x +" balance-windows-area)
  ("q" window-toggle-side-windows)
  ("<return>" rkh/press-return-and-move-up)
  ("C-z" nil))


;; doesn't work. need to investigate when I get time.
(defun orgtable-to-csv (pt mrk)
  (interactive "r")
  (save-excursion
    (let ((table (buffer-substring-no-properties pt mrk)))
      (orgtbl-to-csv table))))


(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line.

Source: https://www.emacswiki.org/emacs/UnfillRegion"
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(define-key global-map "\C-\M-Q" 'unfill-region)

(use-package shell
  :config
  (unless (eq 'windows-nt system-type)
    (setq explicit-mksh-args '("-l"))))


;;----------------------------------------------------------------------
;; Languages

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

(use-package picolisp-mode :disabled
  :straight t  ; this is Alexis's (flexibeast) version (in MELPA)
  :mode "\\.l\\'"
  :config
  ;; Find what we need on $PATH.
  (setq picolisp-pil-executable       "pil"
        picolisp-pilindent-executable "pilIndent"
        picolisp-documentation-directory
        (expand-file-name
         (concat
          (file-name-directory
           (file-truename
            (or (executable-find picolisp-pil-executable) "")))
          "../doc"))
        picolisp-documentation-unavailable
        (not (file-readable-p
              (concat (file-name-as-directory picolisp-documentation-directory)
                      "ref.html")))))

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


(use-package vterm
  :unless (eq 'windows-nt system-type)  ; doesn't work under winders.
  :straight t
  :config
  (setq vterm-shell "/bin/mksh -l"
        vterm-kill-buffer-on-exit t)
  (define-key global-map (kbd "<f7>") 'vterm))

(use-package julia-snail
  ;; Haven't tested this on Windoze yet. Uncomment next line if it doesn't.
  ;;:unless (eq 'windows-nt system-type)  ; doesn't work under winders.
  :straight t
  :hook (julia-mode . julia-snail-mode))

;; (use-package julia-mode
;;   :straight (julia-mode :type git :host github :repo "JuliaEditorSupport/julia-emacs")
;;   :config
;;   (add-hook 'julia-mode-hook #'lsp-mode))

;; (use-package julia-mode
;;   :straight t)

;; (use-package ess
;;   :straight t
;;   :requires (julia-mode))

;; (use-package ess-julia
;;   :straight ess
;;   :config
;;   (add-hook 'ess-julia-mode-hook #'lsp-mode))

;; test: (assoc "\\.jl\\'" auto-mode-alist)
;; should say: ("\\.jl\\'" . ess-julia-mode)

(use-package julia-mode :disabled
  :straight (julia-mode :type git :host github :repo "JuliaEditorSupport/julia-emacs"))

;; (defun rkh/julia-wrapper-config ()
;;   (when (eq 'windows-nt system-type)
;;     (setq windoze-julia-repl-wrapper
;;           (make-temp-file "" nil "-windoze-julia-repl-wrapper.jl"
;;                           "using Base: stdin, stdout, stderr
;; using REPL.Terminals: TTYTerminal
;; using REPL: BasicREPL, run_repl

;; run_repl(BasicREPL(TTYTerminal(\"emacs\",stdin,stdout,stderr)))"))
;;     (setq inferior-julia-args (concat "-L " windoze-julia-repl-wrapper))))

;; (use-package ess-julia
;;   :straight ess
;;   ;;:config
;;   ;;(setq inferior-julia-program-name (executable-find "julia"))
;;   ;;(setq inferior-julia-args "-i --color=yes -e \"ENV[\\\"TERM\\\"]=\\\"emacs\\\"\"")
;;   ;; (rkh/julia-wrapper-config) ; w/o this, Julia REPL hangs in inferior lisp buffer.
;;   )

(use-package julia-repl :disabled
  :straight (julia-repl :type git :host github :repo "tpapp/julia-repl")
  :config
  (add-hook 'ess-julia-mode-hook #'julia-repl-mode))
;; test: (assoc "\\.jl\\'" auto-mode-alist)
;; should say: ("\\.jl\\'" . julia-mode)

(use-package ess
  :straight t
  :bind (:map ess-mode-map
         ("<C-return>" . ess-eval-region-or-line-and-step)
         ("C-x C-e" . ess-eval-paragraph))
  :config
  (when (eq 'windows-nt system-type)
    (setq-default inferior-ess-r-program
                  (expand-file-name "C:/Progra~1/R/R-3.6.3/bin/x64/Rterm.exe")))
  (require 'ess-site)
  (setq ess-eval-visibly nil
        ess-tab-complete-in-script t
        inferior-R-args "--no-save")
  (add-hook 'ess-r-mode-hook
            (lambda ()
              ;; don't indent comments with a single #.
              (setq-default ess-indent-with-fancy-comments nil)
              ;; treat underscore as part of a "word".
              (modify-syntax-entry ?_ "w" ess-r-mode-syntax-table)
              (ess-set-style 'RStudio))))

(use-package lsp-mode :disabled
  :straight t)

(use-package lsp-julia :disabled
  :straight (lsp-julia :type git :host github :repo "non-Jedi/lsp-julia")
  :init
  (setq lsp-julia-package-dir nil))

(use-package jupyter
  :straight t
  :config
  (setq inferior-julia-program-name (executable-find "julia")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (R . t)
   (julia . t)
   (python . t)
   (jupyter . t)))


(use-package eglot
  :straight (eglot :type git :host github :repo "joaotavora/eglot")
  :commands (eglot))


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

(use-package emms
  :straight t
  :config
  ;; First, ensure mpd (server) is running.  It's enough to:
  ;; $ mpd
  ;; mpd will start in the background (i.e., typing `&` after the
  ;; command is unnecessary).  So, you can invoke this on an as-needed
  ;; basis (on the command line) or add the above invocation to your
  ;; .xinitrc.  Note: on FreeBSD, the command is called `musicpd`.
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all) ; don't change this to values you see on stackoverflow questions if you expect emms to work
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-volume-change-function #'emms-volume-mpd-change)
  ;; Following 2 lines maybe unnecessary: `ag mpc` yields no hits in the emms source tree.
  ;;(require 'mpc) ; already included in Emacs; require probably unnecessary.
  ;;(setq mpc-host (expand-file-name "~/.mpd/socket"))
  (setq emms-player-mpd-server-nameq (expand-file-name "~/.mpd/socket")) ; default: "localhost"
  (setq emms-player-mpd-server-port nil)                                 ; default: "6600"
  ;;(setq emms-source-file-default-directory (expand-file-name "~/Music/"))
  (defun my/emms-volume-adjust (inc)
    (interactive "p")
    (let ((ev last-command-event)
	  (echo-keystrokes nil))
      (let* ((base (event-basic-type ev))
             (inc (or inc emms-volume-change-amount))
             (step
              (pcase base
                ((or ?+ ?=) inc)
                (?- (- inc))
                (_ inc))))
        (funcall emms-volume-change-function step)
        (message "Use +,- for further adjustment")
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (dolist (key '(?- ?+ ?=))
             (define-key map (vector (list key))
               `(lambda () (interactive) (my/emms-volume-adjust (abs ,inc)))))
           map)))))
  :bind
  ("s-m c" . emms-player-mpd-connect-function) ; this re-connects emms to the running mpd
  ("s-m p" . emms)              ; this pops-up the playlist
  ("s-m b" . emms-smart-browse) ; this pops-up the browser (from where you can build playlist)
  ("s-m r" . emms-player-mpd-update-all-reset-cache) ; this tells mpd to scan for updated
                                                     ; files in the music directory
  ("s-m P" . emms-pause)
  ("s-m s" . emms-stop)
  ("s-m +" . my/emms-volume-adjust)
  ("s-m =" . my/emms-volume-adjust)
  ("s-m -" . my/emms-volume-adjust)
  ("<XF86AudioPrev>" . emms-previous)
  ("<XF86AudioNext>" . emms-next)
  ("<XF86AudioPlay>" . emms-pause)
  ("<XF86AudioStop>" . emms-stop))

;;----------------------------------------------------------------------
;; exwm

(use-package exwm
  :straight t
  :demand t)

(use-package exwm-config
  :demand t
  :after exwm)

(use-package exwm-input
  :demand t
  :after exwm
  :config
  ;; Bind C-q so that the next key is sent literally to the
  ;; application                    ; from: DamienCassou
  (add-to-list 'exwm-input-prefix-keys ?\C-q)
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  (setq exwm-workspace-number 9     ; from: technomancy
        exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)

  ;; Key bindings accessible from everywhere:
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  ;; Bind "s-w" to switch workspace interactively.
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
  ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
  (dolist (i (number-sequence 0 9))
    (exwm-input-set-key `,(kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  (exwm-input-set-key (kbd "<s-tab>") #'other-window)

  ;; switch focus of buffers with s-[hjkl]   ; from: reddit/u/nice_handbasket
  (exwm-input-set-key (kbd "s-<left>") #'windmove-left)
  (exwm-input-set-key (kbd "s-<down>") #'windmove-down)
  (exwm-input-set-key (kbd "s-<up>") #'windmove-up)
  (exwm-input-set-key (kbd "s-<right>") #'windmove-right)
  ;; swap buffers with C-s-[hjkl]
  ;; (exwm-input-set-key (kbd "C-s-h")
  ;;   (lambda () (interactive) (aw-swap-window (window-in-direction 'left))))
  ;; (exwm-input-set-key (kbd "C-s-j")
  ;;   (lambda () (interactive) (aw-swap-window (window-in-direction 'below))))
  ;; (exwm-input-set-key (kbd "C-s-k")
  ;;   (lambda () (interactive) (aw-swap-window (window-in-direction 'above))))
  ;; (exwm-input-set-key (kbd "C-s-l")
  ;;   (lambda () (interactive) (aw-swap-window (window-in-direction 'right))))
  (exwm-input-set-key (kbd "s-[") #'shrink-window-horizontally)
  (exwm-input-set-key (kbd "s-{") #'shrink-window)
  (exwm-input-set-key (kbd "s-]") #'enlarge-window-horizontally)
  (exwm-input-set-key (kbd "s-}") #'enlarge-window)
  
  (exwm-input-set-key (kbd "s-c") #'kill-this-buffer)
  (exwm-input-set-key (kbd "s-f") #'exwm-floating-toggle-floating)

  ;; whenever I start using floating setup   ; from: gh/Ambrevar
  (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

  (defun my/trim-non-ff ()          ; from: technomancy
    (delete-if-not (lambda (name)
                     (or (string-match "- Mozilla Firefox$" name)
                         (string-match "Chromium" name)))
                   ido-temp-list))

  (add-hook 'exwm-update-title-hook ; from: technomancy
            (lambda ()
              (when (or (string-match "Firefox" exwm-class-name)
                        (string-match "st-256" exwm-class-name))
                (exwm-workspace-rename-buffer exwm-title))))
  
  (add-hook 'exwm-manage-finish-hook; from: technomancy
            (lambda ()
              (when (string-match "st-256" exwm-class-name)
                (exwm-input-release-keyboard))
              ;; (when (string-match "Chromium" exwm-class-name)
              ;;   (exwm-layout-hide-mode-line))
              ;; (when (string-match "Firefox" exwm-class-name)
              ;;   (setq ido-make-buffer-list-hook 'my/trim-non-ff)
              ;;   (exwm-layout-hide-mode-line))
              ))

  ;;(exwm-enable-ido-workaround)      ; from: technomancy
  (defun my/exwm-run (command)      ; from: technomancy
    (interactive (list (read-shell-command "$ ")))
    (save-window-excursion
      (start-process-shell-command command "*my/exwm-run Log*" command)))
  (define-key exwm-mode-map (kbd "s-SPC") 'my/exwm-run)
  (global-set-key (kbd "s-SPC") 'my/exwm-run)

  ;; Launch apps from hotkeys       ; idea from: technomancy
  (dolist (k '(("s-l" "slock")
               ("s-<return>" "/usr/home/rick/builds/xst/xst -A 220 -f terminus:size=12 -g 156x56 -e mksh -l")
               ("s-S-<return>" "st -f terminus:size=12 -g 156x56 -e mksh -l")))
    (exwm-input-set-key `,(kbd (car k))
                        `(lambda () (interactive)
                           (save-window-excursion
                             (start-process-shell-command ,(cadr k) nil ,(cadr k))))))

  ;; Add dmenu_run-like functionality (often mistakenly called
  ;; "dmenu-like" functionality, as this next package does).
  (use-package dmenu
    :straight t
    :config
    (setq dmenu-history-size 0))
  (define-key exwm-mode-map (kbd "s-p") 'dmenu)
  (global-set-key (kbd "s-p") 'dmenu)
  
  ;; Use plan8port plumb(1) for plumbing. Note: you need to start
  ;; plumber(1), the server, first (e.g., M-x my/start-plumber).
  ;; No worky:
  (defun my/start-plumber ()
    (interactive)
    (save-window-excursion
      (let ((save-env process-environment))
        (setenv "PLAN9" "/usr/local/plan9")
        (setenv "PATH" (concat (getenv "PLAN9") "/bin:" (getenv "PATH")))
        (message (getenv "PATH"))
        (start-process "Plan9Plumber" (get-buffer-create "*Plumber Log*") "plumber")
        (setq process-environment save-env))))
  ;; No worky:
  (defun my/plumber ()
    (interactive)
    (save-window-excursion
      (let ((cmd "9 plumber"))
        (start-process-shell-command cmd nil cmd))))
  (defun my/plumb ()
    (interactive)
    (save-window-excursion
      (let ((cmd (concat "9 plumb " (gui-get-primary-selection))))
        (start-process-shell-command cmd nil cmd))))
  (exwm-input-set-key (kbd "s-o") #'my/plumb)

  (exwm-input-set-key (kbd "s-m c") #'emms-player-mpd-connect-function)
  (exwm-input-set-key (kbd "s-m p") #'emms)
  (exwm-input-set-key (kbd "s-m b") #'emms-smart-browse)
  (exwm-input-set-key (kbd "s-m r") #'emms-player-mpd-update-all-reset-cache)
  (exwm-input-set-key (kbd "s-m P") #'emms-pause)
  (exwm-input-set-key (kbd "s-m s") #'emms-stop)
  (exwm-input-set-key (kbd "s-m +") #'my/emms-volume-adjust)
  (exwm-input-set-key (kbd "s-m =") #'my/emms-volume-adjust)
  (exwm-input-set-key (kbd "s-m -") #'my/emms-volume-adjust)
  (exwm-input-set-key (kbd "<XF86AudioPrev>") #'emms-previous)
  (exwm-input-set-key (kbd "<XF86AudioNext>") #'emms-next)
  (exwm-input-set-key (kbd "<XF86AudioPlay>") #'emms-pause)
  (exwm-input-set-key (kbd "<XF86AudioStop>") #'emms-stop)

  ;; Some hotkeys I'd like to work in X11 apps also. (Careful: this
  ;; might shadow the app's own keybinding, when you need whatever
  ;; function that is and is only accessible from that binding).
  (exwm-input-set-key (kbd "<f9>")  #'org-agenda)
  (exwm-input-set-key (kbd "<f12>") #'org-capture)
  (exwm-input-set-key (kbd "<f8>")  #'hydra-straight-helper/body)

  (setq exwm-input-simulation-keys  ; from: DamienCassou
        `(;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\M-<] . [C-home])
          ([?\M->] . [C-end])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end ?\C-x])
          ;; cut/paste, selection
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ([?\M-d] . [C-S-right ?\C-x])
          ([M-backspace] . [C-S-left ?\C-x])
          ([?\C-x h] . [?\C-a])     ; this should start to work from emacs 26.2+
          ;; search
          ([?\C-s] . [?\C-f])
          ;; misc
          ([?\C-g] . [escape])
          ([?\C-i] . [tab])
          ([?\C-m] . [return])))

  (defun my/exwm-keyrules-xst ()
    (if (and exwm-class-name
             (string-match "^xst-256" exwm-class-name))
        (exwm-input-set-local-simulation-keys
         '(([?\C-c] . [?\C-c])
           ([?\C-d] . [?\C-d])
           ([?\C-k] . [?\C-k])
           ([?\C-y] . [?\M-\S-v])
           ([?\M-w] . [?\M-\S-c])))))
  (add-hook 'exwm-manage-finish-hook #'my/exwm-keyrules-xst)
  (defun my/exwm-keyrules-st ()
    (if (and exwm-class-name
             (string-match "^st-256" exwm-class-name))
        (exwm-input-set-local-simulation-keys
         '(([?\C-c] . [?\C-c])
           ([?\C-d] . [?\C-d])
           ([?\C-k] . [?\C-k])
           ([?\C-y] . [?\C-\S-v])
           ([?\M-w] . [?\C-\S-c])))))
  (add-hook 'exwm-manage-finish-hook #'my/exwm-keyrules-st)

;;  (exwm-enable)
;;  ;;(exwm-config-ido)
;;  (exwm-config-misc)
;;  (server-start)
  )

;; The command =emacsclient -a "" -c= seems to start a server for you,
;; i.e., no need for the following lines.
(use-package server :demand
  :config
  (or (eq t (server-running-p)) (server-start)))
