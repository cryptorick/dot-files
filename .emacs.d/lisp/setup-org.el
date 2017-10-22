;; Inspired by https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html

(setq my-gtd-inbox    "~/gtd/inbox.org"
      my-gtd-projects "~/gtd/projects.org"
      my-gtd-tickler  "~/gtd/tickler.org"
      my-gtd-inactive "~/gtd/someday.org")

(setq org-agenda-files
        (list my-gtd-inbox
              my-gtd-projects
              my-gtd-tickler)

      org-capture-templates
        `(("t" "TODO item (goes to inbox)"
           entry
           (file+headline ,my-gtd-inbox "Tasks")
           "* TODO %i%?")
          ("T" "Tickler" entry
           (file+headline ,my-gtd-tickler "Tickler")
           "* %i%? \n %U"))

      org-refile-targets
        '((my-gtd-projects :maxlevel . 3)
          (my-gtd-inactive :level    . 1)
          (my-gtd-tickler  :maxlevel . 2))

      org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))

      org-agenda-custom-commands
        '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
          ("o" "At the office" tags-todo "@office"
           ((org-agenda-overriding-header "Office")
            (org-agenda-skip-function #'my-org-agenda-skip-all-but-first-actionable)))))

(defun my-org-agenda-skip-all-but-first-actionable ()
  "Skip the entry if is not the first TODO entry (among its
siblings). Evaluates to nil, if the entry should not be skipped;
otherwise, evaluates to the position from where the search should
be continued (by the agenda builder (the caller))."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      ;; If the current entry is a TODO, then we will enter the
      ;; following while loop, successively checking prior siblings
      ;; until (i) we find a prior sibling that is a TODO also, in
      ;; which case the current entry is *not* the first TODO entry at
      ;; this level, or (ii) we find no prior siblings which are
      ;; TODOs, which means that the current entry is in fact the
      ;; first TODO at this level.
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(custom-set-faces
  ;; Change the org title and headlines to normal size text (when the
  ;; theme tries to make them bigger, e.g. cyberpunk does this).
  '(org-document-title ((t (:height 1.0))))
  '(org-level-1 ((t (:height 1.0))))
  '(org-level-2 ((t (:height 1.0))))
  '(org-level-3 ((t (:height 1.0)))))

(require 'hydra)

;; (From https://github.com/abo-abo/hydra/wiki/Org-agenda)
;; I don't have this wired to a key yet -- just testing it out:
;; calling it from the entry function hydra-org-agenda/body.
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

(provide 'setup-org)
