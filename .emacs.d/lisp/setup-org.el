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
