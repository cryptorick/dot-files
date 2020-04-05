;; (use-package term  ; emacs provided
;;   :config
;;   ;; Kill the term buffer when you exit the shell. (That's how eshell
;;   ;; does it and how terminal emulator programs outside of Emacs do
;;   ;; it, and I like that.)
;;   (defadvice term-handle-exit
;;       (after term-kill-buffer-on-exit activate)
;;     (kill-buffer)))

;; (use-package xterm-color
;;   :straight t
;;   :config
;;   ;; Instructions from: https://github.com/atomontage/xterm-color
;;   ;; Don't forget to set TERM accordingly (xterm-256color).
;;   (setq comint-output-filter-functions
;;         (remove 'ansi-color-process-output comint-output-filter-functions))
;;   (add-hook 'shell-mode-hook
;;     (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
;; )
;; (use-package eterm-256color :disabled
;;   :straight (:host github
;;              :branch "devel"
;;              :repo "dieggsy/eterm-256color"
;;              :files ("eterm-256color.el" "eterm-256color.ti"))
;;   ;;:hook (term-mode . eterm-256color-mode)
;;   )
;; (use-package xterm-color :disabled
;;   :straight t
;;   :commands xterm-color-filter
;;   :hook ((eshell-before-prompt . (lambda ()
;;                                    (setq xterm-color-preserve-properties t)))))

;; TODO trying to prevent eshell from expanding ~.
;; (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|~\\)/\\'")

;; (setq eshell-visual-subcommands
;;       '(("git" "log" "diff" "show" "lol" "lola")
;;         ("git.exe" "log" "diff" "show" "lol" "lola"))
;;       )

;; TODO FIX doesn't work yet.
;; Note: `eshell-command' has some useful logic to steal. :)
(defun rkh/insert-in-new-buffer (string)
  (let ((buf (generate-new-buffer " *eshell redir*")))
    ;;(with-current-buffer buf
      (insert string)
    ;;  )
      ;;buf
      ))

;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (push (list "/dev/_" 'rkh/insert-in-new-buffer nil)
;;                   eshell-virtual-targets)))

(defalias 'eshell/b 'eshell-exec-visual)

(defun eshell/b (&rest args) args)

;;------------------------------------------------------------------------
;; From https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#aliases

(add-hook 'eshell-mode-hook
  (lambda ()
    (eshell/alias "e" "find-file $1")
    (eshell/alias "ff" "find-file $1")
    (eshell/alias "emacs" "find-file $1")
    (eshell/alias "ee" "find-file-other-window $1")
    (eshell/alias "v" "view-file $1")
    (eshell/alias "vv" "view-file-other-window $1")

    (eshell/alias "gd" "magit-diff-unstaged")
    (eshell/alias "gds" "magit-diff-staged")
    (eshell/alias "d" "dired $1")))

;;------------------------------------------------------------------------
;; from https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#git

(defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

;;------------------------------------------------------------------------
;; from https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#find-file

(defun eshell/f (filename &optional dir try-count)
  "Searches for files matching FILENAME in either DIR or the
current directory. Just a typical wrapper around the standard
`find' executable.

Since any wildcards in FILENAME need to be escaped, this wraps the shell command.

If not results were found, it calls the `find' executable up to
two more times, wrapping the FILENAME pattern in wildcat
matches. This seems to be more helpful to me."
  (let* ((cmd (concat
               (executable-find "find")
               " " (or dir ".")
               "      -not -path '*/.git*'"
               " -and -not -path '*node_modules*'"
               " -and -not -path '*classes*'"
               " -and "
               " -type f -and "
               "-iname '" filename "'"))
         (results (shell-command-to-string cmd)))

    (if (not (s-blank-str? results))
        results
      (cond
       ((or (null try-count) (= 0 try-count))
        (eshell/f (concat filename "*") dir 1))
       ((or (null try-count) (= 1 try-count))
        (eshell/f (concat "*" filename) dir 2))
       (t "")))))

(defun eshell/ef (filename &optional dir)
  "Searches for the first matching filename and loads it into a
file to edit."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (find-file file)))

(defun eshell/find (&rest args)
  "Wrapper around the ‘find’ executable."
  (let ((cmd (concat "find " (string-join args))))
    (shell-command-to-string cmd)))

;;------------------------------------------------------------------------
;; Special Prompt
;;
;; from https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#special-prompt

(defun rkh/get-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (not (file-remote-p pwd))
             (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
           (git-branch (s-trim git-output)))
      git-branch)))

(setq eshell-prompt-function
      (function
       (lambda ()
         (let* ((pwd (eshell/pwd))
                (directory (abbreviate-file-name (eshell/pwd)))
                (branch (rkh/get-git-branch-string pwd))
                (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
                (for-bars `(:weight bold))
                (for-user (if dark-env `(font-lock-type-face :weight bold)
                            '(font-lock-type-face :weight bold)))
                            ;;`(:foreground "dark orange" :weight bold)))
                (for-host (if dark-env `(:foreground "light red" :weight bold)
                            `(:weight bold)))
                (for-dir (if dark-env 'eshell-ls-directory
                           `(font-lock-function-name-face :weight bold)))
                (for-git (if dark-env `(:foreground "green" :weight bold)
                           '(font-lock-string-face :weight ultra-bold)))
                (for-nz-status (if dark-env
                                   `(:foreground "#ff4444" :weight bold)
                                 `(:foreground "red" :weight normal))))
           (concat "\n"
            (propertize user-login-name 'face for-user)
            (propertize " at " 'face nil)
            (propertize (downcase system-name) 'face for-host)
            (propertize " in " 'face nil)
            (propertize directory 'face for-dir)
            (when branch
              (concat (propertize " on " 'face nil)
                      (propertize branch 'face for-git)))
            "\n"
            (let (($? eshell-last-command-status))
              (propertize (format "[%s]" $?) 'face (if (> $? 0) for-nz-status)))
            " "
            (propertize (if (= (user-uid) 0) "#" "$") 'face `(:weight ultra-bold))
            " ")))))

(setq eshell-prompt-regexp "^[^#$]*[#$] ")
(setq eshell-highlight-prompt t)

(provide 'setup-eshell)
