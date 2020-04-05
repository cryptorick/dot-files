;;; Special Winders Setup ... because Windows has to be different.
;;;
;;; For now, just set these up manually.

;; If on a Windoze system, wire in helpful programs that Windoze
;; doesn't have by default.
(setenv "PATH"
        (concat (getenv "PATH")
                "\;C:\\msys64\\usr\\bin"))

(setq magit-git-executable (expand-file-name "~/AppData/Local/Programs/Git/cmd/git.exe"))

;; (setq my/msys-bin-dir "C:/msys64/usr/bin/")

;; (setq ediff-diff-program (concat my/msys-bin-dir "diff.exe")
;;       ediff-diff3-program (concat my/msys-bin-dir "diff3.exe"))

;;----------------------------------------------------------------------
;; M-x shell stuff in Winders

(use-package fakecygpty
  ;; "This package supplies cygwin's pty feature for NTEmacs. If you
  ;; want to use the command requires pty such as bash, try this."
  :straight (fakecygpty :type git :host github :repo "d5884/fakecygpty")
  :config
  (fakecygpty-activate))

(setq explicit-shell-file-name "C:/msys64/usr/bin/mksh.exe")
(setq shell-file-name "mksh")
(setq explicit-mksh.exe-args '("-l" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;;----------------------------------------------------------------------
;; If you use third party programs like AltDrag (or Flexiglass for Mac
;; OS X) to grab and drag windows around, it doesn't work for Emacs
;; windows.  This hack from
;; https://nulana.zendesk.com/hc/en-us/articles/115000711809-Flexiglass-can-t-move-Emacs-windows
;; will get Emacs itself to to that. Warning: it only seems to work
;; when you (quickly) press Super, then Shift-mouse-1.
(define-key global-map (kbd "<S-s-down-mouse-1>")
  (lambda (event)
    (interactive "e")
    (let* ((frame (selected-frame))
           (fx (frame-parameter frame 'left))
           (fy (frame-parameter frame 'top))
           (sx (car (posn-x-y (second event))))
           (sy (cdr (posn-x-y (second event)))))
      (track-mouse
        (loop for movement = (read-event)
              while (mouse-movement-p movement)
              for mx = (car (posn-x-y (second movement)))
              for my = (cdr (posn-x-y (second movement)))
              for dx = (- mx sx)
              for dy = (- my sy)
              do
              (incf fx dx)
              (incf fy dy)
              (set-frame-position frame fx fy))))))

(provide 'setup-windoze)
