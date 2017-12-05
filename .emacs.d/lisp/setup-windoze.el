;;; Special Winders Setup ... because Windows has to be different.
;;;
;;; For now, just set these up manually.

;; If on a Windoze system, wire in helpful programs that Windoze
;; doesn't have by default.
(setenv "PATH"
        (concat (getenv "PATH")
                "\;C:\\msys64\\usr\\bin"))

(setq magit-git-executable "C:/opt/PortableGit/cmd/git.exe")

;; (setq my/msys-bin-dir "C:/msys64/usr/bin/")

;; (setq ediff-diff-program (concat my/msys-bin-dir "diff.exe")
;;       ediff-diff3-program (concat my/msys-bin-dir "diff3.exe"))

;;----------------------------------------------------------------------
;; This is setup-look.el-type logic but only doing this on Windoze
;; for now.

;; This can only work on if emacs is starting normally (i.e., in
;; *non*-server-client mode).
(when (display-graphic-p)
  (set-face-attribute 'default nil :font (concat "Consolas" ":antialias=true:size=14")))

;; This id for when emacs starts in server-client mode.
(let ((font (concat "Consolas" "-" "10.5")))
  (require 'cl-lib)  ; ensure cl-pushnew
  (cl-pushnew (cons 'font font) default-frame-alist
              :key #'car))

(provide 'setup-windoze)
