;;; Special Winders Setup ... because Windows has to be different.
;;;
;;; For now, just set these up manually, hardwire.

;; If on a Windoze system, wire in helpful programs that Windoze
;; doesn't have by default.
(setenv "PATH"
        (concat (getenv "PATH")
                "\;C:\\msys64\\usr\\bin"))

(setq magit-git-executable "C:/opt/PortableGit/cmd/git.exe")

(provide 'setup-windoze)
