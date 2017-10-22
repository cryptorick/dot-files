;; Stolen from
;; https://github.com/eschulte/emacs24-starter-kit/blob/master/starter-kit-misc-recommended.org

;; Uncomment one of the following for your fav browser.

(setq browse-url-browser-function 'browse-url-firefox)
;;(setq browse-url-browser-function 'browse-default-macosx-browser)
;;(setq browse-url-browser-function 'browse-default-windows-browser)
;;(setq browse-url-browser-function 'browse-default-kde)
;;(setq browse-url-browser-function 'browse-default-epiphany)
;;(setq browse-url-browser-function 'browse-default-w3m)
;;(setq browse-url-browser-function 'browse-url-generic
;;      browse-url-generic-program "conkeror")

(global-set-key [M-S-mouse-1] 'browse-url-at-mouse)

(provide 'setup-browser)
