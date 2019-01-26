;; Put the following in ~/.Xresources:
;;
;;   Emacs.font: terminus-12
;;   Emacs.toolBar: off
;;   Emacs.verticalScrollBars: off
;;   Emacs.horizontalScrollBars: off
;;
;; or uncomment the following:
;;
;; (when (display-graphic-p)
;;   (set-face-attribute 'default nil :height 100)  ; just changes size
;;   (set-face-attribute 'default nil :font "monospace:antialias=standard:size=13")
;;   ;; or replace "monospace" in the above with your fav font.
;;   (tool-bar-mode -1)
;;   (scroll-bar-mode -1)
;;   (and (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1)))
;;
;; Hint/Reminder: `M-x describe-font RET` will display your current
;; font settings.

(and (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Display date and time right-justified on the modeline.
;;   - From Alan Schmitt's (https://github.com/brabalan) comment on
;;     https://github.com/jonathanchu/emacs-powerline/issues/27#issuecomment-35736610
(display-time-mode 1)
(setq display-time-format  "%a %e %b %R"
      global-mode-string   (remove 'display-time-string global-mode-string)
      mode-line-end-spaces (list (propertize " "
                                   'display '(space :align-to (- right 15)))
                                 'display-time-string))
(display-time-update)

;; Set transparency to 90%
(when (display-graphic-p)
  (defun transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque: ")
    (set-frame-parameter (selected-frame) 'alpha value))
  (set-frame-parameter (selected-frame) 'alpha 90))

(provide 'setup-look)
