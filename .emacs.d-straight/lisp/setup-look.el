(menu-bar-mode -1)

;; Put the following in ~/.Xresources:
;;
;;   Emacs.font: terminus-12
;;   Emacs.toolBar: off
;;   Emacs.verticalScrollBars: off
;;   Emacs.horizontalScrollBars: off
;;
;; or uncomment the following:
;;
(when (display-graphic-p)
  ;; (set-face-attribute 'default nil :height 100)  ; just changes size
  ;; (set-face-attribute 'default nil :font "terminus-12:antialias=standard")
  ;; or replace "monospace" in the above with your fav font.
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (and (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1)))
;;
;; Hint/Reminder: `M-x describe-font RET` will display your current
;; font settings.

(when (display-graphic-p)
  ;; Old way of setting fonts. (I don't think this works for the initial frame in Widnoze.)
  ;; (set-face-attribute 'default nil :font (font-spec :family "Iosevka SS12 Medium" :size 11.8))
  ;; (set-face-attribute 'default nil :font "Hack:size=14")

  (unless (eq 'windows-nt system-type)
    ;;(set-face-attribute 'default nil :font "Hack:size=16")
    ;; (set-face-attribute 'default nil :font (font-spec :family "Iosevka Term" :size 11.7 :weight 'normal))
    ;; (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka Term" :size 11.7 :weight 'normal))
    ;; (set-face-attribute 'variable-pitch nil :font (font-spec :family "Cantarell" :size 14.0 :weight 'normal))

    (set-face-attribute 'default nil :font "Iosevka Term" :height 135)  ;:font "Fira Code Retina"
    (set-face-attribute 'fixed-pitch nil :font "Iosevka Term" :height 130)  ;:font "Fira Code Retina"
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'regular)
    )

  (when (eq 'windows-nt system-type)
    (setq rkh/normal-font (font-spec :family "Iosevka SS12 Medium" :size 16)
          rkh/big-font (font-spec :family "Iosevka SS12 Medium" :size 24))
    ;; Other nice fonts:
    ;; - "Hack:size=14"
    ;; - (font-spec :family "Consolas" :size 12.8)

    (defun rkh/set-font (fontspec)
      (set-face-attribute 'default nil :font fontspec)
      (set-frame-font fontspec nil t)
      (push `(font . ,(frame-parameter nil 'font)) default-frame-alist))
    (defun rkh/normal-font () (interactive) (rkh/set-font rkh/normal-font))
    (defun rkh/big-font () (interactive) (rkh/set-font rkh/big-font))
    (rkh/normal-font))
  )

(use-package doom-modeline
  :straight (:type git :host github :repo "seagle0128/doom-modeline")
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 15)))

;; Beacon highlights the current line in the buffer
(use-package beacon
   :straight t
   :config
   (beacon-mode 1))
(global-hl-line-mode t)

;; Display date and time right-justified on the modeline.
;;   - From Alan Schmitt's (https://github.com/brabalan) comment on
;;     https://github.com/jonathanchu/emacs-powerline/issues/27#issuecomment-35736610
(display-time-mode 1)
(setq display-time-format  "%a %e %b %R"
      global-mode-string   (remove 'display-time-string global-mode-string)
      mode-line-end-spaces (list (propertize " "
                                   'display '(space :align-to (- right 16)))
                                 'display-time-string))
(display-time-update)

;; Set transparency to 90%
(when (display-graphic-p)
  (defun transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque: ")
    (set-frame-parameter (selected-frame) 'alpha value))
  (set-frame-parameter (selected-frame) 'alpha 100))

(provide 'setup-look)
