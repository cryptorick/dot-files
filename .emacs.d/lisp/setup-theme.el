(add-to-list 'custom-theme-load-path
             (file-name-as-directory
              ;; Source: https://github.com/emacs-jp/replace-colorthemes
              (concat user-emacs-directory "contrib/replace-colorthemes")))
;; Some of my fav (replace-)colorthemes.
;;(load-theme 'lawrence t)  ; #3 -- nice green on black
;;(load-theme 'midnight t)
;;(load-theme 'oswald t)
;;(load-theme 'retro-green t)  ; doesn't work (neither does retro-orange)
;;(load-theme 'charcoal-black t)  ; #2

(add-to-list 'custom-theme-load-path
             (file-name-as-directory
              ;; Source: https://github.com/jd/naquadah-theme.git
              (concat user-emacs-directory "contrib/naquadah-theme")))
;;(load-theme 'naquadah t) ; #1

(add-to-list 'custom-theme-load-path
             (file-name-as-directory
              ;; Source: https://github.com/sellout/emacs-color-theme-solarized.git
              (concat user-emacs-directory "contrib/emacs-color-theme-solarized")))
;;(load-theme 'solarized t t)  ; load, but don't enable
;;(set-frame-parameter nil 'background-mode 'light)  ; or 'light (but light is too bright)
;;(enable-theme 'solarized)

;; Other themes I like.
(use-package cyberpunk-theme :ensure t :disabled)
(use-package plan9-theme :ensure t :disabled)
(use-package occidental-theme :ensure t :disabled)  ; like plan9 but more contrast
(use-package eclipse-theme :ensure t
  :config
  ;; Overrride the default background of eclipse (which is white) with
  ;; a yellowish background (plan9 look) stolen from occidental.
  (let ((class '((class color) (min-colors 88) (background light))))
    (custom-theme-set-faces
     'eclipse
     `(default ((,class (:background "#FFFFEA" :foreground "black")))))))

(provide 'setup-theme)
