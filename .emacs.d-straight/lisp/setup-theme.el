;; Source: https://github.com/emacs-jp/replace-colorthemes
;;(comment
  (straight-use-package
   '(replace-colorthemes :type git :host github :repo "emacs-jp/replace-colorthemes"))
;;)
;; Some of my fav (replace-)colorthemes.
;;(load-theme 'lawrence t)  ; #3 -- nice green on black
;;(load-theme 'midnight t)
;;(load-theme 'oswald t)
;;(load-theme 'retro-green t)  ; doesn't work (neither does retro-orange)
;;(load-theme 'charcoal-black t)  ; #2

;; Source: https://github.com/sellout/emacs-color-theme-solarized.git
(comment
  (straight-use-package
   '(emacs-color-theme-solarized :type git :host github :repo "sellout/emacs-color-theme-solarized"))
)
;;(load-theme 'solarized t t)  ; load, but don't enable
;;(set-frame-parameter nil 'background-mode 'light)  ; or 'light (but light is too bright)
;;(enable-theme 'solarized)

;; Source: https://github.com/jd/naquadah-theme.git
(use-package naquadah-theme :defer t
  :straight (naquadah-theme :type git :host github :repo "jd/naquadah-theme"))

(use-package solarized-emacs :defer t
  :straight (solarized-emacs :type git :host github :repo "bbatsov/solarized-emacs"))

;; Other themes I like.
(use-package cyberpunk-theme :straight t :defer t)
(use-package plan9-theme :straight t :defer t)
(use-package occidental-theme :straight t :defer t)  ; like plan9 but more contrast
(use-package eclipse-theme :straight t :defer t
  :config
  ;; Overrride the default background of eclipse (which is white) with
  ;; a yellowish background (plan9 look) stolen from occidental.
  (let ((class '((class color) (min-colors 88) (background light))))
    (custom-theme-set-faces
     'eclipse
     `(default ((,class (:background "#FFFFEA" :foreground "black")))))))

(use-package doom-themes :defer t
  :straight t
  :config
  (load-theme 'doom-vibrant t))

(use-package modus-vivendi-theme :defer t
  :straight
  (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes"))
(use-package modus-operandi-theme :defer t
  :straight
  (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes")
  :config
  (setq modus-operandi-theme-slanted-constructs nil)
  (setq modus-operandi-theme-bold-constructs nil))

(setq rkh/dark-theme 'naquadah
      rkh/light-theme 'modus-operandi) ;;'solarized-light)

(defun load-dark-theme ()
  (interactive)
  (load-theme rkh/dark-theme t))

(defun load-light-theme ()
  (interactive)
  (load-theme rkh/light-theme t))

(load-light-theme)

(provide 'setup-theme)
