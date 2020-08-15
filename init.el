(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages
      '(highlight-indentation
        gnu-elpa-keyring-update
        magit-org-todos
        py-isort
        smex
        company
        company-jedi
        drag-stuff
        projectile
        avy
        magit
        counsel
        counsel-projectile
        swiper
        irony
        switch-window
        solaire-mode
        neotree
        elpy
        jedi
        flycheck
        pomidor
        alert
        dash
        multiple-cursors
        doom-themes
        persp-projectile
        doom-modeline))
(package-install-selected-packages)


;; Removes toolbar, scroll bar and menu bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Maximizes emacs on startup and removes title bar (borderless fullscreen)
(set-frame-parameter nil 'fullscreen 'maximized)

;; No tabs, please
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun agenda-hook ()
  (split-window-right)
  (let ((org-agenda-window-setup 'other-window))
    (org-agenda nil "a")))

(add-hook 'window-setup-hook #'agenda-hook)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq initial-buffer-choice "~/agenda.org")

(setq-default
 whitespace-style '(face tabs tab-mark spaces space-mark trailing))

(global-whitespace-mode 1)

;; duplicate row by C-c C-d
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

;; pomodoro binding
(global-set-key (kbd "<f12>") #'pomidor)
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil)

;; shell in current buffer
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

;; horizontal mouse scroll
(global-set-key (kbd "<mouse-7>") '(lambda ()
                                     (interactive)
                                     (scroll-left 4)))
(global-set-key (kbd "<mouse-6>") '(lambda ()
                                     (interactive)
                                     (scroll-right 4)))

;; magit global
(global-set-key (kbd "C-x g") 'magit-status)

;; show lines numbers [linum may be slow]
(line-number-mode t)
(column-number-mode t)

;; undo-fu
(require 'undo-fu)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

;; org-babel
(org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)))
(setq org-confirm-babel-evaluate nil)

;; ido-mode
;; (require 'ido)
;; (ido-mode t)

;; theme
(load-theme 'doom-dracula t)

;; doom-modeline
(require 'doom-modeline)
(doom-modeline-mode 1)

;; solaire
(require 'init-solaire)

;; python-mode
(require 'init-python)

;; window-select
(require 'init-windows)

;; projectile
(require 'init-projectile)

;; ivy-mode
(require 'init-ivy)

;; drag-stuff
(require 'init-drag)

;; benchmark
(require 'init-benchmark)

;; multiple-cursors
(require 'init-multiple-cursors)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (persp-projectile flx-ido counsel-projectile elpy highlight-indentation gnu-elpa-keyring-update magit-org-todos py-isort smex company drag-stuff projectile avy magit counsel swiper irony switch-window solaire-mode neotree jedi flycheck doom-themes doom-modeline))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
