;; NOTE: init.el is now generated from README.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!
;; Font size
(defvar cfg/default-font-size 120)
(defvar cfg/default-variable-font-size 120)

;; Frame transparency
(defvar cfg/frame-transparency '(97 . 97))

(setq history-length 300)
(put 'minibuffer-history 'history-length 100)
(put 'kill-ring 'history-length 40)
(setq warning-minimum-level :emergency)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 30)
  (auto-package-update-maybe))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(when (equal system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-key-is-meta nil
          mac-option-modifier 'none)
    (setq ns-right-option-modifier 'super)
    (setenv "LIBRARY_PATH"
            (string-join
             '("/opt/homebrew/opt/gcc/lib/gcc/13"
               "/opt/homebrew/opt/libgccjit/lib/gcc/13"
               "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin21/13")
             ":"))
    (setq insert-directory-program "/opt/homebrew/bin/gls")
    (custom-set-variables '(epg-gpg-program  "/opt/homebrew/bin/gpg"))
    (setq epa-pinentry-mode 'loopback))

(setq ad-redefinition-action 'accept)

(use-package page-break-lines)
(use-package all-the-icons
  :config
  '(lsp-treemacs-theme "all-the-icons"))

(let ((font-dest (cl-case window-system
                 (x  (concat (or (getenv "XDG_DATA_HOME") ;; Default Linux install directories
                                 (concat (getenv "HOME") "/.local/share"))
                             "/fonts/"))
                 (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                 (ns (concat (getenv "HOME") "/Library/Fonts/" )))))
(unless (file-exists-p (concat font-dest "all-the-icons.ttf"))
  (all-the-icons-install-fonts))
(unless (file-exists-p (concat font-dest "NFM.ttf"))
  (nerd-icons-install-fonts)))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(setq dashboard-startup-banner 'logo)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-center-content t)
(setq dashboard-set-footer nil)

;; (setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)
(global-visual-line-mode t)
;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha cfg/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,cfg/frame-transparency))

;; Set frame fulscreen
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default
 whitespace-style '(face tabs tab-mark spaces space-mark trailing))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(setq split-width-threshold 9999) ;; Horizontal split by default

(set-face-attribute 'default nil :font "Fira Code Retina" :height cfg/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height cfg/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height cfg/default-variable-font-size :weight 'regular)

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom
    (doom-modeline-height 15)
    (doom-modeline-buffer-file-name-style 'truncate-upto-project))

(with-eval-after-load "doom-modeline"
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode process vcs "  ")))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package ivy
  :diminish
  :bind (("C-S-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("C-x b" . 'counsel-switch-buffer))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; Counsel should remeber last M-x commands (make it smarter)
(use-package smex)

(setq tab-bar-mode t)
  (setq tab-bar-show nil)
  ;; (setq tab-bar-new-tab-choice "*dashboard*")
;; Rebind C-x t to C-x w for similar and convenient work with eyebrowse's C-c w
(global-unset-key (kbd "C-x t"))
(define-key ctl-x-map "w" tab-prefix-map)
;; Also, set C-c arrow to switch between tabs
(global-set-key (kbd "C-c <left>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-c <right>") 'tab-bar-switch-to-next-tab)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(use-package highlight-indent-guides
:hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
:init
(setq highlight-indent-guides-method 'character)
:config
(defun +indent-guides-init-faces-h (&rest _)
  (when (display-graphic-p)
    (highlight-indent-guides-auto-set-faces)))

;; HACK `highlight-indent-guides' calculates its faces from the current theme,
;;      but is unable to do so properly in terminal Emacs, where it only has
;;      access to 256 colors. So if the user uses a daemon we must wait for
;;      the first graphical frame to be available to do.
(add-hook 'doom-load-theme-hook #'+indent-guides-init-faces-h)
;; `highlight-indent-guides' breaks when `org-indent-mode' is active
(add-hook 'org-mode-local-vars-hook
  (defun +indent-guides-disable-maybe-h ()
    (and highlight-indent-guides-mode
         (bound-and-true-p org-indent-mode)
         (highlight-indent-guides-mode -1)))))

(use-package ace-window
  :init
  (global-set-key (kbd "C-x o") 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-ignore-current t)
  (setq aw-ignore-on nil))

(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("ukrainian-computer"))
  :config
  (reverse-im-mode t))

(use-package avy)
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-word-0)
(global-set-key (kbd "C-]") 'avy-goto-line)
(global-set-key (kbd "C-}") 'avy-goto-word-0)

(defun cfg/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; (add-hook 'org-mode-hook (lambda ()
;;                            "Beautify Org Checkbox Symbol"
;;                            (push '("[ ]" .  "☐") prettify-symbols-alist)
;;                            (push '("[X]" . "☑" ) prettify-symbols-alist)
;;                            (push '("[-]" . "❍" ) prettify-symbols-alist)
;;                            (prettify-symbols-mode)))

;; (defface org-checkbox-done-text
;;   '((t (:foreground "#71696A" :strike-through t)))
;;   "Face for the text part of a checked org-mode checkbox.")

;; (font-lock-add-keywords
;;  'org-mode
;;  `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
;;     1 'org-checkbox-done-text prepend))
;;  'append)
;; Replace list hyphen with dot
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun cfg/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :hook (org-mode . cfg/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)
      ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Dropbox/org_files/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Dropbox/org_files/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Dropbox/org_files/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Dropbox/org_files/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Dropbox/org_files/metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (cfg/org-font-setup))
(setq org-startup-folded t)
(add-hook 'org-mode-hook 'org-hide-block-all)
(setq org-startup-with-inline-images "inlineimages")
(add-hook 'org-babel-after-execute-hook
        (lambda ()
          (when org-inline-image-overlays
            (org-redisplay-inline-images))))
(setq org-image-actual-width (/ (display-pixel-width) 2))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun cfg/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . cfg/org-mode-visual-fill))

(setq org-export-with-sub-superscripts nil)
(setq org-export-backends '(ascii html md odt))

(use-package restclient)
(use-package ob-restclient)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (shell . t)
    (sql . t)
    (js . t)
    (plantuml . t)
    (python . t)
    (restclient . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)
(push '("plantuml" . plantuml) org-src-lang-modes)
(setq org-confirm-babel-evaluate nil)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("rust" . "src rust"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("plant" . "src plantuml"))
(add-to-list 'org-structure-template-alist '("rest" . "src restclient"))

;; Automatically tangle our Emacs.org config file when we save it
(defun cfg/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cfg/org-babel-tangle-config)))

(use-package plantuml-mode)
(setq plantuml-jar-path "~/plantuml.jar")
(setq org-plantuml-jar-path "~/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

(setq org-roam-v2-ack t)
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Dropbox/org_files/org_roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"   . completion-at-point))
  :config
  (org-roam-setup))

(use-package ob-async)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c c")
  ;; (setq-default lsp-modeline-diagnostics-enable nil)
  ;; (setq lsp-modeline-code-actions-enable nil)
  :custom
  ;; (lsp-eldoc-render-all t)
  ;; (lsp-idle-delay 0.500)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 10 1024 1024)) ;; 10Mb
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  :hook ((python-mode . lsp)
         (vue-mode . lsp)
         (rust-mode . lsp)
         (js-mode . lsp))
  :config
  (setq lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-pylsp-configuration-sources ["flake8"])
  ;; (setq lsp-pylsp-plugins-flake8-enabled nil)
  (setq lsp-pylsp-plugins-mccabe-enabled nil)
  (setq lsp-pylsp-plugins-pydocstyle-enabled nil)
  (setq lsp-pylsp-plugins-pyflakes-enabled nil)
  (setq lsp-pylsp-plugins-pylint-enabled nil)
  (setq lsp-pylsp-plugins-autopep8-enabled t)
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                     :major-modes '(python-mode)
                     :remote? t
                     :server-id 'pyls-remote))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  ;; :bind ("C-c c f" . lsp-ui-doc-focus-frame)
  ;; :bind (:map mode-specific-map ("c d" . lsp-ui-doc-focus-frame))
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  )

(use-package lsp-ivy)

(use-package yasnippet
:ensure t
:config
  (yas-global-mode 1)
)
(use-package yasnippet-snippets)         ; Collection of snippets
(use-package py-snippets
:ensure t
:after yasnippet
:config
(py-snippets-initialize))

(use-package dap-mode
  :defer
  :custom
  (dap-auto-configure-mode t "Automatically configure dap.")
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
  (dap-python-debugger 'debugpy)

  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  :config
  (dap-ui-mode 1)
  ;; Set up Node debugging
  ;; (require 'dap-node)
  ;; (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  ;; Set up python debugging
  (require 'dap-python)
  ;; dap-mode for javascript
  ;; you only need call dap-firefox-setup after requiring dap-firefox
  ;; Make sure that dap-firefox-debug-program is pointing to the proper file.
  (require 'dap-firefox))

;; (add-hook 'dap-stopped-hook
;;         (lambda (arg) (call-interactively #'dap-hydra)))
(global-set-key (kbd "C-c c b") 'dap-breakpoint-toggle)
(global-set-key (kbd "C-c c d") 'dap-debug)

;; (with-eval-after-load 'dap-ui
;;   (setq dap-ui-buffer-configurations
;;         `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.32)  (window-height . 0.80)))
;;           (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.32) (window-height . 0.10)))
;;           (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.32) (window-height . 0.10)))
;;           (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
;;           (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
;;           (,dap-ui--repl-buffer . ((side . right) (slot . 2) (window-width . 0.45))))))

(use-package typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . lsp-deferred)
    :config
    (setq typescript-indent-level 2))

(use-package vue-mode)

(use-package py-isort)

(defun py-local-keys()
  (local-set-key (kbd "C-c c i") 'py-isort-buffer)
  (local-set-key (kbd "C-c c e") 'flycheck-list-errors)
  (local-set-key (kbd "<C-backspace>") 'my-backward-delete-word))

(add-hook 'python-mode-hook 'py-local-keys)
(add-hook 'python-mode-hook 'yas-minor-mode-on)

(use-package py-yapf)

;; (use-package pipenv
  ;;     :hook (python-mode . pipenv-mode)
  ;;     :init
  ;;     (setq
  ;;      pipenv-projectile-after-switch-function
  ;;      #'pipenv-projectile-after-switch-extended))

  ;; (add-hook 'python-mode-hook #'pipenv-mode)

(use-package pyvenv
    :ensure t
    :init
    (setenv "WORKON_HOME" "~/.virtualenvs/")
    :config
    (setq pyvenv-mode-line-indicator
          '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
    (pyvenv-mode t)
    ;; Set correct Python interpreter
    (setq pyvenv-post-activate-hooks
          (list (lambda ()
                  (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
    (setq pyvenv-post-deactivate-hooks
          (list (lambda ()
                  (setq python-shell-interpreter "python3")))))

;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

;; ((python-mode . ((eval . (lsp-register-custom-settings
;;                           '(("python.pythonPath" "/.../.venv/bin/python"
;;                              "python.venvPath" "/.../.venv")))))))

(with-eval-after-load 'projectile
  (defvar start-file-path (concat (projectile-project-root) "app.py")
    "The path to the Python project's start file.")

  (defun run-venv-python-file ()
    "Runs python module using start-file-path global variable"
    (let ((default-directory (projectile-project-root)))
      (async-shell-command (concat pyvenv-virtual-env "bin/python3 " start-file-path))
      (other-window 1)
      (rename-buffer (concat (projectile-project-name) " | shell")))
    )

  (defun run-project ()
    "Runs the start module of the current Python project."
    (interactive)
    (let* ((project-root (projectile-project-root)))
      (if (yes-or-no-p (format "Use previously selected start file?\n%s" start-file-path))
          (run-venv-python-file)
        (let ((selected-filepath (read-file-name "Select start file: " project-root)))
          (setq start-file-path selected-filepath)
          (run-venv-python-file))))))

(defun py-mode-specific-bindings ()
  (define-key python-mode-map (kbd "C-c C-c") 'run-project))

(add-hook 'python-mode-hook 'py-mode-specific-bindings)

(use-package lsp-java
  :init
  (defun jmi/java-mode-config ()
    (toggle-truncate-lines 1)
    (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")
    (lsp))

  :config
  ;; Enable dap-java
  (require 'dap-java)

  ;; Support Lombok in our projects, among other things
  (setq lsp-java-vmargs
        (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              (concat "-javaagent:" jmi/lombok-jar)
              (concat "-Xbootclasspath/a:" jmi/lombok-jar))
        lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules"
          ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
          "build")

        lsp-java-import-order '["" "java" "javax" "#"]
        ;; Don't organize imports on save
        lsp-java-save-action-organize-imports nil

        ;; Formatter profile
        lsp-java-format-settings-url
        (concat "file://" jmi/java-format-settings-file))

  :hook (java-mode . jmi/java-mode-config)

  :demand t
  :after (lsp lsp-mode dap-mode jmi-init-platform-paths))
  (add-hook 'java-mode-hook 'lsp-deferred)
  (add-hook 'java-mode-hook 'yas-minor-mode-on)

(use-package php-mode
  :ensure t
  )

(use-package web-mode
:mode
  (
   ".twig$"
   ".hbs$"
   ".html$"
   ".blade.php$"
   ".liquid$"
   ".ts$"
   )
)
(use-package rainbow-mode)

(use-package solidity-mode
  :config
  (setq solidity-comment-style 'slash))

(use-package solidity-flycheck
  :config
  (setq solidity-flycheck-solc-checker-active t)
  (setq solidity-flycheck-solium-checker-active t)
  ;; (setq solidity-flycheck-chaining-error-level ...)
  )
(add-hook 'solidity-mode-hook 'flycheck-mode)

(use-package company-solidity)

(use-package elisp-format)

(use-package rust-mode)
(add-hook 'rust-mode-hook
        (lambda () (setq indent-tabs-mode nil)))
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(use-package company
  :after lsp-mode
  :hook (after-init-hook . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(add-to-list 'company-backends '(company-capf company-dabbrev))
(with-eval-after-load 'company
  (define-key company-mode-map (kbd "<tab>") 'company-complete))

(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :init
  (setq flycheck-check-syntax-automatically '(save new-line)
        flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-standard-error-navigation t
        flycheck-deferred-syntax-check nil)
  )

(use-package treemacs
  :config
  (setq treemacs-python-executable (executable-find "python3")))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode +1)
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(setq projectile-indexing-method 'alien) ;; native hybrid alien
(setq projectile-sort-order 'recentf)

;; (global-set-key (kbd "C-x <left>") 'projectile-previous-project-buffer)
;; (global-set-key (kbd "C-x <right>") 'projectile-next-project-buffer)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :hook (magit-status-refresh-hook . magit-fetch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit
  :config
  (add-to-list 'forge-alist '("git.cbdev.site" "git.cbdev.site/api/v4" "git.cbdev.site"  forge-gitlab-repository))
  :custom
  (global-set-key (kbd "C->") 'mc/mark-next-like-this))

(use-package git-timemachine
   :ensure t
   :bind (("C-c g" . git-timemachine)))

(use-package diff-hl
  :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))
(global-diff-hl-mode)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-vc
  :init
  :config
  (define-ibuffer-column icon
    (:name "Icon" :inline t)
    (all-the-icons-icon-for-mode 'major-mode)))

(with-eval-after-load 'ibuffer
  ;; Display buffer icons on GUI
  ;; (define-ibuffer-column icon (:name "  ")
  ;;   (let ((icon (if (and (buffer-file-name)
  ;;                        (all-the-icons-auto-mode-match?))
  ;;                   (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
  ;;                 (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
  ;;     (if (symbolp icon)
  ;;         (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
  ;;       icon)))

  ;; Redefine size column to display human readable size
  (define-ibuffer-column size
    (:name "Size"
     :inline t
     :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

 (use-package ibuffer-projectile
  ;; Group ibuffer's list by project root
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix
            (concat (all-the-icons-octicon
                     "file-directory"
                     :face ibuffer-filter-group-name-face
                     :v-adjust -0.05)
                    " "
          "Project: ")))

(use-package eyebrowse
  :init  
  (setq eyebrowse-keymap-prefix (kbd "C-c w"))
  :ensure t
  :config
  (eyebrowse-mode t)
  )

(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package json-mode)

(use-package move-text
  :init
  (move-text-default-bindings))

(use-package vlf)

(use-package yafolding)
(add-hook 'json-mode-hook (lambda () (yafolding-mode)))
(add-hook 'python-mode-hook (lambda () (yafolding-mode)))

(define-key yafolding-mode-map (kbd "C-x M-y") 'yafolding-toggle-all)
(define-key yafolding-mode-map (kbd "C-x C-y") 'yafolding-toggle-element)

(use-package tramp ;; with use-package
   :defer t
   :config
   (setq-default tramp-default-method "scpx")
   (setq vc-handled-backends '(Git))
   (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                              vc-ignore-dir-regexp tramp-file-name-regexp))
   (setq tramp-copy-size-limit nil)
   (setq tramp-completion-reread-directory-timeout t)
   (setq tramp-verbose 0)
   (setq make-backup-files nil)
   (setq create-lockfiles nil)
   )

 ;; (use-package vagrant-tramp)

(use-package tramp-term)
(use-package counsel-tramp
   :bind (("C-x t" . counsel-tramp)))

(setq counsel-tramp-control-master t)

(use-package docker) ;; manage docker containers
;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd

(use-package imenu-list
  :ensure t
  :bind ("C-c c l i" . imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t))
(global-set-key (kbd "C-c c l o") 'occur)

(use-package prettier)

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1))

(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil)
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))

(use-package devdocs)

(setq ediff-split-window-function (quote split-window-horizontally))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)

(use-package csv-mode)

(global-set-key (kbd "C-x C-l") 'list-processes)
(define-key process-menu-mode-map (kbd "C-k") 'my/delete-process-at-point)

(defun my-delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

(use-package vterm
  :commands vterm
  :ensure t
  :config
  (setq vterm-always-compile-module t)
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt

  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun shell-toggle (&optional command)
  "Toggle a persistent terminal popup window.
  If popup is visible but unselected, selected it.
  If popup is focused, delete it."
  (interactive)
  (let ((buffer
         (get-buffer-create
          (format "*shell-popup:%s*"
                  (if (bound-and-true-p persp-mode)
                      (safe-persp-name (get-current-persp))
                    "main"))))
        (dir default-directory))
    (if-let (win (get-buffer-window buffer))
        (if (eq (selected-window) win)
            (let (confirm-kill-processes)
              (delete-window win))
          (select-window win)
          (goto-char (point-max)))
      (with-current-buffer (pop-to-buffer buffer)
        (if (not (eq major-mode 'shell-mode))
            (shell buffer)
          (cd dir)
          (run-mode-hooks 'shell-mode-hook))))))
(global-set-key (kbd "C-c t") 'shell-toggle)

(defun term-toggle (&optional command)
  "Toggle a persistent terminal popup window.
If popup is visible but unselected, selected it.
If popup is focused, delete it."
  (interactive)
  (let ((buffer
         (get-buffer-create
          (format "*term-popup:%s*"
                  (if (bound-and-true-p persp-mode)
                      (safe-persp-name (get-current-persp))
                    "main"))))
        (dir default-directory))
    (if-let (win (get-buffer-window buffer))
        (if (eq (selected-window) win)
            (let (confirm-kill-processes)
              (delete-window win))
          (select-window win)
          (goto-char (point-max)))
      (with-current-buffer (pop-to-buffer buffer)
        (if (not (eq major-mode 'term-mode))
            (term "/bin/zsh")
          (cd dir)
          (run-mode-hooks 'term-mode-hook))))))
(global-set-key (kbd "C-c C-t") 'term-toggle)
(add-hook 'python-mode-hook
      (lambda () (local-set-key (kbd "C-c C-t") 'term-toggle)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-laGh1v --group-directories-first"))
)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
         ("," . dired-clean-directory)
         ("." . dired-hide-dotfiles-mode))
)
;; ;; Make dired open in the same window when using RET or ^
;; (put 'dired-find-alternate-file 'disabled nil) ; disables warning
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
;; (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; (use-package dirvish
;;   :init
;;   (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
;;    '(("h" "~/"                          "Home")
;;      ("d" "~/Documents/"                "Documents")
;;      ("r" "/"                           "Root")))
;;   :config
;;   ;; (dirvish-peek-mode) ; Preview files in minibuffer
;;   ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
;;   (setq dirvish-mode-line-format
;;         '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes
;;         '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
;;   (setq delete-by-moving-to-trash t)
;;   (setq dired-listing-switches
;;         "-l --almost-all --human-readable --group-directories-first --no-group")
;;   :bind
;;   (("C-x f" . dirvish-fd)
;;   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
;;    ("a"   . dirvish-quick-access)
;;    ("f"   . dirvish-file-info-menu)
;;    ("y"   . dirvish-yank-menu)
;;    ("N"   . dirvish-narrow)
;;    ("^"   . dirvish-history-last)
;;    ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;;    ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;;    ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-l" . dirvish-ls-switches-menu)
;;    ("M-m" . dirvish-mark-menu)
;;    ("M-t" . dirvish-layout-toggle)
;;    ("M-s" . dirvish-setup-menu)
;;    ("M-e" . dirvish-emerge-menu)
;;    ("M-j" . dirvish-fd-jump)))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(if (executable-find "hunspell") 
    (progn 
      (setq ispell-program-name "hunspell") 
        (setq ispell-really-aspell nil) 
        (setq ispell-really-hunspell t) 
        (setq ispell-dictionary "en-ru")) ) 
(setq default-major-mode 'text-mode)
;; (dolist (hook '(text-mode-hook)) 
;;   (add-hook hook (lambda () 
;;                    (flyspell-mode 1))) )
(global-set-key (kbd "C-c s") 'ispell)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package elfeed)
(use-package elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
(global-set-key (kbd "C-c i") 'elfeed)

(use-package osm
  :bind (("C-c m h" . osm-home)
         ("C-c m s" . osm-search)
         ("C-c m v" . osm-server)
         ("C-c m t" . osm-goto)
         ("C-c m x" . osm-gpx-show)
         ("C-c m j" . osm-bookmark-jump))

  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information

  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))

(use-package dwim-shell-command
  :ensure t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (defun dwim-shell-commands-image-exif-metadata ()
  "View EXIF metadata in image(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "View EXIF"
   "exiftool '<<f>>'"
   :utils "exiftool")))

(require 'request)

(defun chatgpt-get-api-key ()
  (let* ((file-contents (with-temp-buffer
                              (insert-file-contents "~/.emacs.d/secrets.txt.gpg")
                              (buffer-substring-no-properties (point-min) (point-max))))
             (lines (split-string file-contents "\n" t))
             (my-api-key-line (cl-find-if (lambda (line) (string-match-p "^emacs-chatgpt-api-key=" line)) lines))
             (my-api-key-value (when my-api-key-line
                                 (substring my-api-key-line (1+ (string-match "=" my-api-key-line))))))

        (if my-api-key-value
            my-api-key-value
          (error "my-api-key not found in file"))))

;; (defun chatgpt-extract-content-text (response-data)
;;   "Extract content text from response-data."
;;   (let ((choices (cdr (assoc 'choices response-data))))
;;     (mapconcat (lambda (choice)
;;                  (let ((message (cdr (assoc 'message choice))))
;;                    (cdr (assoc 'content message))))
;;                choices
;;                "\n")))

;; (defun chatgpt-send-message ()
;;   "Send a message to ChatGPT and display response choices in a new buffer."
;;   (interactive)
;;   (let ((prompt (read-string "Enter message: ")))
;;     (let ((chat-gpt-buffer (generate-new-buffer "*ChatGPT Response*")))
;;       (set-window-buffer (split-window-right) chat-gpt-buffer)
;;       (chatgpt-display-response prompt chat-gpt-buffer))))


;; (defun chatgpt-display-response (prompt buffer)
;;   "Send a prompt to ChatGPT and display response choices in BUFFER."
;;   (with-current-buffer buffer
;;     (markdown-mode)
;;     (erase-buffer)
;;     (let (
;;       (other-window 1)
;;       (insert "Prompt: \n=========\n")
;;       (insert (concat prompt " \n\n"))
;;       (insert "ChatGPT: \n=========\n")
;;       (insert (chatgpt-extract-content-text (chatgpt-get-response prompt)))
;;       (goto-char (point-max))
;;       (message "Response displayed in new buffer.")))))


;; (defun chatgpt-get-response (prompt)
;;   "Get a response from ChatGPT for the given PROMPT."
;;   (request-response-data
;;    (request "https://api.openai.com/v1/chat/completions"
;;      :type "POST"
;;      :sync t
;;      :headers `(("Content-Type" . "application/json")
;;                 ("Authorization" . ,(concat "Bearer " (chatgpt-get-api-key))))
;;      :data (json-encode `((messages . (((role . "user") (content . ,prompt))))
;;                           (model . "gpt-3.5-turbo")
;;                           (temperature . 0.6)))
;;      :parser 'json-read)))

(use-package gptel
              :config
              (setq gptel-api-key (chatgpt-get-api-key)))
(global-set-key (kbd "C-c C-g") 'gptel-menu)

;; Yes Or No y-or-p
 (defalias 'yes-or-no-p 'y-or-n-p)

 ;; Whitespace mode only for python-mode (add others if you need)
 (defun whitespace-mode-enable()
   (whitespace-mode t))

 (add-hook 'java-mode-hook 'whitespace-mode-enable)
 (add-hook 'python-mode-hook 'whitespace-mode-enable)
 (add-hook 'rust-mode-hook 'whitespace-mode-enable)
 (add-hook 'js-mode-hook 'whitespace-mode-enable)

 ;; Delete highlighted text on input
 (delete-selection-mode 1)

 ;; Electric pair mode (parenthesis)
 (electric-pair-mode 1)

 ;; So-long
 (if (version<= "27.1" emacs-version)
     (global-so-long-mode 1)
     (setq bidi-inhibit-bpa t))

;; dap an lsp additional
(dap-register-debug-template "Docker Debug"
                             (list :type "python"
                                   :request "attach"
                                   :name "Docker Debug"
                                   :host "localhost"
                                   :port 5678))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                  :major-modes '(python-mode)
                  :remote? t
                  :server-id 'pylsp-remote))
;; subprocess call
(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
 Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (let ((command (mapconcat 'identity args " ")))
    (funcall start-file-process-shell-command name buffer command)))

(advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)
(setq org-babel-python-command "python3")

(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))
(setq projectile-mode-line "Projectile")

  ;; remove beep
(setq ring-bell-function 'ignore)

;; Other window alternative
 (global-set-key (kbd "M-o") 'mode-line-other-buffer)
 ;; Duplicate row
 (defun my-duplicate-line ()
   (interactive)
   (move-beginning-of-line 1)
   (kill-line)
   (yank)
   (newline)
   (yank)
 )
 (global-set-key (kbd "C-c d") 'my-duplicate-line)
 (global-set-key (kbd "C-c r") 'kill-whole-line)

 (defun my-copy-row-path-number ()
   (interactive)
   (kill-new (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))

(defun my-delete-word (arg)
   "Delete characters forward until encountering the end of a word.
 With argument, do this that many times.
 This command does not push text to `kill-ring'."
   (interactive "p")
   (delete-region
    (point)
    (progn
      (forward-word arg)
      (point))))

 (defun my-backward-delete-word (arg)
   "Delete characters backward until encountering the beginning of a word.
 With argument, do this that many times.
 This command does not push text to `kill-ring'."
   (interactive "p")
   (my-delete-word (- arg)))

 ;; Bind them to emacs's default shortcut keys:
 (global-set-key (kbd "<C-delete>") 'my-delete-word)
 (global-set-key (kbd "<C-backspace>") 'my-backward-delete-word)
;; just one space to prevent global language change hotkey overrid
 (global-set-key (kbd "C-S-d") 'just-one-space)

;; ;; Clean up lsp blacklist folders
;; (setf (lsp-session-folders-blacklist (lsp-session)) nil)
;; (lsp--persist-session (lsp-session))
(setq counsel-tramp-custom-connections '(/ssh:trx|docker:crystal_trx:/))
