;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Fullscreen and UI options
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Mac preffered modifiers
(setq mac-command-modifier      'meta
      mac-option-modifier       'super)

;; Dap Mode settings
(after! dap-mode
  (setq dap-python-debugger 'debugpy))


;; Whitespace
(global-whitespace-mode t)
(use-package! whitespace
  :config
                                        ; (progn
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  (setq
   whitespace-style '(face tabs tab-mark spaces space-mark trailing))
  )


;; Global keys for personal use
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-t") 'counsel-tramp)

;; Dired mode turn off omit mode by default
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))


;; Custom tramp connections
(setq counsel-tramp-custom-connections
      '(/ssh:trx|docker:crystal_trx:/ /ssh:matic|docker:crystal_matic:/
        /ssh:arb|docker:crystal_arb:/ /ssh:avax|docker:crystal_avax:/))

;; toggle shell

(defun shell-toggle ()
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
(global-set-key (kbd "C-c t t") 'shell-toggle)


(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package! multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(defun my-copy-row-path-number ()
  (interactive)
  (kill-new (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))

(global-set-key (kbd "C-S-d") 'just-one-space)

;; Use clangd as the language server for C++
(after! lsp-clangd
  (setq lsp-clients-clangd-executable "/usr/bin/clangd"))

;; Enable LSP in C++ mode
(add-hook 'c++-mode-hook #'lsp)
(setq compile-command "cmake --build .")

(use-package! cmake-mode
  :config
  (add-hook 'cmake-mode-hook #'lsp))

(after! gptel
  (defun chatgpt-get-api-key ()
    (let* ((file-contents (with-temp-buffer
                            (insert-file-contents "~/.emacs.d_bk/secrets.txt.gpg")
                            (buffer-substring-no-properties (point-min) (point-max))))
           (lines (split-string file-contents "\n" t))
           (my-api-key-line (cl-find-if (lambda (line) (string-match-p "^emacs-chatgpt-api-key=" line)) lines))
           (my-api-key-value (when my-api-key-line
                               (substring my-api-key-line (1+ (string-match "=" my-api-key-line))))))

      (if my-api-key-value
          my-api-key-value
        (error "my-api-key not found in file"))))
  (setq gptel-api-key (chatgpt-get-api-key))
  (global-set-key (kbd "C-c C-g") 'gptel-menu))
