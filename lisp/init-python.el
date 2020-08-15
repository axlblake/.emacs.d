;;; init-python.el --- Python editing -*- lexical-binding: t -*-

;; See the following note about how I set up python + virtualenv to
;; work seamlessly with Emacs:
;; https://gist.github.com/purcell/81f76c50a42eee710dcfc9a14bfc7240


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

;; (setq python-shell-interpreter "python3")

(setq flycheck-python-flake8-executable "flake8")
;; (flycheck-select-checker 'python-flake8)
(flycheck-mode t)

;; elpy
(elpy-enable)
(setq elpy-modules '(eldoc-mode
                     highlight-indentation-mode
                     yas-minor-mode
                     auto-complete-mode))


(provide 'init-python)
;;; init-python.el ends here
