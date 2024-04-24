(defmacro // (&rest sexp) nil)

(// when (string= system-type "gnu/linux")
  (org-babel-load-file (concat user-emacs-directory "linux.org")))
(org-babel-load-file (concat user-emacs-directory "emacs.org"))
(org-babel-load-file (concat user-emacs-directory "languages.org"))
(// add-to-list 'load-path "~/.emacs.d/lisp")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" default))
 '(package-selected-packages
   '(fancy-battery theme-changer rainbow-delimiters rainbox-delimiters highlight-parentheses highlight-parentheses-mode flycheck-clj-kondo org-roam pdf-tools winum company magit projectile perspective spacemacs-theme spaceline avy orderless consult marginalia vertico switch-window counsel exec-path-from-shell))
 '(safe-local-variable-values
   '((cider-repl-display-help-banner)
     (eval add-to-list 'cider-test-defining-forms "def-slt-test")
     (eval define-clojure-indent
	   (match 1)
	   (for-all 1))
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
