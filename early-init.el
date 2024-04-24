(defmacro // (&rest sexp) nil)
(// setq debug-on-error t)

;; open ~maximised in KDE/Plasma https://emacs.stackexchange.com/questions/77379/emacs-window-does-not-fully-maximize-on-kde-desktop
(setq frame-resize-pixelwise t)
(toggle-frame-maximized)

;; Install MELPA package repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(eval-when-compile
  (require 'use-package))

  ;; Spacemacs theme
;; this didn't seem to work without first doing a manual package-install of spacemacs-theme
  (use-package spacemacs-theme
    :defer t
    :init(load-theme 'spacemacs-dark t))
