(setq debug-on-error t)
(setq frame-resize-pixelwise t)
(toggle-frame-maximized)

;; Install MELPA package repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

  ;; Spacemacs theme
;; this didn't seem to work without first doing a manual package-install of spacemacs-theme
  (use-package spacemacs-theme
    :defer t
    :init(load-theme 'spacemacs-dark t))
