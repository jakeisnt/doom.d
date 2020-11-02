;;; private/ranger/config.el -*- lexical-binding: t; -*-

(use-package! ranger
  :init
  (ranger-override-dired-mode t)
  :config
  (setq
   ranger-cleanup-on-disable t
   ranger-modify-header t
   ranger-hide-cursor t
   ranger-preview-file t
   ranger-max-preview-size 20
   ranger-dont-show-binary t))
