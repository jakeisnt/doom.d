;; -*- no-byte-compile: t; -*-
;;; private/copilot/packages.el

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el")))

;; The `tab` keybinding conflicts here, so disable it.
;; If we want `emmet-mode` in the future, use the following line instead:
;; (map! :after emmet-mode [tab] nil)
(package! emmet-mode :disable t)

(provide 'packages)
