;; -*- no-byte-compile: t; -*-
;;; private/copilot/packages.el

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el")))

(provide 'packages)
