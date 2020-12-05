;;; YY --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) YY YY
;;
;; Author: YY <http://github/YY>
;; Maintainer: YY <YY>
;; Created: YY
;; Modified: YY
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/YY/YY
;; Package-Requires: ((emacs YY) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(package! org-projectile :pin "96a57a43555e24e5e0d81e79f0fbb47001c41bac")
(package! org-roam-server :pin "fde2636d794f020ed5810fa38fe57a264932f661")
(package! org-roam :pin "ac2044b84bb99e8a77e834043b4aa16b4b89d8fc")
(package! org-ref :pin "2a91b6f67dc3116b0b688c1242ce67d90d833326")
(package! org-roam-bibtex :pin "e82b63b9b8c6772274f7cc2e9496559dd2219b29")
(package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d")
(package! company-org-roam :pin "1132663bd68022aa7ea005ff53c7c7571890769d")
(package! org-super-agenda :pin "3264255989021b8563ea42b5d26acbc2a024f14d")


;; ~/.doom.d/packages.el
(package! org-mode
  :recipe (:host github
           :repo "yantar92/org"
           :branch "feature/org-fold"
           :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")
           :build (with-temp-file (expand-file-name "org-version.el" (straight--repos-dir "org"))
                    (insert "(fset 'org-release (lambda () \"9.5\"))\n"
                            "(fset 'org-git-version #'ignore)\n"
                            "(provide 'org-version)\n")))
  :pin "308c4f57d26307c0f57ad387a4638b051f541a9c"
  :shadow 'org)

(provide 'packages)
;;; packages.el ends here
