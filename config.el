;;; YY --- description -*- lexical-binding: t; -*-
;;;
;;
;; Copyright (C) YY YY
;;
;; Author: YY <http://github/YY> ;; Maintainer: YY <YY> ;; Created: YY
;; Modified: YY
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/YY/YY
;; Package-Requires: ((emacs YY) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;; Code:

(require 'evil)

;;; Code:
(setq user-full-name "Jacob Chvatal"
      user-mail-address "jakechvatal@gmail.com"
      doom-theme 'doom-nord)

(setq-default delete-by-moving-to-trash t
              tab-width 4
              uniquify-buffer-name-style 'forward
              window-combination-resize t
              x-stretch-cursor t
              history-length 1000
              prescient-history-length 100)

(setq undo-limit 80000000
      evil-want-fine-undo t
      auth-source-cache-expiry nil
      inhibit-compacting-font-caches t
      display-line-numbers-type 'relative
      select-enable-clipboard t
      read-process-output-max (* 1024 1024)
      ;; lsp-completion-provider :capf
      lsp-idle-delay 0.500

      ;; lsp-ui conflicts with eldoc; disable it
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil

      evil-ex-substitute-global t)

(setq doom-font (font-spec :family "monospace" :size 14 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 14))

(setq projectile-globally-ignored-directories '("node_modules" ".happypack" "flow-typed" "build" "lib")
      grep-find-ignored-directories '("node_modules" ".happypack"))

;; always split window to bottom right
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(delete-selection-mode 1)


(map!
 :leader
 :prefix "w"
 "v" (lambda () (interactive) (evil-window-vsplit) (+ivy/switch-buffer))
 "s" (lambda () (interactive) (evil-window-split) (+ivy/switch-buffer))
 "x" (lambda () (interactive) (evil-window-vsplit) (+ivy/projectile-find-file))
 "z" (lambda () (interactive) (evil-window-vsplit) (+ivy/project-search)))

(setq +ivy-buffer-preview t) ;; buffer previews

;; TODO: figure out the buffer names I want!
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string ".*/[0-9]*-?" "🢔 " buffer-file-name)
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(use-package! abbrev
  :init
  (setq-default abbrev-mode t)
  ;; a hook function that sets the abbrev-table to org-mode-abbrev-table
  ;; whenever the major mode is a text mode
  (defun tec/set-text-mode-abbrev-table ()
    (if (derived-mode-p 'text-mode)
        (setq local-abbrev-table org-mode-abbrev-table)))
  :commands abbrev-mode
  :hook
  (abbrev-mode tec/set-text-mode-abbrev-table)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
  (setq save-abbrevs 'silently))

(use-package! company
  :config
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-show-numbers t)
  ;; (add-hook 'evil-normal-state-entry-hook #'company-abort)
  )

(map! :leader
      "h r n" (lambda () (interactive)
                (async-shell-command "hey rebuild")))

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; to use pdfview with auctex
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;    TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
;; to have the buffer refresh after compilation

;;; automatic #bang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable

       Since it doesn't rely on ##chmod##, it also works for remote
       files, i.e. those accessed by TrampMode.

       taken from:
       http://www.emacswiki.org/emacs-en/MakingScriptsExecutableOnSave"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
                 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o100))
        (message (concat "Made " buffer-file-name " executable"))))))


(defun doom-dashboard-widget-banner ()
  (mapc (lambda (line)
          (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                              'face 'doom-dashboard-banner) " ")
          (insert "\n"))
        '("
                              ::::.    ':::::     ::::'
                              ':::::    ':::::.  ::::'
                                :::::     '::::.:::::
                          .......:::::..... ::::::::
                        ::::::::::::::::::. ::::::    ::::.
                        ::::::::::::::::::::: :::::.  .::::'
                              .....           ::::' :::::'
                              :::::            '::' :::::'
                    ........:::::               ' :::::::::::.
                    :::::::::::::                 :::::::::::::
                    ::::::::::: ..              :::::
                        .::::: .:::            :::::
                        .:::::  :::::          '''''    .....
                        :::::   ':::::.  ......:::::::::::::'
                        :::     ::::::. ':::::::::::::::::'
                                .:::::::: '::::::::::
                              .::::''::::.     '::::.
                              .::::'   ::::.     '::::.
                            .::::      ::::      '::::.

                                    E M A C S

")))

;; (use-package! ranger
;;   :init
;;   (ranger-override-dired-mode t)
;;   :config
;;   (setq
;;    ranger-cleanup-on-disable t
;;    ranger-modify-header t
;;    ranger-hide-cursor t
;;    ranger-preview-file t
;;    ranger-max-preview-size 20
;;    ranger-dont-show-binary t))

(defun ido-kill-emacs-hook ()
  ;; Quit emacs despite the ido.last buffer being written to.
  (ignore-errors (ido-save-history)))

(use-package! atomic-chrome
  :after-call after-focus-change-function
  :config
  (setq atomic-chrome-default-major-mode 'org-mode
        atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-start-server))

(map! :n [tab] (cmds! (and (featurep! :editor fold)
                           (save-excursion (end-of-line) (invisible-p (point))))
                      #'+fold/toggle
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)
      :v [tab] (cmds! (and (bound-and-true-p yas-minor-mode)
                           (or (eq evil-visual-selection 'line)
                               (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                      #'yas-insert-snippet
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)

      :leader
      "h L" #'global-keycast-mode
      "f t" #'find-in-dotfiles
      "f T" #'browse-dotfiles)

(use-package! browse-url
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"))

(define-key evil-normal-state-map (kbd "SPC a") 'link-hint-open-link)

(defun visit-localhost ()
  "Visit a specified localhost port."
  (interactive)
  (browse-url
   (concat "http://localhost:" (read-string "Visit port:"))))

;; Find a URL
(map! :leader
      "\"" (lambda ()
             (interactive)
             (browse-url (read-string "URL:"))))
(map! :leader
      "'" (lambda () (interactive) (counsel-search)))
(map!
 :leader
 :prefix "v"
 :desc "Visit Calendar" "c" (lambda () (interactive) (browse-url "https://calendar.google.com"))
 :desc "Visit Discord" "d" (lambda () (interactive) (browse-url "https://discord.gg"))
 :desc "Visit Spotify" "s" (lambda () (interactive) (browse-url "https://open.spotify.com"))
 :desc "Visit Gmail" "m" (lambda () (interactive) (browse-url "https://gmail.com"))
 :desc "Visit GitHub" "g" (lambda () (interactive) (browse-url "https://github.com/jakeisnt"))
 :desc "Visit local port" "l" 'visit-localhost)

;; Insert the contents of all of the files in a directory into a single file
(defun insert-directory ()
  (interactive)
  (let ((dir (read-directory-name "Directory to insert: ")))
    (mapc #'(lambda (file)
              (let ((file-full (concat dir file)))
                (insert-file-contents file-full)))
          (cddr (directory-files dir)))))

;; Use pinentry inside emacs?
(defun pinentry-emacs (desc prompt)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": ")))) str))

(require 'haskell)

;; (defun haskell-auto-insert-module-template-universum ()
;;   "Insert a module template for the newly created buffer."
;;   (interactive)
;;   (when (and (= (point-min)
;;                 (point-max))
;;              (buffer-file-name))
;;     (insert
;;      "-- | "
;;      "\n"
;;      "\n"
;;      "module "
;;      )
;;     (let ((name (haskell-guess-module-name)))
;;       (if (string= name "")
;;           (progn (insert "Main")
;;                  (shm-evaporate (- (point) 5)
;;                                 (point)))
;;         (insert name)))
;;     (insert "\n"
;;             "       (\n"
;;             "       ) where\n"
;;             "\n"
;;             "import Universum\n"
;;             "\n"
;;             )
;;     (goto-char (point-min))
;;     (forward-char 4)
;;     (god-mode)))

;; (remove-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template-universum)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(global-wakatime-mode)
(pinentry-start)

(provide 'config)
;;; config.el ends here
