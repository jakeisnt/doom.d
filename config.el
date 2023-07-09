;;; config.el --- Custom functions for Doom -*- lexical-binding: t; -*-
;;; Commentary: hey
;;;
;;
;; Copyright (C) 2019-2021 Jacob Chvatal
;;
;; Author: YY <http://github/YY> ;; Maintainer: YY <YY> ;; Created: YY
;; Modified: YY
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/YY/YY
;; Package-Requires: ((emacs "24.1"))
;;
;; This file is not part of GNU Emacs.
;;; Commentary: hey
;;; Code:

(require 'evil)

;; make sure that emacs receives this
;; there must be a better way to do this?
(setenv "GNUPGHOME" "~/.config/gnupg")

;; run commands from emacs in a bash shell
;; TODO: What path gives us bash with the environment variables?


(if (eq system-type 'darwin)
  nil
  (progn
   (setq
    shell-file-name "/run/current-system/sw/bin/bash"
    explicit-shell-file-name  "/run/current-system/sw/bin/bash"))
  (setq-default
   shell-file-name "/run/current-system/sw/bin/bash"
   explicit-shell-file-name  "/run/current-system/sw/bin/bash"))


;;; Code:
(setq user-full-name "Jacob Chvatal"
      user-mail-address "jakechvatal@gmail.com"
      doom-theme 'doom-city-lights
      doom-font
      (if (eq system-type 'darwin)
          (font-spec :family "Berkeley Mono" :size 12 :weight 'semi-light)
        (font-spec :family "monospace" :size 12 :weight 'semi-light))
      doom-variable-pitch-font
      (if (eq system-type 'darwin)
          (font-spec :family "Gill Sans" :size 12)
          (font-spec :family "sans" :size 12)))

(setq-default delete-by-moving-to-trash t
              tab-width 4
              uniquify-buffer-name-style 'forward
              window-combination-resize t
              x-stretch-cursor t
              history-length 1000
              prescient-history-length 100)

(setq undo-limit 80000000
      gc-cons-threshold most-positive-fixnum
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

;; garbage collect when idling, but allow as many conses as we need. no freezing!
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

(setq projectile-globally-ignored-directories
      '("node_modules" ".happypack" "flow-typed" "build" "lib")
      grep-find-ignored-directories
      '("node_modules" ".happypack"))

(delete-selection-mode 1)


;; --- Window management
;; always split window to bottom right
(setq evil-vsplit-window-right t
      evil-split-window-below t)

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
        company-show-quick-access t)
  ;; (add-hook 'evil-normal-state-entry-hook #'company-abort)
  )

(map! :leader
      "h r n" (lambda () (interactive)
                (async-shell-command "hey rebuild")))

;;; automatically chmod shell scripts
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun doom-dashboard-widget-banner ()
  "Render a NixOS desktop widget banner."
  (mapc (lambda (line)
          (insert (propertize
                   (+doom-dashboard--center +doom-dashboard--width line)
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

;; Find a URL
;; TODO: Visit the localhost port shared by the current project or the page that has it open.
(map! :leader
      "\"" (lambda ()
             (interactive)
             (browse-url (read-string "URL:")))
      "'" (lambda () (interactive) (counsel-search)))

(use-package! symex
  :config
  (symex-initialize)
  (global-set-key (kbd "s-;") 'symex-mode-interface)
  :custom
  (symex-modal-backend 'evil)
  (global-set-key (kbd "C-(") 'sp-backward-barf-sexp)
  (global-set-key (kbd "C-)") 'sp-forward-barf-sexp)
  (global-set-key (kbd "C-9") 'sp-backward-slurp-sexp)
  (global-set-key (kbd "C-0") 'sp-forward-slurp-sexp))

;; md-roam

;; (use-package! md-roam
;; :after org-roam
;; :config
;; (setq org-roam-file-extensions '("org" "md"))
;; (md-roam-mode 1)
;; (setq md-roam-file-extension "md")
;; (org-roam-db-autosync-mode 1))

(use-package! epg
  :init (defvar epg-pinentry-mode 'loopback))


(use-package! vlf
  :config
  (require 'vlf-setup))

;; (advice-add #'evil-motion-range :around #'~/evil-motion-range--wrapper)


(use-package! wakatime-mode
  :functions global-wakatime-mode
  :init (global-wakatime-mode))

(use-package! activity-watch-mode
  :functions global-activity-watch-mode
  :config (global-activity-watch-mode))



;; don't show the ignored git files in projectile
(setq projectile-git-command "git ls-files -zco --exclude-standard")

(provide 'config)

;;; config.el ends here
