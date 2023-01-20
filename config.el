;;; config.el --- Custom functions for Doom -*- lexical-binding: t; -*-
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
;;; Commentary:
;; This is commentary suggested by error messages.
;;; Code:

(require 'evil)

;; make sure that emacs receives this
;; there must be a better way to do this?
(setenv "GNUPGHOME" "~/.config/gnupg")

;; run commands from emacs in a bash shell
;; TODO: What path gives us bash with the environment variables?
(setq
 shell-file-name "/run/current-system/sw/bin/bash"
 explicit-shell-file-name  "/run/current-system/sw/bin/bash")

(setq-default
 shell-file-name "/run/current-system/sw/bin/bash"
 explicit-shell-file-name  "/run/current-system/sw/bin/bash")

;;; Code:
(setq user-full-name "Jacob Chvatal"
      user-mail-address "jakechvatal@gmail.com"
      doom-theme 'doom-city-lights)

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
        company-show-quick-access t)
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

;; TODO: Visit the localhost port shared by the current project or the page that has it open.

;; Find a URL
(map! :leader
      "\"" (lambda ()
             (interactive)
             (browse-url (read-string "URL:"))))
(map! :leader
      "'" (lambda () (interactive) (counsel-search)))


;; Language-sensitive manipulation tools!
(use-package! symex
  :config
  (symex-initialize)
  (global-set-key (kbd "s-;") 'symex-mode-interface)
  :custom
  (symex-modal-backend 'evil))


(global-set-key (kbd "C-(") 'sp-backward-barf-sexp)
(global-set-key (kbd "C-)") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-9") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-0") 'sp-forward-slurp-sexp)

(use-package! epg
  :init (setq epg-pinentry-mode 'loopback))

;; difftastic
;; https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(transient-define-prefix th/magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
   ("s" "Difftastic Show" th/magit-show-with-difftastic)])

(transient-append-suffix 'magit-dispatch "!"
  '("#" "My Magit Cmds" th/magit-aux-commands))

(define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)

;; (advice-add #'evil-motion-range :around #'~/evil-motion-range--wrapper)
(global-wakatime-mode)

(provide 'config)
;;; config.el ends here
