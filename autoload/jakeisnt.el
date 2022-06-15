;;; YY --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) YY YY
;;
;; Author: YY <http://github/YY>
;; Maintainer: YY <YY>
;; Created: YY
;; Modified: YY
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://github.com/YY/YY
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; nothing lol
;;
;;
;;; Code:

(defun gotop ()
  "Run the GOTOP process monitor."
  (interactive)
  (if (get-buffer "*gotop*")
      (switch-to-buffer "*gotop*")
    (ansi-term "/usr/bin/env bash" "gotop")
    (comint-send-string "*gotop*" "gotop\n")))

(defun htop ()
  "Run the HTOP process monitor."
  (interactive)
  (if (get-buffer "*htop*")
      (switch-to-buffer "*htop*")
    (ansi-term "/usr/bin/env bash" "htop")
    (comint-send-string "*htop*" "htop\n")))

(defun connect-vultr ()
  "Connect to my Vultr VPS."
  (interactive)
  (dired "/ssh:jake@107.191.42.68:"))

;; https://stackoverflow.com/questions/32977707/sending-bash-commands-to-an-open-terminal-buffer-in-emacs
(defun visit-project-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer (persp-ansi-buffer-name)))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL"))
        (rename-buffer (persp-ansi-buffer-name))
        (end-of-buffer)
        (insert (format "cd %s" (projectile-project-root)))
        (term-send-input))
    (switch-to-buffer-other-window (persp-ansi-buffer-name))))

;;; ~/.config/doom/autoload/hlissner.el -*- lexical-binding: t; -*-

(defconst dotfiles "~/.config")

;;;###autoload
(defun find-in-dotfiles ()
  "Open a file somewhere in ~/.dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name dotfiles)))

;;;###autoload
(defun browse-dotfiles ()
  "Browse the files in ~/.dotfiles."
  (interactive)
  (doom-project-browse (expand-file-name dotfiles)))


;;
;;; Recording frame

;;; Scratch frame
(defvar +hlissner--scratch-frame nil)

(defun cleanup-scratch-frame (frame)
  (when (eq frame +hlissner--scratch-frame)
    (with-selected-frame frame
      (setq doom-fallback-buffer-name (frame-parameter frame 'old-fallback-buffer))
      (remove-hook 'delete-frame-functions #'cleanup-scratch-frame))))

;;;###autoload
(defun open-scratch-frame (&optional fn)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (let* ((frame-title-format "")
         (preframe (cl-loop for frame in (frame-list)
                            if (equal (frame-parameter frame 'name) "scratch")
                            return frame))
         (frame (unless preframe
                  (make-frame `((name . "scratch")
                                (width . 120)
                                (height . 24)
                                (transient . t)
                                (internal-border-width . 10)
                                (left-fringe . 0)
                                (right-fringe . 0)
                                (undecorated . t)
                                ,(if IS-LINUX '(display . ":0")))))))
    (setq +hlissner--scratch-frame (or frame posframe))
    (select-frame-set-input-focus +hlissner--scratch-frame)
    (when frame
      (with-selected-frame frame
        (if fn
            (call-interactively fn)
          (with-current-buffer (switch-to-buffer "*scratch*")
            ;; (text-scale-set 2)
            (when (eq major-mode 'fundamental-mode)
              (emacs-lisp-mode)))
          (redisplay)
          (set-frame-parameter frame 'old-fallback-buffer doom-fallback-buffer-name)
          (setq doom-fallback-buffer-name "*scratch*")
          (add-hook 'delete-frame-functions #'cleanup-scratch-frame))))))

(provide 'jakeisnt)
;;; jakeisnt.el ends here
