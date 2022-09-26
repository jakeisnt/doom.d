;; Pete Manolios 2020-01-30
;; https://gitlab.com/jakeisnt/acl2s-scripts/-/blob/master/.lisp.el
;; Here is emacs lisp mode that may be useful. In particular, the
;; send-acl2-form is what I've been asked about.

(defun send-area-to-lisp (x1 x2)
  "Send the region between ARG1 and ARG2 to the lisp process and evaluate.
Also see send-region-to-lisp, send-defun-to-lisp, and send-sexp-to-lisp"
  (interactive)
  (save-excursion
    (let ((beginning    (min x1 x2))
          (end          (max x1 x2))
          (current-buffer (current-buffer))
          (lisp-window    (get-buffer-window  "*inferior-lisp*"))
          (lisp-process   (get-buffer-process "*inferior-lisp*"))
          (tempend))
      (set-buffer "*inferior-lisp*")
      (goto-char (point-max))
      (insert "...\C-j")
      (set-marker (process-mark lisp-process) (point-max))
      (set-window-point lisp-window (point-max))
      (set-buffer current-buffer)
      ;; The following is the result of many hours work getting
      ;; the transfer to work properly.
      (while (< beginning end)
        (setq tempend (min end (+ beginning 63)))
        (process-send-region "*inferior-lisp*" beginning tempend)
        (accept-process-output)
        (setq beginning tempend))
      ;; Send the string <linefeed> = ^J to terminate the form.
      (process-send-string "*inferior-lisp*" "\C-j")
      (accept-process-output)
      (setq this-command 'nil))))

(defun send-region-to-lisp ()
  "Send the region between mark and point to the lisp process and evaluate"
  (interactive)
  (send-area-to-lisp (point) (mark)))

(global-set-key "\C-\M-r" 'send-region-to-lisp)

(defun send-sexp-to-lisp ()
  "Send the previous s-expression to the lisp process."
  (interactive)
  (save-excursion
    (backward-sexp 1)
    (backward-char 1)
    (mark-sexp 1)
    (send-region-to-lisp)))

(global-set-key "\C-\M-s" 'send-sexp-to-lisp)

(global-set-key "\M-s" 'isearch-forward-regexp)

;;;  Miscellaneous functions

(put 'eval-expression  'disabled nil)   ; EMACS evaluator OK.

;;;  Set correct editor indentation for the functions listed.
;;;  Current indentation is returned by (get '<func> 'lisp-indent-hook).

(mapcar #'(lambda (x)
            (put x 'lisp-indent-hook 'defun))
        '(all block case do do* do-filled-array
           dolist dolist-numbered domap domap-numbered dotimes
           ecase etypecase eval-when exists exporting-defstruct
           labels let let* multiple-value-bind mvbind mvsetq
           nd-rule prog1 return-from rule-bind term-bind typecase
           unless unwind-protect when with-open-file))

;;; Cause editor to indent first 3 forms following an 'if.

(put 'if 'lisp-indent-hook 3)

(defun center0 ()
  "Redraw screen with pointer at beginning of screen"
  (interactive)
  (recenter 0))

(global-set-key "\C-xl" 'center0)

(defvar acl2-active-shell-buffer "*shell*"
  "*Used by send-acl2-form to send the current acl2 form to the active shell buffer")

(defun set-acl2-active-shell-buffer ()
  "Make the current buffer the active acl2 shell buffer"
  (interactive)
  (setq acl2-active-shell-buffer
        (buffer-name)))

(defun skip-white-space ()
  (while (looking-at "[ \t\n]")
    (forward-char 1)))

(defun skip-nested-comments (depth)
  (cond
   ((< depth 1) t)
   (t (re-search-forward "|#\\|#|" (point-max) t 1)
      (forward-char -2)
      (cond ((looking-at "#|")
             (forward-char 2)
             (skip-nested-comments (1+ depth)))
            (t (forward-char 2)
               (skip-nested-comments (1- depth)))))))

(defun skip-comments ()
  (cond ((looking-at ";")
         (beginning-of-line 2)
         (skip-comments))
        ((looking-at "#|")
         (forward-char 2)
         (skip-nested-comments 1)
         (skip-comments))
        ((looking-at "[ \t\n]")
         (skip-white-space)
         (skip-comments))
        (t t)))

(defun send-acl2-form ()
  "Send the sexpt starting from the point to acl2-active-shell-buffer"
  (interactive)
  (skip-comments)
  (let ((begin (point))
        (end))
    (forward-sexp)
    (setq end (point))
    (process-send-region acl2-active-shell-buffer begin end)
    (process-send-string acl2-active-shell-buffer "\C-j")
    (accept-process-output))
  (skip-comments))

(defun indent-untabify-sexp ()
  "Indent an s-expression and untabify it."
  (interactive)
  (save-excursion
    (indent-sexp)
    (let ((begin (point)))
      (forward-sexp 1)
      (untabify begin (point)))))

(defun indent-untabify-sexps-in-area (x1 x2)
  "Indent and untabify all s-expressions in the region bounded by x1, x2."
  (interactive)
  (save-excursion
    (let ((beginning (min x1 x2))
          (end       (max x1 x2)))
      (goto-char beginning)
      (skip-comments)
      (while (< beginning end)
        (indent-untabify-sexp)
        (forward-sexp)
        (skip-comments)
        (setq beginning (point))))))

(defun indent-untabify-sexps-in-region ()
  "Indent and untabify all s-expressions in a region."
  (interactive)
  (save-excursion
    (indent-untabify-sexps-in-area (point) (mark))))

(defun indent-untabify-sexps-in-buffer ()
  "Indent and untabify all s-expressions in a region."
  (interactive)
  (save-excursion
    (indent-untabify-sexps-in-area (point-min) (point-max))))

(defun find-unbalanced-pars ()
  "Finds parenthesis mismatch error in buffer. Reads through all of the
current buffer and tries to find places in which the parentheses do not
balance. Positions point to possible trouble-spots, printing out a message
that says what the trouble appears to be.  This command only finds
one such error; if you suspect more errors, run it again."
  (interactive)
  (let ((saved-point (point))
        (old-point))
    (goto-char (point-min));; Go to start of buffer.
    (setq old-point (point))
    (skip-comments)
    (forward-sexp)
    (while (not (equal (point) old-point))
      (setq old-point (point))
      (skip-comments)
      (forward-sexp))
    (goto-char saved-point)
    (message "All parentheses appear balanced.")))

(global-set-key "\M-n" 'send-acl2-form)
(global-set-key "\C-xas" 'set-acl2-active-shell-buffer)
(local-unset-key "\C-\M-q")
(global-set-key "\C-\M-q" 'indent-untabify-sexp)

;; Some code for syntax highlighting.

(defun acl2s-add-keywords (face-name keyword-rules)
  (let* ((keyword-list (mapcar #'(lambda (x)
                                   (symbol-name (cdr x)))
                               keyword-rules))
         (keyword-regexp (concat "(\\("
                                 (regexp-opt keyword-list)
                                 "\\)[ \n]")))
    (font-lock-add-keywords 'lisp-mode
                            `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
            (put (cdr x)
                 'lisp-indent-function
                 (if (equal (car x) 0) 'defun (car x))))
        keyword-rules))

                                        ; Should improve to highlight anything starting with define or defun
(acl2s-add-keywords
 'font-lock-keyword-face
 '((0 . match)
   (0 . defmacro)
   (0 . defconst)
   (0 . def-const)
   (0 . defundc)
   (0 . defuncd)
   (0 . definecd)
   (0 . definedc)
   (0 . definec-no-test)
   (0 . definecd-no-test)
   (0 . definec)
   (0 . defunc)
   (0 . thm)
   (0 . sig)
   (0 . defthm)
   (0 . register-type)
   (0 . defdata)
   (0 . defdata-alias)
   (0 . include-book)
   (0 . in-theory)
   (0 . property)
   (0 . property-fail)
   (0 . progn!)
   (0 . defdata-disjoint)
   (0 . defdata-disjoint-strict)
   (0 . defdatas-disjoint)
   (0 . defdatas-disjoint-strict)
   (0 . must-fail)
   (0 . must-succeed)
   (0 . modeling-start)
   (1 . let+)
   (1 . b*)
   (1 . test?)))
