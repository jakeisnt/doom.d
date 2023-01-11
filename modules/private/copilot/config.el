;;; private/copilot/config.el -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook 'copilot-mode)
(customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))

;; complete by copilot first, then company-mode
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

;; modify company-mode behaviors
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends)
  ;; enable tab completion
  (define-key company-mode-map (kbd "<tab>") 'my-tab)
  (define-key company-mode-map (kbd "TAB") 'my-tab)
  (define-key company-active-map (kbd "<tab>") 'my-tab)
  (define-key company-active-map (kbd "TAB") 'my-tab))
