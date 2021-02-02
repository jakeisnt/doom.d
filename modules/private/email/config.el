;;; private/email/config.el -*- lexical-binding: t; -*-
(setq mu4e-contexts
 `( ,(make-mu4e-context
     :name "isnt"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/isnt" (mu4e-message-field msg :maildir))))
     :vars '(
       (mu4e-trash-folder . "/isnt/[isnt].Trash")
       (mu4e-refile-folder . "/isnt/[isnt].Archive")
       ))
   ,(make-mu4e-context
     :name "Exchange"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/Exchange" (mu4e-message-field msg :maildir))))
     :vars '(
       (mu4e-trash-folder . "/Exchange/Deleted Items")
       (mu4e-refile-folder . exchange-mu4e-refile-folder)
       ))
   )
 )
