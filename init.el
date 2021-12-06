;;; init.el -*- lexical-binding: t; -*-

(doom!
 :desktop
 ;; exwm

 :completion
 (company
  ;; +childframe
  ;; +tng ; when i want it to get outof the way...
  )
 ivy
 ;;helm
 ;;ido

 :ui
 deft
 doom
 doom-dashboard
 hl-todo


 ;;indent-guides
 (modeline +light)

 ;;neotree
 ;;ophints
 (popup +defaults)
 ;;pretty-code
 ;;tabs
 ;;treemacs
 ;;unicode
 vc-gutter
 ;;window-select
 ;;workspaces
 zen

 :editor
 (evil +everywhere)
 file-templates
 fold
 (format +onsave)
 ;;god
 lispy
 multiple-cursors
 ;;objed
 ;;parinfer
 rotate-text
 snippets
 word-wrap

 :emacs
 dired
 electric
 ;;ibuffer
 (undo +tree)
 vc

 :term
 eshell                 ; the elisp shell that works everywhere
 ;;shell           ; simple shell REPL for Emacs
 term                   ; basic terminal emulator for Emacs
 vterm          ; the best terminal emulation in Emacs

 :checkers
 syntax
 ;;spell
 ;;grammar

 :tools
 editorconfig
 ;;ansible
 ;;debugger
 direnv
 ;;docker
 ;;ein
 (eval +overlay)
 ;;gist              ; interacting with github gists
 lookup                               ; navigate your code and its documentation
 lsp
 ;;macos
 magit
 ;;make              ; run make tasks from Emacs
 pass           ; password manager for nerds
 pdf            ; pdf enhancements
 ;;prodigy           ; managing external services & code builders
 ;;rgb               ; creating color strings
 taskrunner        ; taskrunner for all your projects
 ;;terraform         ; infrastructure as code
 ;;tmux              ; an API for interacting with tmux
 ;;upload            ; map local to remote projects via ssh/ftp

 :os
 (:if IS-MAC macos)
 ;;tty

 :lang
 agda                    ; types of types of types of types...
 cc                      ; C/C++/Obj-C madness
 (clojure +lsp)          ; java with a lisp
 common-lisp             ; if you've seen one lisp, you've seen them all
 ;; coq               ; proofs-as-programs
 ;;crystal           ; ruby at the speed of c
 ;;csharp            ; unity, .NET, and mono shenanigans
 data       ; config/data formats
 ;;(dart +flutter)   ; paint ui and not much else
 ;; elixir              ; erlang done right
 elm        ; care for a cup of TEA?
 emacs-lisp ; drown in parentheses
 ;;erlang            ; an elegant language for a more civilized age
 ;;ess               ; emacs speaks statistics
 ;;faust             ; dsp, but you get to keep your soul
 ;;fsharp            ; ML stands for Microsoft's Language
 ;;fstar             ; (dependent) types and (monadic) effects and Z3
 ;;gdscript          ; the language you waited for
 ;;(go +lsp)         ; the hipster dialect
 (haskell +nix +lsp)                    ; a language that's lazier than I am
 ;;hy                ; readability of scheme w/ speed of python
 ;; idris        ; idris2 is better!!!!!!!!!!!!!!
 json         ; At least it ain't XML
 ;; (java +meghanada) ; the poster child for carpal tunnel syndrome
 (javascript +lsp)
 ;;julia             ; a better, faster MATLAB
 ;; kotlin            ; a better, slicker Java(Script)
 latex                ; writing papers in Emacs has never been so fun
 ;;lean
 ;;factor
 ;;ledger            ; an accounting system in Emacs
 ;;lua               ; one-based indices? one-based indices
 markdown                  ; writing docs for people to ignore
 ;;nim               ; python + lisp at the speed of c
 nix                                ; I hereby declare "nix geht mehr!"
 ocaml ;; - ocamlformat broken for some reason

 (org
  +dragndrop
  +gnuplot
  +pandoc
  +pomodoro
  +present
  ;; +roam
  +roam2
  +noter
  ;; +present
  ;; +tanglesync
  ;; +chef
  ;; +ref
  )

 ;;php               ; perl's insecure younger brother
 ;;plantuml          ; diagrams for confusing people more
 ;; purescript        ; javascript, but functional
 (python
  +lsp
  +pyright)
                                        ; beautiful is better than ugly
 ;;qt                ; the 'cutest' gui framework ever
 (racket
  ;; +lsp
  +xp)
                                        ; a DSL for DSLs
 ;;raku              ; the artist formerly known as perl6
 ;;rest              ; Emacs as a REST client
 ;;rst               ; ReST in peace
 ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
 (rust +lsp)  ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
 ;;scala             ; java, but good
 ;;scheme            ; a fully conniving family of lisps
 sh           ; she sells {ba,z,fi}sh shells on the C xor
 ;;sml
 ;;solidity          ; do you need a blockchain? No.
 ;;swift             ; who asked for emoji variables?
 ;;terra             ; Earth and Moon in alignment for performance.
 web                                    ; the tubes
 yaml                                   ; JSON, but reada
 zig                                    ;; C but simpler!
 acl2s

 :email
 ;;(mu4e +gmail)
 ;;notmuch
 ;;(wanderlust +gmail)

 :app
 ;;calendar
 ;;irc               ; how neckbeards socialize
 rss            ; emacs as an RSS reader
 ;;twitter           ; twitter client https://twitter.com/vnought

 :private
 editor
 internet
 modeline
 org
 tidal
 ;; urweb

 :config
 ;;literate
 (default +bindings +smartparens))
