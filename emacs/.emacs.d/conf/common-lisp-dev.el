;;;; Common Lisp related Configuration
;;;; By BreakDS
(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "~/.emacs.d/lisp/slime/")
(require 'slime)
(slime-setup '(slime-fancy))

;; setup load-path and autoloads
(require 'slime-autoloads)

;; Set your lisp system and, optionally, some contribs
;; (setq slime-contribs '(slime-fancy))
