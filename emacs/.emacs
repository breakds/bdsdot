;;;; ------------------------------------------------------------+
;;;; Emacs Configuration                                         |
;;;; ------------------------------------------------------------+
;;;; Author: Break Yang <breakds@gmail.com>
;;;;
;;;; References:
;;;; 1. Sacha Chua's emacs configuration (http://sach.ac/dotemacs)

;; Enable Common Lisp
(require 'cl)

;; Set up the load path for ELPA/MELPA packages.
(require 'package)
(package-initialize nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Set up use-package.
(require 'use-package)

;; Use load-dir to load the configuration directory. The function call
;; (load-dirs) load the elisp files from the paths specified in the
;; variable LOAD-DIRS.
(use-package load-dir)
(add-to-list 'load-dirs "~/.emacs.d/conf")
(load-dirs)

;;;; ============================================================+
;;;; Key Bindings                                                |
;;;; ------------------------------------------------------------+
;;;;
;;;; * C-x <Right> and C-x <Left>
;;;;   winner-mode cycle windows.
;;;;
;;;; * C-x p
;;;;   Go back to the mark (set by C-<space>).
;;;;
;;;; * C-x r
;;;;   Open recent files.
;;;;
;;;; * Org Mode Bidings
;;;;   * C-c C-y
;;;;     Yank the link
;;;;   * C-x n s
;;;;     Focus on a headline
;;;;   * C-x n w
;;;;     Unfocus
;;;;   * C-c C-w
;;;;     refile
;;;;   * C-c C-x C-j
;;;;     go to running clock

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-list (quote (("Atril" "atril %o"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Atril")
     (output-html "xdg-open"))))
 '(markdown-command "grip --export -")
 '(org-agenda-files
   (quote
    ("~/org/cron.org" "~/org/reading.org" "~/org/unsorted.org" "~/org/projects.org")))
 '(package-selected-packages
   (quote
    (cmake-ide rtags flycheck company protobuf-mode yaml-mode web-mode use-package toc-org solarized-theme smart-mode-line org-doing org-dashboard markdown-mode+ load-dir forecast fcitx exec-path-from-shell elm-mode ein cargo))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
