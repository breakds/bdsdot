;;;; C/C++ development appearance

;; (setq default-c-style "stroustrup")
(setq c-basic-indent 2)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(put 'eval-expression 'disabled nil)

;; Add some colors to emacs session
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


;; Associate .cu file with c++-mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))



;;;; Cedet Configration
;; documentation of semantic submodes can be found at 
;; www.gnu.org/software/emacs/manual/html_node/semantic/Semantic-mode.html
(load-file "~/.emacs.d/conf/google-c-style.el")
;; (load-file "~/.emacs.d/lisp/cedet/cedet-devel-load.el")
;; (add-to-list 'load-path "~/.emacs.d/lisp/cedet/contrib")
;; (add-to-list 'Info-directory-list "~/projects/cedet/doc/info")

;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
;;(add-to-list 'semantic-default-submodes ')

;; Activate semantic
;; (semantic-mode 1)
;; (require 'semantic/bovine/c)
;; (require 'semantic/bovine/gcc)
;; (require 'semantic/bovine/clang)
;; (require 'semantic/ia)
;; (require 'semantic/decorate/include)
;; (require 'semantic/lex-spp)
;; (require 'eassist)

;; (defun my-cedet-hook ()
;;   (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
;;   (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;;   (local-set-key "\C-c=" 'semantic-decoration-include-visit)
;;   ;; Press f11 to jump to tags, and Shift-F11 to jump back
;;   (local-set-key [f11] 'semantic-ia-fast-jump)
;;   (local-set-key [S-f11]
;;                  (lambda ()
;;                    (interactive)
;;                    (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
;;                        (error "Semantic Bookmark ring is currently empty"))
;;                    (let* ((ring (oref semantic-mru-bookmark-ring ring))
;;                           (alist (semantic-mrub-ring-to-assoc-list ring))
;;                           (first (cdr (car alist))))
;;                      (if (semantic-equivalent-tag-p (oref first tag)
;;                                                     (semantic-current-tag))
;;                          (setq first (cdr (car (cdr alist)))))
;;                      (semantic-mrub-switch-tags first))))
;;   (local-set-key "\C-cq" 'semantic-ia-show-doc)
;;   (local-set-key "\C-cs" 'semantic-ia-show-summary)
;;   (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
;;   (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
;;   (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block))
;; (add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;; (add-hook 'lisp-mode-hook 'my-cedet-hook)
;; (add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)

;; (defun my-c-mode-cedet-hook ()
;;   ;; (local-set-key "." 'semantic-complete-self-insert)
;;   ;; (local-set-key ">" 'semantic-complete-self-insert)
;;   (local-set-key "\C-ct" 'eassist-switch-h-cpp)
;;   (local-set-key "\C-xt" 'eassist-switch-h-cpp)
;;   (local-set-key "\C-ce" 'eassist-list-methods)
;;   (local-set-key "\C-c\C-r" 'semantic-symref))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)



;; (semanticdb-enable-gnu-global-databases 'c-mode t)
;; (semanticdb-enable-gnu-global-databases 'c++-mode t)

;; (defconst cedet-user-include-dirs
;;   (list ".." "../include" "../inc" "../common" "../public"
;;         "../.." "../../include" "../../inc" "../../common" "../../public"))


;; (let ((include-dirs cedet-user-include-dirs))
;;   (when (eq system-type 'windows-nt)
;;     (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
;;   (mapc (lambda (dir)
;;           (semantic-add-system-include dir 'c++-mode)
;;           (semantic-add-system-include dir 'c-mode))
;;         include-dirs))



;;;; Compilation Mode
;; copied from http://www.emacswiki.org/emacs/CompileCommand
(defun in-directory ()
  "Reads a directory name (using ido), then runs
execute-extended-command with default-directory in the given
directory."
  (interactive)
  (let ((default-directory 
          (ido-read-directory-name "In directory: "
                                   nil nil t)))
    (call-interactively 'execute-extended-command)))

;; copied from http://www.emacswiki.org/emacs/CompileCommand
(defun get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
		      (loop 
		       for d = default-directory then (expand-file-name ".." d)
		       if (file-exists-p (expand-file-name file d))
		       return d
		       if (equal d root)
		       return nil))))
(require 'compile)
;; Copied from http://www.emacswiki.org/emacs/CompileCommand
;; Modified by BreakDS
;; searching for "build" folder
(add-hook 'c-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "make -j4 -C %s" (get-closest-pathname "build")))))
(add-hook 'c++-mode-hook (lambda () (set (make-local-variable 'compile-command) (format "make -j4 -C %s" (get-closest-pathname "build")))))


;; Highlight Erros
;; Copied from http://www.emacswiki.org/emacs/CompilationMode
(defun my-recompile ()
  "Run compile and resize the compile window closing the old one if necessary"
  (interactive)
  (progn
    (if (get-buffer "*compilation*") ; If old compile window exists
  	(progn
  	  (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
  	  (kill-buffer "*compilation*") ; and kill the buffers
  	  )
      )
    (call-interactively 'compile)
    (enlarge-window 20)
    )
  )
(defun my-next-error () 
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(defun my-previous-error () 
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )
(global-set-key (kbd "M-n") 'my-next-error)
(global-set-key (kbd "M-p") 'my-previous-error)

(global-set-key [f5] 'my-recompile)

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
