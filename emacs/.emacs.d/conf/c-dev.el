;;;; C/C++ development appearance

;;;; ============================================================+
;;;; Global Font Lock Mode for Syntax Highlighting               |
;;;; ------------------------------------------------------------+
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;;;; ============================================================+
;;;; C++ IDE Configurations                                      |
;;;; ------------------------------------------------------------+

;; cmake-ide
;; 
;; -- Enable cmake-ide which set corresponding variables based on the
;; -- CMakeLists.txt if it can find one.
(cmake-ide-setup)

;; Formatter
;;
;; -- Use clang-format to control the style of the code, and format it
;; -- automatically. The style is defined in a .clang-format file
;; -- within the project.
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

(defun clang-format-region-and-back-to-indentation ()
  "Call clang-format to format the whole region, and move the
  cursor to the first non-space character of the current line."
    (interactive)
    (clang-format-region (line-beginning-position) (line-end-position))
    (back-to-indentation))
  
(defun clang-format-bindings ()
  "Hijack the tab key to perform the function defined above,
  which is `clang-format-region-and-back-to-indentation`."
  (define-key c++-mode-map [tab]
    'clang-format-region-and-back-to-indentation))

(add-hook 'c++-mode-hook 'clang-format-bindings)

;; Code Indexer (RTags)
;;
;; -- This is for definition jumping and code completion
(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,") (function rtags-location-stack-back))
;; ;; Other shortcuts defined in the standard-keybndings
;; (define-key c-mode-base-map (kbd "C-M-a") (function beginning-of-defun))
;; (define-key c-mode-base-map (kbd "C-M-e") (function end-of-defun))
;; (define-key c-mode-base-map (kbd "C-M-h") (function makr-defun))
;; (define-key c-mode-base-map (kbd "C-c-r ,") (function rtags-find-references-at-point))

;; Syntax Checking
;;
;; -- Call flycheck (together with RTags) to do syntax check.
(require 'flycheck-rtags)
(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;; Compilation Support
;;
;; -- CMake based compilation key bindings.

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
(defun get-closest-pathname (file)
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







