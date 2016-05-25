;;;; ============================================================+
;;;; Personal Information                                        |
;;;; ------------------------------------------------------------+
(setq user-full-name "Break Yang"
      user-mail-address "breakds@gmail.com")

;;;; ============================================================+
;;;; Global Functions                                            |
;;;; ------------------------------------------------------------+

(defun ensure-package-installed (&rest packages)
  ;; http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (when (y-or-n-p (format "Package %s is missing. Install it?" package))
              (package-install package))))
        packages))

;;;; ============================================================+
;;;; Global Settings                                             |
;;;; ------------------------------------------------------------+
;;; * Disable Toolbar
(tool-bar-mode -1)

;;; * Change backup directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;; * Change auto-save settings
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t)))

;;; * Make the kill ring larger
(setq kill-ring-max 5000)

;;; * Visual bell (as alternative to beep)
(setq visible-bell 't)

;;; * Font. Font size is in 1/10 pt
(set-face-attribute 'default nil :height 95)

;;; * X Window Clipbord (enable utf-8)
(set-clipboard-coding-system 'utf-8)
(setq x-select-enable-clipboard t)

;;; * Sentence should end with single space.
(setq sentence-end-double-space nil)

;;; * Time display in 24 hour format
(setq display-time-24hr-format t)
(display-time)

;;; * Yes -> y, No -> n
(fset 'yes-or-no-p 'y-or-n-p)

;;; * Getting back to the mark.
;;;   Note that mark is set with C-<space>
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;;; * Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)
(bind-key "C-x r" 'recentf-open-files)

;;; * Avoid huge jump when scrolling
(setq scroll-step 1) 
(defun scroll-up-slightly () (interactive) (scroll-up 3))
(defun scroll-down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-4] 'scroll-down-slightly)
(global-set-key [mouse-5] 'scroll-up-slightly)

;;; * Maximized on start up
(defun toggle-maximized ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(toggle-maximized)

;;; * A more convenient buffer list
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;;; * Make emacs respect the bash environment variables.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
            

;;;; ============================================================+
;;;; Theme Settings                                              |
;;;; ------------------------------------------------------------+
(load-theme 'solarized-light t)
