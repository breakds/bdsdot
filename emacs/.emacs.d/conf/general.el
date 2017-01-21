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
;;; * Disable tab. Use space.
(setq-default indent-tabs-mode nil)

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

;;; * Always highlight current line
(global-hl-line-mode t)

;;; * Font. 

;; If you really want to set it in your emacs configuration, use the
;; one below. However, it is not portable and incompatible if you are
;; sharing this eamcs configuration between displays of different
;; resolutions (i.e. A normal monitor and a HiDpi one).
;;
;; The recommended way is to set "emacs.font" in ~/.Xresources
;;
;; (set-frame-font "DejaVu Sans Mono-11")

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
  ;; Only run maximization when the underlying window manager is X
  ;; Windows.
  (when (equal window-system "x")
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))
(toggle-maximized)

;;; * A more convenient buffer list
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;;; * Make emacs respect the bash environment variables.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")

;;; * IME: fcitx
(fcitx-aggressive-setup)
(setq fcitx-use-dbus t)
            
;;;; ============================================================+
;;;; Theme Settings                                              |
;;;; ------------------------------------------------------------+
(load-theme 'solarized-light t)
