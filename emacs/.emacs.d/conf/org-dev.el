;;;; ============================================================+
;;;; Load org modules                                            |
;;;; ------------------------------------------------------------+
(use-package org
  :init
  ;; Time tracking features
  (progn
    ;; Prompt me about the idle time if I am AFK for 15 minutes or longer.
    (setq org-clock-idle-time 15)
    ;; Log the timestamp when a task is done.
    (setq org-log-done 'time)
    ;; Do not start clocking from the timestamp of last clock-out.
    (setq org-clock-continuously nil)
    ;; Save unfinished clock when emacs is closed.
    (setq org-clock-persist t)
    ;; Switch state to ONGOING when clocked in.
    (setq org-clock-in-switch-to-state "ONGOING")
    (setq org-clock-in-resume nil)
    (setq org-show-notification-handler 'message)
    ;; When non-nil, include the current clocking task time in clock
    ;; reports.
    (setq org-clock-report-include-clocking-task t))
  :config
  (setq org-startup-indented t)
  (org-clock-persistence-insinuate))

(add-to-list 'org-modules
             'org-habit
             'org-expiry)
(require 'org-install)
;; org-habit won't be enabled unless it is required.
(require 'org-habit)

;;;; ============================================================+
;;;; Detailed Configurations                                     |
;;;; ------------------------------------------------------------+

;;; * org directory
(setq org-directory "~/org")

;;; * org-expiry
(setq org-expiry-inactive-timestamps t)

;;; * TODO/DONE and states
(setq org-todo-keywords
      '((sequence "TODO(t)" "ONGOING(g)" "PAUSE(p)" "|" "DONE(d)" "DELEGATED(e)" "CANCELLED(c@)")))
;;;   Font faces of the states
(setq org-todo-keyword-faces
      '(("ONGOING" . (:foreground "blue" :weight bold))
        ("PAUSE" . (:foreground "red" :weight bold))
        ("DELEGATED" . (:foreground "orange" :weight bold))))

;;; * Refile
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((nil . (:maxlevel . 2))
                           (org-agenda-files . (:maxlevel . 2))))

;;; * Taking notes on by inserting links.
(defun my/yank-link ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "C-c y") 'my/yank-link)

;;; * Capture
(setq org-default-notes-file "~/org/unsorted.org")
(bind-key "C-c c" 'org-capture)

;;; * Habits
(setq org-habit-graph-column 105)
(setq org-habit-show-haibts-only-for-today nil)
(setq org-log-repeat 'time)

;;;; ============================================================+
;;;; org capture templates                                       |
;;;; ------------------------------------------------------------+
(setq org-capture-templates
      '(("T" "Quick Tasks" entry
         (file+headline "~/org/unsorted.org" "Tasks")
         "* TODO %?\n %i\n  %a")))

;;;; ============================================================+
;;;; org agenda customization                                    |
;;;; ------------------------------------------------------------+
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-custom-commands
      '(("w" "Weekly Review" todo "DONE"
         ((org-agenda-files '("~/org/projects.org"
                              "~/org/unsorted.org"
                              "~/org/reading.org"
                              "~/org/projects.org_archive"))
          (org-agenda-span 7)))))



