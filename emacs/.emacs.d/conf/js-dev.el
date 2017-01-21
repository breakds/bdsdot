(require 'web-mode)

;;;; ============================================================+
;;;; Indentation                                                 |
;;;; ------------------------------------------------------------+

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; http://stackoverflow.com/questions/17901900/how-can-i-set-a-tab-width-for-json-files
(add-hook 'json-mode-hook
          (lambda ()
            ;; Make js-indent-level local so that it does not conflict
            ;; with js-mode.
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;;;; ============================================================+
;;;; Associates files with web-mode                              |
;;;; ------------------------------------------------------------+

;;; * Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

;;; * JSX
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;;; * Typescript
(add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))

;;; * Typescript JSX
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;;; * Content Type "JSX"
(setq web-mode-content-types-alist
      '(("jsx" . "\\.ts[x]?\\'")
        ("jsx" . "\\.js[x]?\\'")))

;;;; ============================================================+
;;;; Tide (Typescript IDE)                                       |
;;;; ------------------------------------------------------------+

(defun setup-tide-mode()
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'web-mode-hook
	  (lambda ()
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))
