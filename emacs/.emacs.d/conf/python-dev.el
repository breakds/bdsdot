;;;; ============================================================+
;;;; Python Development Settings                                 |
;;;; ------------------------------------------------------------+

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

(defun comint-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun comint-hook ()
  (local-set-key "\C-c\M-o" 'comint-clear))

(add-hook 'shell-mode-hook 'comint-hook)
