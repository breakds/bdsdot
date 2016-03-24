;;;; ============================================================+
;;;; Python Development Settings                                 |
;;;; ------------------------------------------------------------+

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))
