(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(setq web-mode-content-types-alist
      '(("jsx" . "\\.ts[x]?\\'")
        ("jsx" . "\\.js[x]?\\'")))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
