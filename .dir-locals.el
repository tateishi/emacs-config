((emacs-lisp-mode
  (eval . (progn
            (setq-loal flycheck-disabled-checkers
                       (append flycheck-disabled-checkers '(emacs-lisp-docstring)))))))
