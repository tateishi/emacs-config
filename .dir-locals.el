((emacs-lisp-mode
  (eval . (progn
            (setq-local flycheck-disabled-checkers
                        (append flycheck-disabled-checkers '(emacs-lisp-docstring)))))))
