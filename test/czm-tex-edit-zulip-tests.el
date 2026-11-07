;;; -*- lexical-binding: t; -*-
;;; czm-tex-edit-zulip-tests.el --- Copying LaTeX as Zulip math

(require 'ert)
(require 'czm-tex-edit)

(ert-deftest czm-tex-edit-copy-region-to-zulip ()
  (dolist (example
           '(("Price \\$5; $x$; $$y$$; f''; ``quoted''; unmatched ``."
              . "Price \\$5; $$x$$; $$y$$; f''; \"quoted\"; unmatched ``.")
             ("$x$$y$" . "$$x$$$$y$$")
             ("``The derivative $f''$ exists.''"
              . "\"The derivative $$f''$$ exists.\"")
             ("``Display $$f''$$ and $``x''$ quoted.''"
              . "\"Display $$f''$$ and $$``x''$$ quoted.\"")
             ("Before\n  \\begin{equation*}\n f''(x) = \\text{``yes''}\n \\end{equation*}\nAfter."
              . "Before\n```math\nf''(x) = \\text{``yes''}\n```\nAfter.")
             ("Before \\begin{equation*} x=1 \\end{equation*} after."
              . "Before \n```math\nx=1\n```\n after.")))
    (with-temp-buffer
      (insert (car example))
      (goto-char 3)
      (set-mark 5)
      (set-buffer-modified-p nil)
      (let ((kill-ring nil)
            (interprogram-cut-function nil))
        (czm-tex-edit-copy-region-to-zulip (point-min) (point-max))
        (should (equal (car kill-ring) (cdr example))))
      (should (equal (buffer-string) (car example)))
      (should (= (point) 3))
      (should (= (mark) 5))
      (should-not (buffer-modified-p)))))

(ert-deftest czm-tex-edit-copy-region-to-zulip-keeps-previous-kill-on-error ()
  (with-temp-buffer
    (insert "``Prose'' $x$ \\begin{equation*} incomplete")
    (let ((source (buffer-string))
          (position (point))
          (kill-ring '("previous kill"))
          (interprogram-cut-function nil))
      (should-error (czm-tex-edit-copy-region-to-zulip (point-min) (point-max))
                    :type 'user-error)
      (should (equal kill-ring '("previous kill")))
      (should (equal (buffer-string) source))
      (should (= (point) position)))))

;;; czm-tex-edit-zulip-tests.el ends here
