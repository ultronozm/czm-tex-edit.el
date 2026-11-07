;;; -*- lexical-binding: t; -*-
;;; czm-tex-edit-fold-tests.el --- Inserted-text folding tests

(require 'ert)
(require 'cl-lib)
(require 'czm-tex-edit)

(defmacro czm-tex-edit-test--with-folding (&rest body)
  "Run BODY in a LaTeX buffer with predictable fold specifications."
  (declare (indent 0))
  `(with-temp-buffer
     (LaTeX-mode)
     (setq-local TeX-fold-macro-spec-list
                 '(("[r]" ("ref" "eqref"))))
     (setq-local TeX-fold-region-functions nil)
     (TeX-fold-mode 1)
     ,@body))

(defun czm-tex-edit-test--folded-text ()
  "Return the text covered by each TeX fold in the current buffer."
  (mapcar (lambda (overlay)
            (buffer-substring-no-properties (overlay-start overlay)
                                            (overlay-end overlay)))
          (cl-remove-if-not
           (lambda (overlay) (eq (overlay-get overlay 'category) 'TeX-fold))
           (overlays-in (point-min) (point-max)))))

(ert-deftest czm-tex-edit-fold-yanked-references ()
  (advice-add 'yank :after #'czm-tex-edit-fold-yanked-references)
  (unwind-protect
      (dolist (prefix '(nil (4)))
        (czm-tex-edit-test--with-folding
          (insert "\\ref{old} ")
          (let ((kill-ring '("\\ref{a} and \\eqref{b}"))
                (kill-ring-yank-pointer nil))
            (yank prefix))
          (let ((folded (czm-tex-edit-test--folded-text)))
            (should (member "\\ref{a}" folded))
            (should (member "\\eqref{b}" folded))
            (should-not (member "\\ref{old}" folded)))
          (should (= (point) (if prefix 11 (point-max))))))
    (advice-remove 'yank #'czm-tex-edit-fold-yanked-references)))

;;; czm-tex-edit-fold-tests.el ends here
