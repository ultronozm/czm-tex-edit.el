;;; czm-tex-edit.el --- Helper functions for editing LaTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-edit.el
;; Package-Requires: ((emacs "29.1") (dynexp "0.0") (auctex))
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Helper functions for editing LaTeX.

;;; Code:

(require 'tex)
(require 'latex)
(require 'preview)
(require 'dynexp)

;;;###autoload
(defun czm-tex-edit-make-equation-numbered ()
  "Toggle whether an equation is numbered."
  (interactive)
  (save-excursion
    (when (texmathp)
      (preview-clearout-at-point)
      (let ((env (car texmathp-why))
            (pos (cdr texmathp-why)))
        (cond
         ((or
           (equal env '"\\[") (equal env '"$$"))
          (czm-tex-edit--handle-env env pos 2 (if (equal env "$$") "$$" "\\]") nil))
         ((equal env "$")
          (czm-tex-edit--handle-env env pos 1 "$" nil))
         ((equal env "equation*")
          (czm-tex-edit--handle-env env pos 0 "\\end{equation*}" t))
         ((equal env "equation")
          (czm-tex-edit--handle-env env pos 0 "\\end{equation}" nil)))))))

(defcustom czm-tex-edit-label-function
  'reftex-label
  "Function to use for adding a label to a LaTeX environment."
  :group 'czm-tex-edit
  :type 'function)

(defun czm-tex-edit-add-label ()
  "Add LaTeX label to matching \\begin{} at environment beginning.
Also, save label to kill ring as an \\eqref{} command."
  (interactive)
  (save-excursion
    (LaTeX-find-matching-begin)
    (end-of-line)
    (funcall czm-tex-edit-label-function)
    (let ((end (point)))
      (search-backward "{")
      (let ((beg (point)))
        (kill-new (concat "\\eqref" (buffer-substring beg end)))))))

(defcustom czm-tex-edit-punctuation-string
  "[\\.|,|;|!|?]"
  "Regexp for matching punctuation characters."
  :group 'czm-tex-edit
  :type 'regexp)

(defun czm-tex-edit--handle-env (env pos num-chars search-str numbered)
  "Helper function for `czm-tex-edit-make-equation-numbered'.

Remove part of the environment string and insert a numbered or
unnumbered LaTeX equation environment at POS.  ENV specifies the
name of the environment to match, NUM-CHARS specifies the number
of characters to delete, SEARCH-STR specifies the string to
search for and NUMBERED determines if the equation environment
should be numbered."
  (goto-char pos)
  (delete-char num-chars)
  (when (or (equal env "equation") (equal env "equation*"))
    (kill-line))
  (push-mark (save-excursion
               (search-forward search-str)
               (delete-region (match-beginning 0) (match-end 0))
               (when (equal env "$")
                 (while (looking-at-p czm-tex-edit-punctuation-string)
                   (forward-char)))
               (point)))
  (activate-mark)
  (LaTeX-insert-environment (if numbered "equation" "equation*"))
  (when numbered
    (czm-tex-edit-add-label)))

;;;###autoload
(defun czm-tex-edit-make-equation-align ()
  "Toggle LaTeX environment between `equation' and `align'."
  (interactive)
  (czm-tex-edit--make-equation-helper "align"))

;;;###autoload
(defun czm-tex-edit-make-equation-multline ()
  "Toggle LaTeX environment between `equation' and `multline'."
  (interactive)
  (czm-tex-edit--make-equation-helper "multline"))

(defun czm-tex-edit--make-equation-helper (type)
  "Helper function for between equation/align/multline envs.
TYPE is either \"align\" or \"multline\"."
  (let ((type* (concat type "*")))
    (save-excursion
      (when (texmathp)
        (preview-clearout-at-point)
        (cond
         ((equal (car texmathp-why) '"\\[")
          (goto-char (cdr texmathp-why))
          (delete-char 2)
          (push-mark)
          (search-forward "\\]")
          (delete-region (match-beginning 0) (match-end 0))
          (exchange-point-and-mark)
          (activate-mark)
          (LaTeX-insert-environment type*))
         ((equal (car texmathp-why) '"$")
          (goto-char (cdr texmathp-why))
          (delete-char 1)
          (push-mark)
          (search-forward "$")
          (delete-region (match-beginning 0) (match-end 0))
          (exchange-point-and-mark)
          (activate-mark)
          (LaTeX-insert-environment type*))
         ((equal (car texmathp-why) type*)
          (LaTeX-modify-environment "equation*"))
         ((member (car texmathp-why) '("equation" "equation*" "align" "align*" "multline" "multline*"))
          (LaTeX-modify-environment type*)))))))


;;;###autoload
(defun czm-tex-edit-merge-equations-to-align (beg end)
  "Merge LaTeX equations between BEG and END into an align."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt '("\\begin{equation*}" "\\begin{align*}")) nil t)
        (replace-match "")
        (kill-line))
      (goto-char (point-min))
      (while (re-search-forward
              (concat
               (regexp-opt '("\\begin{equation}" "\\begin{align}"))
               "\\\\label{\\([^}]+\\)}")
              nil t)
        (replace-match "\\\\label{\\1}")
        (kill-line))
      (goto-char (point-min))
      (while (re-search-forward
              (regexp-opt '("\\begin{equation}" "\\begin{align}"))
              nil t)
        (replace-match "")
        (kill-line))
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt '("\\end{equation*}" "\\end{align*}" "\\end{equation}" "\\end{align}")) nil t)
        (replace-match "\\\\\\\\")
        (join-line))
      (goto-char (point-min))
      (insert "\\begin{align*}\n")
      (goto-char (point-max))
      (insert "\\end{align*}\n")
      ;; search backwards for most recent \\ and delete that
      (re-search-backward "\\\\\\\\")
      (delete-region (point) (+ 2 (point))))))

;;;###autoload
(defun czm-tex-edit-make-equation-inline ()
  "Convert LaTeX equation environment at point to inlined math.
Format LaTeX environment at point by surrounding the math
environment with dollar signs, removing any leading or trailing
text."
  (interactive)
  (when (texmathp)
    (preview-clearout-at-point)
    (let ((cur (point-marker)) beg end)
      (save-excursion
        (LaTeX-find-matching-end)
        (setq end (line-beginning-position 2))
        (goto-char cur)
        (LaTeX-find-matching-begin)
        (setq beg (point)))
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (kill-line 1)
        (beginning-of-line-text)
        (delete-region (point-min) (point))
        (goto-char (point-max))
        (forward-line -2)
        (end-of-line)
        (delete-region (point) (point-max))
        (whitespace-cleanup)
        (goto-char (point-min))
        (insert "$")
        (goto-char (point-max))
        (insert "\n")
        (backward-char)
        (while (looking-back czm-tex-edit-punctuation-string 5)
          (backward-char))
        (insert "$")
        (while (> (count-lines (point-min) (point-max)) 1)
          (join-line))
        (current-buffer))
      (join-line)
      (goto-char cur))))





;;;###autoload
(defun czm-tex-edit-repeat-most-recent-equation (&optional n)
  "Repeat a recent equation environment.
By default, repeat the most recent equation.  If the optional
argument N is non-nil, then repeat the Nth most recent
equation."
  (interactive "p")
  (let ((contents) (beg) (end))
    (save-excursion
      (search-backward "\\begin{equation" nil nil n)
      (end-of-line)
      (when (texmathp)
        (cond
         ((equal (car texmathp-why) "equation*")
          (goto-char (cdr texmathp-why))
          (setq beg (point))
          (search-forward "\\end{equation*}")
          (setq end (match-end 0)))
         ((equal (car texmathp-why) "equation")
          (goto-char (cdr texmathp-why))
          (setq beg (point))
          (search-forward "\\end{equation}")
          (setq end (match-end 0))))
        (setq contents (buffer-substring beg end))))
    (end-of-line)
    (reindent-then-newline-and-indent)
    (save-excursion
      (insert contents)))
  (forward-line))

;;;###autoload
(defun czm-tex-edit-repeat-line-contents ()
  "Repeat the contents of the current line."
  (interactive)
  (beginning-of-line)
  (let ((beg (point)) (end) (contents))
    (end-of-line)
    (setq end (point))
    (setq contents (buffer-substring beg end))
    (save-excursion
      (insert (concat " " contents " <++>")))))

;;;###autoload
(defun czm-tex-edit-repeat-region ()
  "Repeat the contents of the current region."
  (interactive)
  (let ((beg (region-beginning)) (end (region-end)) (at-end (< (mark) (point))) (contents))
    (setq contents (buffer-substring beg end))
    (save-excursion
      (if at-end
          (insert " <++>")
        (goto-char end))
      (insert (concat " " contents " <++>")))
    (exchange-point-and-mark)))

;;;###autoload
(defun czm-tex-edit-repeat-align-segment ()
  "Repeat the contents of the current align segment."
  (interactive)
  (let* ((start (point))
         (bound (save-excursion
                  (LaTeX-find-matching-begin)
                  (point)))
         (end (search-backward "&" bound t)))
    (if end
        (let ((text-to-copy (buffer-substring-no-properties (1+ end) start)))
          (goto-char start)
          (insert "\\\\\n")
          (save-excursion
            (insert "&" text-to-copy " <++>"))
          ;; (LaTeX-indent-line)
          ; this doesn't work as it should -- need to figure out a way
          ; to execute a "next" command?
          )
      (let ((text-to-copy (buffer-substring-no-properties
                           (save-excursion
                             (goto-char bound)
                             (forward-line 1)
                             (point))
                           start)))
        (save-excursion
          (insert "&= " (string-trim text-to-copy) " <++>"))))))



;;;###autoload
(defun czm-tex-edit-emphasize (beg end)
  "Emphasize the region between BEG and END."
  (interactive "r")
  (czm-tex-edit-macro-helper beg end "emph"))

(defun czm-tex-edit-alertify (beg end)
  "Emphasize the region between BEG and END."
  (interactive "r")
  (czm-tex-edit-macro-helper beg end "alert"))

;;;###autoload
(defun czm-tex-edit-bold (beg end)
  "Bold the region between BEG and END."
  (interactive "r")
  (czm-tex-edit-macro-helper beg end "textbf"))

;;;###autoload
(defun czm-tex-edit-underline (beg end)
  "Underline the region between BEG and END."
  (interactive "r")
  (czm-tex-edit-macro-helper beg end "underline"))

(defun czm-tex-edit-macro-helper (beg end type)
  "Helper function for emph/bold/underline macros.
BEG and END specify the region to emphasize/bold/underline.
TYPE specifies the type of macro to use."
  (save-excursion
    (goto-char end)
    (let ((end-marker (point-marker)))
      (insert "}")
      (goto-char beg)
      (insert
       (concat
        "\\"
        type
        "{"))
      (font-lock-ensure beg end-marker)
      (let ((beginnings
             (dynexp-split-macro beg)))
        (dolist (b beginnings)
          (goto-char b)
          (when TeX-fold-mode
            (TeX-fold-macro)))))))

(defun czm-tex-edit-create-color-function (color)
  "Create a function for coloring text with COLOR."
  (let ((function-name (intern (concat "czm-tex-edit-textcolor-" color))))
    `(defun ,function-name (beg end)
       (interactive "r")
       (czm-tex-edit-macro-helper beg end ,(concat "textcolor{" color "}")))))

(defun czm-tex-edit-create-color-bindings (color key prefix)
  "Create keybindings for coloring text with COLOR.
PREFIX and KEY specify the key to use for the keybinding."
  (let* ((function-name (intern (concat "czm-tex-edit-textcolor-" color)))
         (key-sequence (concat prefix " " key)))
    `(define-key LaTeX-mode-map (kbd ,key-sequence) ',function-name)))

;;;###autoload
(defmacro czm-tex-edit-define-color-functions-and-bindings (prefix color-key-pairs)
  "Define color functions and keybindings for COLOR-KEY-PAIRS.
PREFIX specifies the prefix to use for the keybindings."
  `(progn
     ,@(mapcar (lambda (pair)
                 (let ((color (car pair))
                       (key (cdr pair)))
                   `(progn
                      ,(czm-tex-edit-create-color-function color)
                      ,(czm-tex-edit-create-color-bindings color key prefix))))
               color-key-pairs)))

;; (defun czm-tex-edit-create-color-bindings-2 (color key)
;;   "Create keybindings for coloring text with COLOR.
;; KEY specifies the key to use for the keybinding.

;; "
;;   (let* ((function-name (intern (concat "czm-tex-edit-textcolor-" color)))
;;          (key-sequence (concat "C-c t c " key)))
;;     `(global-set-key (kbd ,key-sequence) ',function-name)))


;;;###autoload
(defun czm-tex-edit-unemphasize (beg end)
  "Unemphasize the region between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward
              (concat
               "\\\\"
               (regexp-opt '("emph" "textbf" "textit"))
               "{\\([^}]*\\)}")
              nil t)
        (let ((contents (match-string 1)))
          (TeX-fold-clearout-region (match-beginning 0) (match-end 0))
          (replace-match contents)))
      (while (re-search-forward
              (concat
               "\\\\textcolor"
               "{[^}]*}{\\([^}]*\\)}")
              nil t)
        (let ((contents (match-string 1)))
          (TeX-fold-clearout-region (match-beginning 0) (match-end 0))
          (replace-match contents t))))))

;;;###autoload
(defun czm-tex-edit-external-document-link (beg end)
  "Create a link to an external document between BEG and END."
  (interactive "r")
  (let ((external-documents
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (let (matches)
               (while (re-search-forward "\\\\externaldocument{\\([^}]*\\)}" nil t)
                 (push (match-string-no-properties 1) matches))
               (nreverse matches)))))
        (initial-text (buffer-substring-no-properties beg end)))
    (if external-documents
        (let* ((selected-document (completing-read "Select external document: "
                                                   external-documents nil t))
               (new-text (format "\\href{%s.pdf}{%s}" selected-document initial-text)))
          (delete-region beg end)
          (insert new-text))
      (message "No \\externaldocument{} macros found in this buffer."))))





;;;###autoload
(defun czm-tex-edit-substackify ()
  "Add a substack to current subscript block."
  (interactive)
  (search-forward "}")
  (backward-list)
  (forward-char)
  ;; (search-backward "{")
  ;; (forward-char)
  (let ((beg (point)))
    (backward-char)
    (forward-list)
    (backward-char)
    (let ((contents (buffer-substring beg (point))))
      (delete-region beg (point))
      (delete-char 1)
      (newline)
      (insert "\\substack{")
      (newline)
      (insert contents " \\\\")
      (newline)
      (save-excursion
        (newline)
        (insert "}")
        (newline)
        (insert "}<++>")
        (LaTeX-fill-region beg (point)))))
  (indent-for-tab-command))

;;;###autoload
(defun czm-tex-edit-yank-interior-delete-delim ()
  "Yank the interior of a LaTeX environment and delete the delimiters."
  (interactive)
  (forward-char)
  (let ((beg (point)))
    (backward-char)
    (forward-list)
    (backward-char)
    (kill-region beg (point)))
  (backward-char)
  (delete-char 2))

;;;###autoload
(defun czm-tex-edit-fractionify-region (beg end)
  "Turn a region between BEG and END into a \"\\frac\" LaTeX command."
  (interactive "r")
  (let ((contents (buffer-substring beg end)) (num) (den))
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (search-forward "/")
      (setq num (buffer-substring (point-min) (match-beginning 0)))
      (setq den (buffer-substring (match-end 0) (point-max))))
    (when (and num den)
      (delete-region beg end)
      (insert "\\frac{" num "}{" den "}"))))

;;;###autoload
(defun czm-tex-edit-enlarge-parentheses ()
  "Enlarge parentheses at point."
  (interactive)
  (when (looking-at "\\s\)")
    (forward-char)
    (backward-list))
  (when (looking-at "\\s\(")
    (save-excursion
      (forward-list)
      (backward-char)
      (when (looking-at "}") (backward-char))
      (insert "\\right"))
    (when (looking-at "}") (backward-char))
    (insert "\\left")))

;;;###autoload
(defun czm-tex-edit-split-equation (arg)
  "Split equation environment into two equation environments.
If ARG is non-nil, then fill and indent afterwards."
  (interactive "P")
  (when (texmathp)
    (cond
     ((equal (car texmathp-why) "equation*")
      (beginning-of-line)
      (insert "\\end{equation*}")
      (newline)
      (save-excursion
        (insert "\\begin{equation*}")
        (newline)
        (when arg
            (LaTeX-fill-environment nil)))))))

(defun czm-tex-edit--texmathp-region ()
  "Get the math region containing point in a LaTeX buffer.
Currently only supports $...$ and begin/end blocks."
  (interactive)
  (when (texmathp)
    (let* ((math-start (cdr texmathp-why))
           (math-end (save-excursion
                       (goto-char math-start)
                       (cond
                        ;; Toggle math mode
                        ((looking-at-p "\\$")
                         (search-forward "$" nil t 2))
                        ;; Environment math mode
                        (t
                         (end-of-line)
                         (LaTeX-find-matching-end)
                         (end-of-line)))
                       (point))))
      (cons math-start math-end))))


;;;###autoload
(defun czm-tex-edit-return ()
  "Exits from equation environments, otherwise behaves like RET."
  (interactive)
  (if-let ((math-region (czm-tex-edit--texmathp-region)))
      (let ((_beg (car math-region))
            (end (cdr math-region)))
        (goto-char end)
        (unless (looking-back "\\$" 1)
          (forward-char)))
    (LaTeX-insert-item)))

;;;###autoload
(defun czm-tex-edit-delete-commented-lines ()
  "Delete all lines in the current buffer that start with a percentage sign."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*%.*$" nil t)
      (delete-line))))

;;;###autoload
(defun czm-tex-edit-convert-display-math-to-equation-star ()
  "Convert LaTeX displayed math blocks to equation* environments."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\[\n?\\([[:space:]]*\\)" nil t)
      (replace-match "\n\\\\begin{equation*}\n\\1" t))
    (goto-char (point-min))
    (while (re-search-forward "\\([[:space:]]*\\)\\\\\\]\n?" nil t)
      (replace-match "\\1\n\\\\end{equation*}\n" t))
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\begin{equation\\*}\n[[:space:]]*" nil t)
      (replace-match "\\\\\\begin{equation\\*}\n" t))
    (goto-char (point-min))
    (while (re-search-forward "[[:space:]]*\\\\\\end{equation\\*}" nil t)
      (replace-match "\n\\\\end{equation*}" t))))

;;;###autoload
(defun czm-tex-edit-fix-buffer ()
  "Put LaTeX buffer in the format you like.
Replace \\[...\\] and $$...$$ with equation* blocks.
Make sure each \\begin{...} and \\end{...} block appears on its own line."
  (interactive)
  (save-excursion
    ;; first, we replace \[...\] with \begin{equation*}...\end{equation*}
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\[" nil t)
      (replace-match "\\\\begin{equation*}"))
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\]" nil t)
      (replace-match "\\\\end{equation*}"))

    ;; next, we search through for $$...$$ and replace them with
    ;; \begin{equation*}...\end{equation*}.  to do this, we first look
    ;; for $$ such that when we position the cursor just after it, we
    ;; are in math mode according to texmathp.
    (goto-char (point-min))
    (while (re-search-forward "\\$\\$" nil t)
      (when (and ;; (texmathp)
             (let ((face (plist-get (text-properties-at (point))
                                    'face)))
               (or (eq face 'font-latex-math-face)
                   (and (listp face)
                        (memq 'font-latex-math-face face))))
             ;; check whether % appears between point and line beginning.
             ;; if so, we are in a comment and should ignore this match.
             ;; first, remember the current value of point
             (save-excursion
               (save-match-data
                 (let ((p (point)))
                   (beginning-of-line)
                   (not (search-forward "%" p t))))))
        (replace-match "\\\\begin{equation*}")
        (re-search-forward "\\$\\$" nil t)
        (replace-match "\\\\end{equation*}")))

    ;; We make sure that each
    ;; \begin{...} is the first non-whitespace part of the line that
    ;; it appears in, but we don't introduce unnecessary newlines.
    (dolist (str '("\\\\begin{\\([^}]+\\)}"
                   "\\\\end{\\([^}]+\\)}"))
      (goto-char (point-min))
      (while (re-search-forward str nil t)
        ;;  check that we aren't on a commented line
        (unless (save-excursion
                  (beginning-of-line)
                  (looking-at-p "[[:space:]]*%"))
          ;; check that we only have whitespace before
          (unless
              (save-excursion
                (beginning-of-line)
                (looking-at-p
                 (concat
                  "[[:space:]]*"
                  str)))
            (save-excursion
              (backward-char (length (match-string 0)))
              (insert "\n")))
          ;;  check that we don't have anything non-whitespace after except possibly \label{...}
          ;;  first, move past any \label{...}
          (while (re-search-forward "\\\\label{[^}]+}" (line-end-position) t))
          ;;  then, check that we have only whitespace up to the end
          ;;  of the line
          (unless (looking-at-p "[[:space:]]*$")
            (insert "\n")))))))

;;;###autoload
(defun czm-tex-edit-fix-latex-hyperref-section-warnings ()
  "Wrap math expressions in a section title with \\texorpdfstring."
  (interactive)
  (when (looking-at-p
         (regexp-opt
          '("\\section" "\\subsection" "\\subsubsection" "\\chapter" "\\part")))
    (let ((start (point))
          (end (progn (forward-list) (point))))
      (goto-char end)
      (while (re-search-backward "\\$\\([^$]+\\)\\$" start t)
        (let ((beg (match-beginning 0))
              (math (match-string 0))
              replacement)
          (save-match-data
            (unless (looking-back "\\texorpdfstring{[^}]*" (line-beginning-position))
              (let ((plain (read-string
                            (format "Plain text for math expression %s: " math))))
                (setq replacement (concat "\\texorpdfstring{" math "}{" plain "}")))))
          (when replacement
            (replace-match replacement t t)
            (goto-char beg))))
      ;; now same thing, but for \(...\) rather than $...$:
      ;; (goto-char end)
      ;; (while (re-search-backward "\\\\\\([^\\]+\\\\\\)" start t)
      ;;   (let ((beg (match-beginning 0))
      ;;         (math (match-string 0))
      ;;         replacement)
      ;;     (save-match-data
      ;;       (unless (looking-back "\\texorpdfstring{[^}]*" (line-beginning-position))
      ;;         (let ((plain (read-string
      ;;                       (format "Plain text for math expression %s: " math))))
      ;;           (setq replacement (concat "\\texorpdfstring{" math "}{" plain "}")))))
      ;;     (when replacement
      ;;       (replace-match replacement nil t)
      ;;       (goto-char beg))))
      )))


;;;###autoload
(defun czm-tex-edit-insert-dollar-or-wrap-region (arg)
  "Insert $ or surround region (if active) with $'s.
Defers to `TeX-insert-dollar' if no region is active.

See `TeX-insert-dollar-action' for more information on the
behavior, as well as the role of ARG."
  (interactive "*P")
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "$")
        (goto-char start)
        (insert "$"))
    (TeX-insert-dollar arg)))

;;;###autoload
(defun czm-tex-edit-insert-quote-or-wrap-region ()
  "Insert quote or surround region (if active) with quotes."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char end)
        (TeX-insert-quote nil)
        (goto-char start)
        (TeX-insert-quote nil))
    (TeX-insert-quote nil)))

(provide 'czm-tex-edit)
;;; czm-tex-edit.el ends here
