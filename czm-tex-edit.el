;;; czm-tex-edit.el --- Helper functions for editing LaTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
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



(defun czm-make-equation-numbered ()
  "Makes an equation numbered by inserting refs and labels, or remove equation labels."
  (interactive)
  (save-excursion
    (when (texmathp)
      (preview-clearout-at-point)
      (let ((env (car texmathp-why))
            (pos (cdr texmathp-why)))
        (cond
         ((or
	   (equal env '"\\[") (equal env '"$$"))
          (czm-handle-env env pos 2 (if (equal env "$$") "$$" "\\]") nil))
         ((equal env "$")
          (czm-handle-env env pos 1 "$" nil))
         ((equal env "equation*")
          (czm-handle-env env pos 0 "\\end{equation*}" t))
         ((equal env "equation")
          (czm-handle-env env pos 0 "\\end{equation}" nil)))))))

(defun czm-handle-env (env pos num-chars search-str numbered)
  "Helper function for czm-make-equation-numbered.

Remove part of the environment string and insert a numbered or unnumbered LaTeX equation environment at POS.  ENV specifies the name of the environment to match, NUM-CHARS specifies the number of characters to delete, SEARCH-STR specifies the string to search for and NUMBERED determines if the equation environment should be numbered."
  (goto-char pos)
  (delete-char num-chars)
  (when (or (equal env "equation") (equal env "equation*"))
    (kill-line))
  (push-mark)
  (search-forward search-str)
  (delete-region (match-beginning 0) (match-end 0))
  (when (equal env "$")
    (while (looking-at-p czm/punct-string)
      (forward-char)))
  (exchange-point-and-mark)
  (LaTeX-insert-environment (if numbered "equation" "equation*"))
  (when numbered
    (czm-add-and-save-label-at-beginning)))

```
;; Define a custom variable for a list of typical binary relations.
;; TODO: can probably extract this from some standard library
(defcustom my-latex-relations
  '("=" "\\leq" "<" ">" "\\geq" "\\ll" "\\gg" "\\nless" "\\ngtr" "\\sim" "\\simeq"
    "\\equiv" "\\approx" "\\asymp")
  "List of commonly used LaTeX binary relation symbols."
  :group 'my-latex-settings
  :type '(repeat string))

(setq  my-latex-relations
  '("\\leq"))

(setq my-latex-relations
  '("=" "<" ">")
  )


(defun add-line-breaks-and-alignment (beg end)
  "Adds a line break and prepends alignment character to all matches in the region between BEG and END in a LaTeX buffer."
  (interactive "r\nsList of substrings (space separated): ")
  (let* ((str (buffer-substring-no-properties beg end))
         (re (concat "\\(" (regexp-opt my-latex-relations 'words) "\\)"))
         (replace-str (concat "& " "\\&" "\\1 " "\\\\\n"))
         (new-str (replace-regexp-in-string re replace-str str)))
    (delete-region beg end)
    (insert new-str)))

(defun add-line-breaks-and-alignment (beg end)
  "Adds a line break and prepends alignment character to all matches in the region between BEG and END in a LaTeX buffer."
  (interactive "r\nsList of substrings (space separated): ")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((first t))
	(while (re-search-forward (concat (regexp-opt my-latex-relations 'symbols)) nil t)
	  (replace-match
	   (concat (unless first "\\\\\\\\\n") "&" (match-string 0)))
	  (when first (setq first nil))
	  )))))
;; OK, you should probably just make this optional.

(defun czm-make-equation-align ()
  (interactive)
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
	(LaTeX-insert-environment "align*")
	
	)
       ((equal (car texmathp-why) '"$")
	(goto-char (cdr texmathp-why))
	(delete-char 1)
	(push-mark)
	(search-forward "$")
	(delete-region (match-beginning 0) (match-end 0))
	(exchange-point-and-mark)
	(LaTeX-insert-environment "align*"))
       ((equal (car texmathp-why) "equation*")
	(LaTeX-modify-environment "align*")
	;; (let* ((bounds (current-latex-environment-bounds))
	;;        (beg (car bounds))
	;;        (end (cdr bounds)))
	;;   (add-line-breaks-and-alignment beg end))
	)
       ((equal (car texmathp-why) "equation")
	(LaTeX-modify-environment "align*"))
       ((equal (car texmathp-why) "align*")
	(LaTeX-modify-environment "equation*"))))))


;; (defun czm-preview-in-progress ()
;;   (let* ((name "Preview-LaTeX")
;; 	 (process (TeX-process name)))
;;     (eq (process-status process) 'run)))


(defun czm/latex-merge-equations-to-align (beg end)
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
      (insert "\\end{align*}\n"))))


(defcustom czm/punct-string "[\\.|,|;|!|?]" "Regexp for matching punctuation characters.")

(defun czm-make-equation-inline ()
  "Converts LaTeX equation environment at point to inlined math.

Format a LaTeX math environment at the current point by surrounding the math environment with dollar signs, removing any leading or trailing text."
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
	(beginning-of-buffer)
	(kill-line 1)
	(beginning-of-line-text)
	(delete-region (point-min) (point))
	(end-of-buffer)
	(forward-line -2)
	(end-of-line)
	(delete-region (point) (point-max))
	(whitespace-cleanup)
	(beginning-of-buffer)
	(insert "$")
	(end-of-buffer)
	(insert "\n")
	(backward-char)
	(while (looking-back czm/punct-string)
	  (backward-char))
	(insert "$")
	(while (> (count-lines (point-min) (point-max)) 1)
	  (join-line))
	(current-buffer))
      (join-line)
      (goto-char cur))))



(defun czm-repeat-most-recent-equation (&optional arg)
  (interactive "P")
  (let ((contents) (beg) (end))
    (save-excursion
      (search-backward "\\begin{equation" nil nil arg)
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
  (next-line))

(defun czm-repeat-line-contents ()
  (interactive)
  (beginning-of-line)
  (let ((beg (point)) (end) (contents))
    (end-of-line)
    (setq end (point))
    (setq contents (buffer-substring beg end))
    (save-excursion
      (insert (concat " " contents " <++>")))))

;	could combine these two functions using "if (use-region-p)"

(defun czm-repeat-region ()
  (interactive)
  (let ((beg (region-beginning)) (end (region-end)) (at-end (< (mark) (point))) (contents))
    (setq contents (buffer-substring beg end))
    (save-excursion
      (if at-end
	  (insert " <++>")
	(goto-char end))
      (insert (concat " " contents " <++>")))
    (exchange-point-and-mark)))

(defun czm-repeat-align-segment ()
  (interactive)
  (let* ((start (point))
	 (bound (save-excursion
		  (LaTeX-find-matching-begin)
		  (point)))
	 (end (search-backward "&" bound t)))
    (when end
      (goto-char start)
      (let ((text-to-copy (buffer-substring-no-properties (1+ end) start)))
	(insert "\\\\\n")
	(save-excursion
	  (insert "&" text-to-copy " <++>"))))))



(defun czm-latex-emphasize (beg end)
  (interactive "r")
  (czm-latex-macro-helper beg end "emph"))

(defun czm-latex-bold (beg end)
  (interactive "r")
  (czm-latex-macro-helper beg end "textbf"))

(defun czm-latex-underline (beg end)
  (interactive "r")
  (czm-latex-macro-helper beg end "underline"))

(defun czm-latex-macro-helper (beg end type)
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
	  (TeX-fold-macro))))
    ))
(defun czm-latex-create-color-function (color)
  (let ((function-name (intern (concat "czm-latex-textcolor-" color))))
    `(defun ,function-name (beg end)
       (interactive "r")
       (czm-latex-macro-helper beg end ,(concat "textcolor{" color "}")))))

(defun czm-latex-create-color-bindings (color key)
  (let* ((function-name (intern (concat "czm-latex-textcolor-" color)))
         (key-sequence (concat "C-c t c " key)))
    `(define-key LaTeX-mode-map (kbd ,key-sequence) ',function-name)))

(defmacro czm-latex-define-color-functions-and-bindings (color-key-pairs)
  `(progn
     ,@(mapcar (lambda (pair)
                 (let ((color (car pair))
                       (key (cdr pair)))
                   `(progn
                      ,(czm-latex-create-color-function color)
                      ,(czm-latex-create-color-bindings color key))))
               color-key-pairs)))




(defun czm-latex-textcolor-red (beg end)
  (interactive "r")
  (czm-latex-macro-helper beg end "textcolor{red}"))


(defun czm-latex-create-color-function (color)
  (let ((function-name (intern (concat "czm-latex-textcolor-" color))))
    `(defun ,function-name (beg end)
       (interactive "r")
       (czm-latex-macro-helper beg end ,(concat "textcolor{" color "}")))))

(defun czm-latex-create-color-bindings (color key)
  (let* ((function-name (intern (concat "czm-latex-textcolor-" color)))
         (key-sequence (concat "C-c t c " key)))
    `(global-set-key (kbd ,key-sequence) ',function-name)))


(defun czm-latex-unemphasize (beg end)
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

(defun czm-latex-external-document-link (beg end)
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





(defun czm-substackify ()
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

(defun czm-yank-interior-delete-delim ()
  (interactive)
  (forward-char)
  (let ((beg (point)))
    (backward-char)
    (forward-list)
    (backward-char)
    (kill-region beg (point)))
  (backward-char)
  (delete-char 2))

(defun czm-fractionify-region (beg end)
  (interactive "r")
  (let ((contents (buffer-substring beg end)) (num) (den))
    (with-temp-buffer
      (insert contents)
      (beginning-of-buffer)
      (search-forward "/")
      (setq num (buffer-substring (point-min) (match-beginning 0)))
      (setq den (buffer-substring (match-end 0) (point-max))))
    (when (and num den)
      (delete-region beg end)
      (insert "\\frac{" num "}{" den "}"))))

(defun czm-enlarge-parentheses ()
  (interactive)
  (when (looking-at "\\s\)")
    (czm-match-paren nil))
  (when (looking-at "\\s\(")
    (save-excursion
      (czm-match-paren nil)
      (when (looking-at "}") (backward-char))
      (insert "\\right"))
    (when (looking-at "}") (backward-char))
    (insert "\\left")))


(defun czm-split-equation (arg)
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
	(if arg
	    (LaTeX-fill-environment nil)))))))

(defun czm-exit-equation-save-and-previous-or-insert-item ()
  (interactive)
  (if (texmathp)
      (progn
	(call-interactively 'save-buffer)
	(evil-normal-state)
	;; (save-excursion
	;; 	 (LaTeX-fill-environment nil)
	;; )
	(save-excursion
	  (preview-clearout-at-point)
	  (ignore-errors
	    (preview-at-point)))
	(evil-forward-paragraph)
	(evil-next-line))
    (LaTeX-insert-item)))

(defun czm-exit-equation-save-and-previous-or-insert-item-without-evil
    ()
  (interactive)
  (if-let ((math-region (texmathp-region)))
      (let
	  ((beg (car math-region))
	   (end (cdr math-region)))
	(goto-char end)
	(unless (looking-back "\\$")
	  (forward-char)))
    (LaTeX-insert-item)))

(defun delete-commented-lines ()
  "Delete all lines in the current buffer that start with a percentage sign."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*%.*$" nil t)
      (delete-line))))

(defun latex-convert-display-math-to-equation-star ()
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

(defun czm-tex-fix-file ()
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
      (when (and (texmathp)
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

(provide 'czm-tex-edit)
;;; czm-tex-edit.el ends here
