;;; ellsp-completion.el --- Completion Handler  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Completion Handler.
;;

;;; Code:

(cl-defun ellsp--list-completion-items (list &key transform kind)
  ""
  (setq transform (or transform #'identity))
  (apply #'vector
         (mapcar
          (lambda (item)
            (let ((completion (funcall transform item)))
              (lsp-make-completion-item
               :label (if (listp completion)
                          (plist-get completion :label)
                        completion)
               :kind (if (listp completion)
                         (plist-get completion :kind)
                       (or kind lsp/completion-item-kind-text)))))
          list)))

(defun ellsp--completions-bounds ()
  ""
  (with-syntax-table emacs-lisp-mode-syntax-table
    (message "completion bounds point %s" (point))
    (let* ((pos (point))
           (beg (condition-case nil
                    (save-excursion
                      (backward-sexp 1)
                      (skip-chars-forward "`',‘#")
                      (point))
                  (scan-error pos)))
           (end
            (unless (or (eq beg (point-max))
                        (member (char-syntax (char-after beg))
                                '(?\" ?\()))
              (condition-case nil
                  (save-excursion
                    (goto-char beg)
                    (forward-sexp 1)
                    (skip-chars-backward "'’")
                    (when (>= (point) pos)
                      (point)))
                (scan-error pos)))))
      (list beg end))))

(defun ellsp--function-completions ()
  ;; only used to extract start and end... we can reimplement it later
  (-when-let ((beg end) (ellsp--completions-bounds))
    (message "bounds %s %s" beg end)
    (let* ((candidates nil)
           (funpos (eq (char-before beg) ?\())
           (prefix (buffer-substring-no-properties beg end)))
      (message "prefix %s" prefix)
      (cond
       (funpos
        (maphash
         (lambda (k v)
           (when (string-prefix-p prefix (symbol-name k))
             (push v candidates)))
         (oref elsa-global-state defuns))
        (message "has %d functions for prefix %s" (length candidates) prefix)))
      candidates)))

(defun ellsp--capf-completions ()
  "Fallback completions engine is the `elisp-completion-at-point'."
  (-when-let ((start end table . props) (elisp-completion-at-point))
    (-let* ((predicate (plist-get props :predicate))
            (prefix (buffer-substring-no-properties start end))
            (meta (completion-metadata prefix table predicate))
            (candidates (completion-all-completions
                         prefix table predicate (length prefix) meta))
            (last (last candidates))
            (base-size (and (numberp (cdr last)) (cdr last))))
      (when base-size
        (setcdr last nil))
      candidates)))

(defun ellsp--handle-textDocument/completion (id method params)
  "Handle text completions.")

(provide 'ellsp-completion)
;;; ellsp-completion.el ends here
