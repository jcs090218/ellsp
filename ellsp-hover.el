;;; ellsp-hover.el --- Hover Handler  -*- lexical-binding: t; -*-

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
;; Hover Handler.
;;

;;; Code:

(defun ellsp--describe-string ()
  "Return the describe symbol string."
  (let ((thing (symbol-at-point)))
    (with-current-buffer (help-buffer)
      (let (buffer-read-only) (erase-buffer))
      (describe-symbol thing)
      (buffer-string))))

(defun ellsp--describe-at-point ()
  "Describe symbol at point."
  (when-let (((derived-mode-p 'lisp-data-mode))
             (desc (ellsp--describe-string)))
    (if (or (string-empty-p desc)
            (string= (string-trim desc) "[back]"))
        nil
      desc)))

(defun ellsp--handle-textDocument/hover (id params)
  "Handle method `textDocument/hover'."
  (-let* (((&HoverParams :text-document (&TextDocumentIdentifier :uri)
                         :position (&Position :line :character))
           params)
          (file (lsp--uri-to-path uri))
          (buffer (ellsp-get-buffer ellsp-workspace file)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line line)
        (forward-char character)
        (when-let ((desc (ellsp--describe-at-point))
                   (line (line-number-at-pos nil t))
                   (column-beg (save-excursion (forward-symbol -1) (current-column)))
                   (column-end (save-excursion (forward-symbol 1) (current-column))))
          (lsp--make-response
           id
           (lsp-make-hover
            :contents (lsp-make-markup-content
                       :kind "plaintext"
                       :value desc)
            :range (lsp-make-range
                    :start (lsp-make-position
                            :line line
                            :character column-beg)
                    :end (lsp-make-position
                          :line line
                          :character column-end)))))))))

(provide 'ellsp-hover)
;;; ellsp-hover.el ends here
