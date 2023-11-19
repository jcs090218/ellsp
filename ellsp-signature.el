;;; ellsp-signature.el --- Signature Handler  -*- lexical-binding: t; -*-

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
;; Signature Handler.
;;

;;; Code:

(defun ellsp--signatures ()
  "Return a list of signature items."
  (let* ((prefix (company-capf 'prefix))
         (candidates (company-capf 'candidates prefix)))
    (mapcar (lambda (candidate)
              (lsp-make-signature-information
               :documentation ""
               :parameters? ""))
            candidates)))

(defun ellsp--handle-textDocument/signatureHelp (id parmas)
  "Handle method `textDocument/signatureHelp'."
  (-let* (((&SignatureHelpParams :text-document (&TextDocumentIdentifier :uri)
                                 :position (&Position :line :character))
           params)
          (file (lsp--uri-to-path uri))
          (buffer (ellsp-get-buffer ellsp-workspace file)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line line)
        (forward-char character)
        (lsp--make-response
         id
         (lsp-make-signature-help
          :active-parameter? 0
          :active-signature? 0
          :signatures (apply #'vector (ellsp--signatures))))))))

(provide 'ellsp-signature)
;;; ellsp-signature.el ends here
