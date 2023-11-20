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

(require 'cl-lib)
(require 'eldoc)

(defvar ellsp-signature--full-string nil
  "Record current label.")

(defvar ellsp-signature--function-name nil
  "Record current function name.")

(defvar ellsp-signature--parameters nil
  "Record current parameters name.")

(defvar ellsp-signature--active-parameter 0
  "Record current parameters index.")

(defun ellsp-chop (string separator)
  "Split a STRING without consuming the SEPARATOR."
  (cl-loop with seplen = (length separator)
           with len = (length string)
           with start = 0
           with next = seplen
           for end = (or (cl-search separator string :start2 next) len)
           for chunk = (substring string start end)
           collect chunk
           while (< end len)
           do (setf start end next (+ seplen end))))

(defun ellsp-signature--display (docs _interactive)
  "Function used to get all the signature information."
  (setq ellsp-signature--parameters nil)  ; Reset
  (let* ((data (car docs))
         (params (car data))     ; string
         (metadata (cdr data)))  ; plist
    (setq ellsp-signature--function-name (plist-get metadata :thing))
    (let* ((params (s-replace "(" "" params))
           (params (s-replace ")" "" params))
           (params (ellsp-chop params "&"))
           (count 0))
      (dolist (param params)
        (setq param (string-trim param))
        (push param ellsp-signature--parameters)
        (when (equal 'eldoc-highlight-function-argument
                     (get-text-property (1- (length param)) 'face param))
          (setq ellsp-signature--active-parameter count))
        (cl-incf count)))
    (setq ellsp-signature--parameters (reverse ellsp-signature--parameters))))

(defun ellsp-signature--initialized ()
  "Initialized event."
  (global-eldoc-mode 1)
  (setq eldoc-display-functions (append eldoc-display-functions
                                        '( ellsp-signature--display))))

(add-hook 'ellsp-initialized-hook #'ellsp-signature--initialized)

(defun ellsp-signature--parameter-items ()
  "Return a list of parameter information."
  (mapcar (lambda (param)
            (lsp-make-parameter-information :label param))
          ellsp-signature--parameters))

(defun ellsp-signature--signature-items ()
  "Return a list of signature items."
  ;; XXX: No function overloard. There is function overload but the signature
  ;; is the same; therefore, there are no differences in this case.
  (list (lsp-make-signature-information
         :label ellsp-signature--full-string
         :documentation? (documentation ellsp-signature--function-name)
         :parameters? (apply #'vector (ellsp-signature--parameter-items)))))

(defun ellsp--handle-textDocument/signatureHelp (id params)
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
        (cond ((eldoc-print-current-symbol-info)
               (message "?")
               (setq ellsp-signature--full-string
                     (with-current-buffer eldoc--doc-buffer (buffer-string)))
               (lsp--make-response
                id
                (lsp-make-signature-help
                 :active-signature? 0
                 :signatures (apply #'vector (ellsp-signature--signature-items))
                 :active-parameter? ellsp-signature--active-parameter)))
              (t
               (lsp--make-response id (lsp-make-signature-help :signatures nil))))))))

(provide 'ellsp-signature)
;;; ellsp-signature.el ends here
