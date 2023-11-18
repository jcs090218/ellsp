;;; ellsp.el --- Elisp Language Server  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/ellsp
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (lsp-mode "6.0.1") (log4e "0.1.0"))
;; Keywords: convenience lsp

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
;; Elisp Language Server
;;

;;; Code:

(require 'rx)
(require 'lsp-mode)

(require 'ellsp-log)
(require 'ellsp-tdsync)
(require 'ellsp-completion)
(require 'ellsp-hover)

(defgroup ellsp nil
  "Elisp Language Server."
  :prefix "ellsp-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ellsp"))

(defvar ellsp--initialized-p nil
  "Non-nil when it initialize successfully.")

(defun ellsp-send-response (msg)
  "Send response MSG."
  (when (or (hash-table-p msg)
            (and (listp msg) (plist-get msg :jsonrpc)))
    (setq msg (lsp--json-serialize msg)))
  (setq msg (format "Content-Length: %d\n\n%s" (string-bytes msg) msg))
  (princ msg)
  (terpri)
  msg)

(defun ellsp--uri-to-file (uri)
  "Convert URI to file."
  (substring uri 7))

(defun ellsp-form-to-lsp-range (form)
  "Convert FORM to LSP range."
  (lsp-make-range
   :start (lsp-make-position
           :line (1- (oref form line))
           :character (oref form column))
   :end (lsp-make-position
         :line (1- (oref form end-line))
         :character  (oref form end-column))))

(defun ellsp--initialize (id params)
  "Initialize the language server."
  (lsp--make-response
   id
   (lsp-make-initialize-result
    :server-info (lsp-make-server-info
                  :name "ellsp"
                  :version? "0.1.0")
    :capabilities (lsp-make-server-capabilities
                   :hover-provider? t
                   :text-document-sync? (lsp-make-text-document-sync-options
                                         :open-close? t
                                         :save? t
                                         :change 1)
                   :completion-provider? (lsp-make-completion-options
                                          :resolve-provider? json-false
                                          :trigger-characters? [":" "-"])))))

(defun ellsp--initialized ()
  "After server initialization."
  (setq ellsp--initialized-p t)
  nil)

(defun ellsp--on-request (id method params)
  "On request callback."
  (message "method: %s" method)
  (let ((res
         (pcase method
           ("initialize"              (ellsp--initialize id params))
           ("initialized"             (ellsp--initialized))
           ("textDocument/hover"      (ellsp--handle-textDocument/hover id method params))
           ("textDocument/completion" (ellsp--handle-textDocument/completion id method params))
           ("textDocument/didOpen"    (ellsp--handle-textDocument/didOpen params))
           ("textDocument/didSave"    (ellsp--handle-textDocument/didSave))
           ("textDocument/didChange"  (ellsp--handle-textDocument/didChange id method params)))))
    (if (not res)
        (message "<< %s" "no response")
      (message "<< %s" (lsp--json-serialize res))
      (ellsp-send-response (lsp--json-serialize res)))))

(defun ellsp--get-content-length (input)
  "Return the content length from INPUT."
  (string-to-number (nth 1 (split-string input ": "))))

(defun ellsp--check-content-type (input length)
  "Return non-nil when INPUT match content's LENGTH."
  (and length
       (= (length input) length)))

(defun ellsp-stdin-loop ()
  "Reads from standard input in a loop and process incoming requests."
  ;;(ellsp--info "Starting the language server...")
  (let ((input)
        (has-header)
        (content-length))
    (while (progn (setq input (read-from-minibuffer "")) input)
      ;;(ellsp--info input)
      (cond
       ((string= "some" input)
        (ellsp-send-response "Hi, there!"))
       ((string-empty-p input) )
       ((and (null content-length)
             (string-match-p (rx "content-length: " (group (1+ digit))) input))
        (setq content-length (ellsp--get-content-length input)))
       ((ellsp--check-content-type input content-length)
        (-let* (((&JSONResponse :params :method :id) (lsp--read-json input)))
          (condition-case err
              (ellsp--on-request id method params)
            (error (ellsp--error "Ellsp error: %s"
                                 (error-message-string err)))))
        (setq content-length nil))))))

(provide 'ellsp)
;;; ellsp.el ends here
