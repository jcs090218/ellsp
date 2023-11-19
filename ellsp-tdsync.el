;;; ellsp-tdsync.el --- Text document sync  -*- lexical-binding: t; -*-

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
;; Text document sync.
;;

;;; Code:

(defclass ellsp-file ()
  ((name :type string :initarg :name)
   (buffer :type buffer :initarg :buffer))
  "Tracked buffer file.")

(defclass ellsp-state ()
  ((files
    :type hash-table :initform (make-hash-table :test #'equal)
    :documentation "Workspace files.")
   (dependencies
    :type list :initform nil
    :documentation "Loaded dependencies.

We use this list to remove dependencies which don't need to
be re-analysed during textDocument/didOpen handler.")))

(defvar ellsp-workspace (ellsp-state)
  "Workspace state.")

(cl-defmethod ellsp-get-file ((state ellsp-state) (file string))
  "Return the file if found."
  (gethash file (oref state files)))

(cl-defmethod ellsp-get-buffer ((state ellsp-state) (file string))
  "Get the buffer from workspace state."
  (when-let ((file (ellsp-get-file state file)))
    (oref file buffer)))

(cl-defmethod ellsp-update-file-buffer ((state ellsp-state) (file string) &optional content)
  "Sync the text document buffer."
  (message "Trying to update file %s" file)
  (when-let ((buffer (ellsp-get-buffer state file)))
    (with-current-buffer buffer
      (erase-buffer)
      (if (and content (stringp content))
          (insert content)
        (insert-file-contents file)))
    (message "Updated file %s" file)))

(cl-defmethod ellsp-add-file ((state ellsp-state) (file string))
  "Add a file to workspace."
  (message "Added file %s to state" file)
  (puthash file
           (ellsp-file
            :name file
            :buffer (with-current-buffer
                        (get-buffer-create (format "*ellsp:%s*" file))
                      (emacs-lisp-mode)
                      (current-buffer)))
           (oref state files))
  (ellsp-update-file-buffer state file))

(defun ellsp--handle-textDocument/didOpen (params)
  "On method `textDocument/didOpen'."
  (-let* (((&DidOpenTextDocumentParams :text-document (&TextDocumentItem :uri :version)) params)
          (file (lsp--uri-to-path uri)))
    (ellsp-add-file ellsp-workspace file)
    nil))

(defun ellsp--handle-textDocument/didSave (params)
  "On method `textDocument/didSave'."
  (-let* (((&DidSaveTextDocumentParams :text-document (&TextDocumentItem :uri :version)) params)
          (file (lsp--uri-to-path uri)))
    (ellsp-update-file-buffer ellsp-workspace file)
    nil))

(defun ellsp--handle-textDocument/didChange (params)
  "On method `textDocument/didChange'."
  (-let* (((&DidChangeTextDocumentParams
            :text-document (&VersionedTextDocumentIdentifier :uri :version?)
            :content-changes [(&TextDocumentContentChangeEvent :text)])
           params)
          (file (lsp--uri-to-path uri))
          (buffer (ellsp-get-buffer ellsp-workspace file)))
    (ellsp-update-file-buffer ellsp-workspace file text)
    nil))

(provide 'ellsp-tdsync)
;;; ellsp-tdsync.el ends here
