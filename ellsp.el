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

(require 'lsp-mode)

(require 'ellsp-log)

(defgroup ellsp nil
  "Elisp Language Server."
  :prefix "ellsp-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ellsp"))

(defun ellsp-send-response (message)
  (when (or (hash-table-p message)
            (and (listp message) (plist-get message :jsonrpc)))
    (setq message (lsp--json-serialize message)))

  (princ (format "Content-Length: %d\r\n\r\n" (string-bytes message)))
  (princ message)
  (terpri))

(defclass ellsp-file ()
  ((name :type string :initarg :name)
   (buffer :type buffer :initarg :buffer))
  "")

(defclass ellsp-state ()
  ((files
    :type hash-table :initform (make-hash-table :test #'equal)
    :documentation "Workspace files.")
   (dependencies
    :type list :initform nil
    :documentation "Loaded dependencies.

We use this list to remove dependencies which don't need to
be re-analysed during textDocument/didOpen handler.")))

(defvar ellsp-state (ellsp-state)
  "")

(cl-defmethod ellsp-add-file ((state ellsp-state) (file string))
  ""
  (message "Added file %s to state" file)
  (puthash file
           (ellsp-file
            :name file
            :buffer (with-current-buffer
                        (get-buffer-create (concat "ellsp-" file))
                      (emacs-lisp-mode)
                      (current-buffer)))
           (oref state files))
  (ellsp-update-file-buffer state file))

(cl-defmethod ellsp-get-file ((state ellsp-state) (file string))
  ""
  (gethash file (oref state files)))

(cl-defmethod ellsp-get-buffer ((state ellsp-state) (file string))
  ""
  (when-let ((file (ellsp-get-file state file)))
    (oref file buffer)))

(cl-defmethod ellsp-update-file-buffer ((state ellsp-state) (file string) &optional content)
  ""
  (message "Trying to update file %s" file)
  (when-let ((buffer (ellsp-get-buffer state file)))
    (with-current-buffer buffer
      (erase-buffer)
      (if (and content (stringp content))
          (insert content)
        (insert-file-contents file)))
    (message "Updated file %s" file)))

(cl-defmethod ellsp--analyze-file ((this ellsp-state) (file string))
  ""
  (let ((state (elsa-analyse-file-parallel
                file
                elsa-global-state
                (oref this dependencies))))
    (oset this dependencies
          (-uniq
           (-concat
            (oref this dependencies)
            (oref state dependencies))))
    state))

(defun ellsp--uri-to-file (uri)
  ""
  (substring uri 7))

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

(defun ellsp--get-diagnostics (file)
  ""
  (let ((state (progn
                 (oset elsa-global-state number-of-files 1)
                 (oset elsa-global-state processed-file-index 1)
                 (elsa-process-file file elsa-global-state))))
    (elsa-state-update-global state elsa-global-state)
    (apply #'vector (mapcar #'elsa-message-to-lsp (oref state errors)))))

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
  ""
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

(defun ellsp--analyze-textDocument/hover (form _state _method params)
  "")

(defun ellsp--handle-textDocument/hover (id method params)
  "")

(defun ellsp--analyze-textDocument/completion (form state method params)
  "")

(defun ellsp--handle-textDocument/completion (id method params)
  "")

(defun ellsp--handle-textDocument/didOpen (params)
  ""
  (-let* (((&DidOpenTextDocumentParams :text-document (&TextDocumentItem :uri :version)) params)
          (file (ellsp--uri-to-file uri)))
    (ellsp-add-file ellsp-state file)
    (let ((state (ellsp--analyze-file ellsp-state file)))
      (lsp--make-notification
       "textDocument/publishDiagnostics"
       (lsp-make-publish-diagnostics-params
        :uri uri
        :version version
        :diagnostics (apply
                      #'vector
                      (mapcar #'elsa-message-to-lsp
                              (oref state errors))))))))

(defun ellsp--handle-textDocument/didSave ()
  ""
  (-let* (((&DidSaveTextDocumentParams :text-document (&TextDocumentItem :uri :version)) params)
          (file (ellsp--uri-to-file uri)))
    (ellsp-update-file-buffer ellsp-state file)
    (when (string-match-p "/elsa-" file)
      (ellsp--debug "Reloading file %s" file)
      (load file t))
    (lsp--make-notification
     "textDocument/publishDiagnostics"
     (lsp-make-publish-diagnostics-params
      :uri uri
      :version version
      :diagnostics (elsa-lsp--get-diagnostics file)))))

(defun ellsp--handle-textDocument/didChange (id method params)
  "")

(defun ellsp--on-request (id method params)
  (ellsp--info "? %s %s %s" id method params)
  (ellsp--trace ">> %s" (lsp--json-serialize (list :id id :method method :params params)))
  (let ((res
         (pcase method
           ("initialize"              (ellsp--initialize id params))
           ("textDocument/hover"      (ellsp--handle-textDocument/hover id method params))
           ("textDocument/completion" (ellsp--handle-textDocument/completion id method params))
           ("textDocument/didOpen"    (ellsp--handle-textDocument/didOpen params))
           ("textDocument/didSave"    (ellsp--handle-textDocument/didSave))
           ("textDocument/didChange"  (elsa-lsp--handle-textDocument/didChange id method params)))))
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

(defun ellsp-stdin ()
  "Return standard input."
  (condition-case nil
      (with-temp-buffer
        (insert (read-from-minibuffer ""))
        (buffer-string))
    (error)))

(defun ellsp-stdin-loop ()
  "Reads from standard input in a loop and process incoming requests."
  (let ((input)
        (has-header)
        (content-length))
    (while (progn (setq input (ellsp-stdin)) input)
      (message "input: %s" input)
      (ellsp--info input)
      (cond
       ((string-empty-p input) )
       ((and (null content-length)
             (string-match-p (rx "content-length: " (group (1+ digit))) input))
        (setq content-length (ellsp--get-content-length input)))
       ((ellsp--check-content-type input content-length)
        (-let* (((&JSONResponse :params :method :id) (lsp--read-json input)))
          (condition-case err
              (ellsp--on-request id method params)
            (error (ellsp--log-error "Ellsp error: %s"
                                     (error-message-string err)))))
        (setq content-length nil))))))

(provide 'ellsp)
;;; ellsp.el ends here
