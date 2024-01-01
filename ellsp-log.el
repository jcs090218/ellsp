;;; ellsp-log.el --- Logger module  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh

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
;; Logger module.
;;

;;; Code:

(require 'log4e)

(with-no-warnings
  (log4e:deflogger "ellsp" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                     (error . "error")
                                                     (warn  . "warn")
                                                     (info  . "info")
                                                     (debug . "debug")
                                                     (trace . "trace"))))

(ellsp--log-enable-debugging)
(ellsp--log-enable-messaging)
(ellsp--log-enable-logging)

(defun ellsp-output (&rest _)
  "Output log file."
  (let ((log-file "./.log/ellsp.log"))
    (ignore-errors (make-directory (file-name-directory log-file) t))
    (with-temp-buffer
      (ellsp--log-open-log)
      (write-region (point-min) (point-max) log-file))))

(advice-add #'ellsp--fatal :after #'ellsp-output)
(advice-add #'ellsp--error :after #'ellsp-output)
(advice-add #'ellsp--warn :after #'ellsp-output)
(advice-add #'ellsp--info :after #'ellsp-output)
(advice-add #'ellsp--debug :after #'ellsp-output)
(advice-add #'ellsp--trace :after #'ellsp-output)

(provide 'ellsp-log)
;;; ellsp-log.el ends here
