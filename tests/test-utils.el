;;; test-utils.el --- Test utilities

;; Copyright (C) 2016 Chris Gregory czipperz@gmail.com

;; This file is part of Elisp Type Lint.
;;
;; Elisp Type Lintis free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Elisp Type Lintis distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Elisp Type Lint.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:
(require 'ert)

(defmacro deftest (test-name file-contents &rest body)
  "Define a test TEST-NAME that inserts FILE-CONTENTS then runs BODY.

Goes to bob before running BODY."
  (declare (indent 2))
  `(ert-deftest
       ,test-name ()
       (with-temp-buffer
         (insert ,file-contents)
         (goto-char (point-min))
         ,@body)))

(provide 'test-utils)
;;; test-utils.el ends here
