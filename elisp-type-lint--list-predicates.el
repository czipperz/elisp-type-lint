;;; elisp-type-lint--list-predicates.el --- Basic list predicates

;; Copyright (C) 2016 Chris Gregory czipperz@gmail.com

;; This file is part of Elisp Type Lint.
;;
;; Elisp Type Lint is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Elisp Type Lint is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Elisp Type Lint.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Chris Gregory "czipperz"
;; Email: czipperz@gmail.com
;; Version: 0.0.1

;;; Commentary:
;;; Code:

(defun elisp-type-lint--all (pred list)
  "Return t if (PRED e) is non-nil for each e in LIST, otherwise nil."
  (catch 'done
    (while (not (null list))
      (unless (funcall pred (car list))
        (throw 'done nil))
      (setq list (cdr list)))
    t))

(defun elisp-type-lint--any (pred list)
  "Return t if (PRED e) is non-nil for any e in LIST, otherwise nil."
  (catch 'done
    (while (not (null list))
      (when (funcall pred (car list))
        (throw 'done t))
      (setq list (cdr list)))
    nil))

(provide 'elisp-type-lint--list-predicates)
;;; elisp-type-lint--list-predicates.el ends here
