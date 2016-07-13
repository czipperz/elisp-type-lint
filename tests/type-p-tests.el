;;; type-p-tests.el --- Test functions in elisp-type-lint--type-predicates.el

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
(require 'ert)
(require 'elisp-type-lint)

(ert-deftest elisp-type-lint-type-int-p--type-p-1 ()
  (should (elisp-type-lint-type-int-p 'int))
  (should-not (elisp-type-lint-type-int-p 'float))
  (should-not (elisp-type-lint-type-int-p 'string))
  (should-not (elisp-type-lint-type-int-p 'null))
  (should-not (elisp-type-lint-type-int-p 't))
  (should-not (elisp-type-lint-type-int-p '(cons int float)))
  (should-not (elisp-type-lint-type-int-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-int-p '(fun int float float)))
  (should-not (elisp-type-lint-type-int-p '(or int float null)))
  (should-not (elisp-type-lint-type-int-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-int-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-float-p--type-p-1 ()
  (should-not (elisp-type-lint-type-float-p 'int))
  (should (elisp-type-lint-type-float-p 'float))
  (should-not (elisp-type-lint-type-float-p 'string))
  (should-not (elisp-type-lint-type-float-p 'null))
  (should-not (elisp-type-lint-type-float-p 't))
  (should-not (elisp-type-lint-type-float-p '(cons int float)))
  (should-not (elisp-type-lint-type-float-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-float-p '(fun int float float)))
  (should-not (elisp-type-lint-type-float-p '(or int float null)))
  (should-not (elisp-type-lint-type-float-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-float-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-string-p--type-p-1 ()
  (should-not (elisp-type-lint-type-string-p 'int))
  (should-not (elisp-type-lint-type-string-p 'float))
  (should (elisp-type-lint-type-string-p 'string))
  (should-not (elisp-type-lint-type-string-p 'null))
  (should-not (elisp-type-lint-type-string-p 't))
  (should-not (elisp-type-lint-type-string-p '(cons int float)))
  (should-not (elisp-type-lint-type-string-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-string-p '(fun int float float)))
  (should-not (elisp-type-lint-type-string-p '(or int float null)))
  (should-not (elisp-type-lint-type-string-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-string-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-null-p--type-p-1 ()
  (should-not (elisp-type-lint-type-null-p 'int))
  (should-not (elisp-type-lint-type-null-p 'float))
  (should-not (elisp-type-lint-type-null-p 'string))
  (should (elisp-type-lint-type-null-p 'null))
  (should-not (elisp-type-lint-type-null-p 't))
  (should-not (elisp-type-lint-type-null-p '(cons int float)))
  (should-not (elisp-type-lint-type-null-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-null-p '(fun int float float)))
  (should-not (elisp-type-lint-type-null-p '(or int float null)))
  (should-not (elisp-type-lint-type-null-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-null-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-t-p--type-p-1 ()
  (should-not (elisp-type-lint-type-t-p 'int))
  (should-not (elisp-type-lint-type-t-p 'float))
  (should-not (elisp-type-lint-type-t-p 'string))
  (should-not (elisp-type-lint-type-t-p 'null))
  (should (elisp-type-lint-type-t-p 't))
  (should-not (elisp-type-lint-type-t-p '(cons int float)))
  (should-not (elisp-type-lint-type-t-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-t-p '(fun int float float)))
  (should-not (elisp-type-lint-type-t-p '(or int float null)))
  (should-not (elisp-type-lint-type-t-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-t-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-cons-p--type-p-1 ()
  (should-not (elisp-type-lint-type-cons-p 'int))
  (should-not (elisp-type-lint-type-cons-p 'float))
  (should-not (elisp-type-lint-type-cons-p 'string))
  (should-not (elisp-type-lint-type-cons-p 'null))
  (should-not (elisp-type-lint-type-cons-p 't))
  (should (elisp-type-lint-type-cons-p '(cons int float)))
  (should-not (elisp-type-lint-type-cons-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-cons-p '(fun int float float)))
  (should-not (elisp-type-lint-type-cons-p '(or int float null)))
  (should-not (elisp-type-lint-type-cons-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-cons-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-quoted-p--type-p-1 ()
  (should-not (elisp-type-lint-type-quoted-p 'int))
  (should-not (elisp-type-lint-type-quoted-p 'float))
  (should-not (elisp-type-lint-type-quoted-p 'string))
  (should-not (elisp-type-lint-type-quoted-p 'null))
  (should-not (elisp-type-lint-type-quoted-p 't))
  (should-not (elisp-type-lint-type-quoted-p '(cons int float)))
  (should (elisp-type-lint-type-quoted-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-quoted-p '(fun int float float)))
  (should-not (elisp-type-lint-type-quoted-p '(or int float null)))
  (should-not (elisp-type-lint-type-quoted-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-quoted-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-fun-p--type-p-1 ()
  (should-not (elisp-type-lint-type-fun-p 'int))
  (should-not (elisp-type-lint-type-fun-p 'float))
  (should-not (elisp-type-lint-type-fun-p 'string))
  (should-not (elisp-type-lint-type-fun-p 'null))
  (should-not (elisp-type-lint-type-fun-p 't))
  (should-not (elisp-type-lint-type-fun-p '(cons int float)))
  (should-not (elisp-type-lint-type-fun-p '(quoted (cons int float))))
  (should (elisp-type-lint-type-fun-p '(fun int float float)))
  (should-not (elisp-type-lint-type-fun-p '(or int float null)))
  (should-not (elisp-type-lint-type-fun-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-fun-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-or-p--type-p-1 ()
  (should-not (elisp-type-lint-type-or-p 'int))
  (should-not (elisp-type-lint-type-or-p 'float))
  (should-not (elisp-type-lint-type-or-p 'string))
  (should-not (elisp-type-lint-type-or-p 'null))
  (should-not (elisp-type-lint-type-or-p 't))
  (should-not (elisp-type-lint-type-or-p '(cons int float)))
  (should-not (elisp-type-lint-type-or-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-or-p '(fun int float float)))
  (should (elisp-type-lint-type-or-p '(or int float null)))
  (should-not (elisp-type-lint-type-or-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-or-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-let-p--type-p-1 ()
  (should-not (elisp-type-lint-type-let-p 'int))
  (should-not (elisp-type-lint-type-let-p 'float))
  (should-not (elisp-type-lint-type-let-p 'string))
  (should-not (elisp-type-lint-type-let-p 'null))
  (should-not (elisp-type-lint-type-let-p 't))
  (should-not (elisp-type-lint-type-let-p '(cons int float)))
  (should-not (elisp-type-lint-type-let-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-let-p '(fun int float float)))
  (should-not (elisp-type-lint-type-let-p '(or int float null)))
  (should (elisp-type-lint-type-let-p '(let id (a) a)))
  (should-not (elisp-type-lint-type-let-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-impl-p--type-p-1 ()
  (should-not (elisp-type-lint-type-impl-p 'int))
  (should-not (elisp-type-lint-type-impl-p 'float))
  (should-not (elisp-type-lint-type-impl-p 'string))
  (should-not (elisp-type-lint-type-impl-p 'null))
  (should-not (elisp-type-lint-type-impl-p 't))
  (should-not (elisp-type-lint-type-impl-p '(cons int float)))
  (should-not (elisp-type-lint-type-impl-p '(quoted (cons int float))))
  (should-not (elisp-type-lint-type-impl-p '(fun int float float)))
  (should-not (elisp-type-lint-type-impl-p '(or int float null)))
  (should-not (elisp-type-lint-type-impl-p '(let id (a) a)))
  (should (elisp-type-lint-type-impl-p '(impl id (a)))))

(ert-deftest elisp-type-lint-type-p--type-p-1 ()
  (should (elisp-type-lint-type-p 'int))
  (should (elisp-type-lint-type-p 'float))
  (should (elisp-type-lint-type-p 'string))
  (should (elisp-type-lint-type-p 'null))
  (should (elisp-type-lint-type-p 't))
  (should (elisp-type-lint-type-p '(cons int float)))
  (should (elisp-type-lint-type-p '(quoted (cons int float))))
  (should (elisp-type-lint-type-p '(fun int float float)))
  (should (elisp-type-lint-type-p '(or int float null))))

(ert-deftest elisp-type-lint-type-p--type-p-2 ()
  (should (elisp-type-lint-type-p 'int))
  (should-not (elisp-type-lint-type-p '(cons)))
  (should-not (elisp-type-lint-type-p '(cons int)))
  (should (elisp-type-lint-type-p '(cons int int)))
  (should-not (elisp-type-lint-type-p '(cons int int int))))

(provide 'type-p-tests)
;;; type-p-tests.el ends here
