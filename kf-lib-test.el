;;; kf-lib-test.el --- Tests for kf-lib.el                   -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Kevin Fischer

;; Author: Kevin Fischer <kfischer_okarin@yahoo.co.jp>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(ert-deftest kf-lib-test-assoc-value ()
  (let ((alist '((1 . 1) (2 . 4) (3 . 9))))
    (should (eq (kf-lib-assoc-value 1 alist) 1))
    (should (eq (kf-lib-assoc-value 2 alist) 4))
    (should (eq (kf-lib-assoc-value 3 alist) 9))
    (should (eq (kf-lib-assoc-value 4 alist) nil))))

(ert-deftest kf-lib-test-assoc-value-with-testfn ()
  (let ((alist '((1 . 1) (2 . 4) (3 . 9))))
    (should (eq (kf-lib-assoc-value 1 alist #'>) 4))))