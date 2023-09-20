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


;;;; Data structure helpers

(ert-deftest test-kf-lib-assoc-value ()
  (let ((alist '((1 . 1) (2 . 4) (3 . 9))))
    (should (eq (kf-lib-assoc-value 1 alist) 1))
    (should (eq (kf-lib-assoc-value 2 alist) 4))
    (should (eq (kf-lib-assoc-value 3 alist) 9))
    (should (eq (kf-lib-assoc-value 4 alist) nil))))

(ert-deftest test-kf-lib-assoc-value-with-testfn ()
  (let ((alist '((1 . 1) (2 . 4) (3 . 9))))
    (should (eq (kf-lib-assoc-value 1 alist #'>) 4))))

(ert-deftest test-kf-lib-set-alist-value ()
  (let ((alist '((1 . 1) (2 . 4) (3 . 9))))
    (kf-lib-set-alist-value 1 2 alist)
    (should (equal alist '((1 . 2) (2 . 4) (3 . 9))))
    (kf-lib-set-alist-value 4 16 alist)
    (should (equal alist '((4 . 16) (1 . 2) (2 . 4) (3 . 9))))))


;;;; Encrypted Secrets
(let ((script-header "#!/bin/sh\n")
      (verify-script-argument
       "if [ \"$1\" != \"/my/secrets/secrets.json\" ]; then\n  echo \"wrong argument '$1'\"\n  exit 1\nfi\n")
      (print-secret-json "echo \"{\\\"TEST_SECRET\\\": \\\"test secret\\\"}\""))
  (cl-flet ((expect-passphrase (passphrase)
              (concat "printf \"Enter passphrase: \"\nread -s passphrase\n"
                      "if [ \"$passphrase\" != \"" passphrase "\" ]; then\n"
                      "  echo \"wrong passphrase '$passphrase'\"\n"
                      "  exit 1\n"
                      "fi\n"))
            (prepare-decrypt-script-file (content)
              (let ((script-file (make-temp-file "test-decrypter" nil ".sh" content)))
                (set-file-modes script-file #o755)
                script-file)))
    (cl-macrolet ((with-decrypt-script-content (content &rest body)
                    `(let ((kf-lib-secrets-directory "/my/secrets/")
                           (kf-lib-decrypt-secrets-command (prepare-decrypt-script-file
                                                            (concat script-header
                                                                    ,content))))
                       ,@body))
                  (with-minibuffer-input (input &rest body)
                    `(minibuffer-with-setup-hook
                         (lambda ()
                           (insert ,input)
                           (run-with-timer 0 nil
                                           (lambda ()
                                             (execute-kbd-macro (kbd "RET")))))
                       ,@body)))

      (ert-deftest test-kf-lib-get-secret-no-passphrase ()
        (with-decrypt-script-content (concat verify-script-argument
                                             print-secret-json)
                                     (let ((output (kf-lib-get-secret 'TEST_SECRET)))
                                       (should (equal output "test secret")))))

      (ert-deftest test-kf-lib-get-secret-with-passphrase-success ()
        (with-decrypt-script-content (concat verify-script-argument
                                             (expect-passphrase "passphrase")
                                             print-secret-json)
                                     (with-minibuffer-input "passphrase"
                                       (kf-lib-get-secret 'TEST_SECRET)))))))
