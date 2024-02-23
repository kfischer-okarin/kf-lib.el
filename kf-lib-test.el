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


(defvar kf-lib-test-result nil)

(defmacro kf-lib-test-set-result (result)
  `(lambda ()
     (interactive)
     (setq kf-lib-test-result ,result)))


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


;;;; Emacs Automation

(ert-deftest test-kf-lib-with-minibuffer-input ()
  (kf-lib-with-minibuffer-input "test"
                                (should (equal (read-string "test") "test"))))


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
                                                                    verify-script-argument
                                                                    ,content
                                                                    print-secret-json))))
                       ,@body)))

      (ert-deftest test-kf-lib-get-secret-no-passphrase ()
        (with-decrypt-script-content nil
                                     (let ((result (kf-lib-get-secret 'TEST_SECRET)))
                                       (should (equal result "test secret")))))

      (ert-deftest test-kf-lib-get-secret-with-passphrase-success ()
        (with-decrypt-script-content (expect-passphrase "passphrase")
                                     (kf-lib-with-minibuffer-input "passphrase"
                                                                   (let ((result (kf-lib-get-secret 'TEST_SECRET)))
                                                                     (should (equal result "test secret"))))))

      (ert-deftest test-kf-lib-get-secret-with-passphrase-fail ()
        (with-decrypt-script-content (expect-passphrase "passphrase")
                                     (kf-lib-with-minibuffer-input "not the passphrase"
                                                                   (condition-case err
                                                                       (kf-lib-get-secret 'TEST_SECRET)
                                                                     ('error
                                                                      (should (equal (error-message-string err)
                                                                                     "Incorrect passphrase")))))))

      (ert-deftest test-kf-lib-get-secret-caches-secrets ()
        (let ((executed-filename "executed-decryption"))
          (with-decrypt-script-content (concat "touch " executed-filename "\n")
                                       (setq kf-lib-cached-secrets nil)
                                       (unwind-protect
                                           (progn
                                             (kf-lib-get-secret 'TEST_SECRET)
                                             (should (file-exists-p executed-filename))
                                             (delete-file executed-filename)
                                             (let ((result (kf-lib-get-secret 'TEST_SECRET)))
                                               (should (equal result "test secret"))
                                               (should (not (file-exists-p executed-filename)))))
                                         (delete-file executed-filename)))))

      (ert-deftest test-kf-lib-reload-secrets ()
        (let ((executed-filename "executed-decryption"))
          (with-decrypt-script-content (concat "touch " executed-filename "\n")
                                       (setq kf-lib-cached-secrets '(("TEST_SECRET" . "old secret")))
                                       (unwind-protect
                                           (progn
                                             (kf-lib-reload-secrets)
                                             (should (file-exists-p executed-filename))
                                             (let ((result (kf-lib-get-secret 'TEST_SECRET)))
                                               (should (equal result "test secret"))))
                                         (delete-file executed-filename))))))))


;;;; Org Mode

(ert-deftest test-kf-lib-org-go-to-drawer ()
  (with-temp-buffer
    (insert "* Headline\n"
            ":PROPERTIES:\n"
            ":END:\n"
            ":LOGBOOK:\n"
            ":END:\n"
            "\n"
            "* Headline without drawer\n"
            "Some content\n"
            "\n"
            "* Other Headline\n"
            ":PROPERTIES:\n"
            ":END:\n"
            ":LOGBOOK:\n"
            ":END:\n")
    (goto-char (point-min))
    (kf-lib-org-go-to-drawer "LOGBOOK")
    (should (eq (line-number-at-pos) 4))
    (should (eq (current-column) 0))
    (goto-char (point-max))
    (kf-lib-org-go-to-drawer "LOGBOOK")
    (should (eq (line-number-at-pos) 13))
    (should (eq (current-column) 0))
    (goto-line 10) ; Directly on headline
    (kf-lib-org-go-to-drawer "LOGBOOK")
    (should (eq (line-number-at-pos) 13))
    (should (eq (current-column) 0))
    (goto-line 8) ; Headline without drawer
    (condition-case err
        (progn
          (kf-lib-org-go-to-drawer "LOGBOOK")
          (should nil)) ; Should not be reached
      ('error
       (should (equal (error-message-string err)
                      "No drawer LOGBOOK found"))))))

(ert-deftest test-kf-lib-org-go-to-next-logbook-item ()
  (with-temp-buffer
    (insert ":LOGBOOK:\n"
            "- Note taken on [2023-09-21 Thu 17:12] \\\\\n"
            "Some long\n"
            "multiline\n"
            "note\n"
            "CLOCK: [2023-09-20 Wed 13:55]--[2023-09-20 Wed 14:22] =>  0:27\n"
            "CLOCK: [2023-09-20 Wed 10:55]--[2023-09-20 Wed 11:22] =>  0:27\n"
            "- Clock note\n"
            "- State \"TODO\"       from              [2023-09-20 Wed 10:23]\n"
            ":END:\n"
            "- non logbook item\n")
    (goto-char (point-min))
    (kf-lib-org-go-to-next-logbook-item)
    (should (eq (line-number-at-pos) 2))
    (kf-lib-org-go-to-next-logbook-item)
    (should (eq (line-number-at-pos) 6))
    (kf-lib-org-go-to-next-logbook-item)
    (should (eq (line-number-at-pos) 7))
    (kf-lib-org-go-to-next-logbook-item)
    (should (eq (line-number-at-pos) 9))
    (forward-char 10) ; To make sure the exact point is remembered before error
    (let ((point-before-error (point)))
      (condition-case err
          (progn
            (kf-lib-org-go-to-next-logbook-item)
            (should nil)) ; Should not be reached
        ('error
         (should (equal (error-message-string err)
                        "On last logbook item"))))
      (should (eq (point) point-before-error)))))

(let ((new-time (encode-time (list 30 12 9 21 11 2023))))

  (ert-deftest test-kf-lib-org-with-current-time-org-time-stamp ()
    (with-temp-buffer
      (kf-lib-org-with-current-time new-time
                                    (org-time-stamp '(16)))
      (should (equal (buffer-string) "<2023-11-21 Tue 09:12>"))))

  (ert-deftest test-kf-lib-org-with-current-time-close-todo ()
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task\n")
        (kf-lib-org-with-current-time new-time
                                      (org-todo "DONE"))
        (should (equal (buffer-string) (concat "* DONE Task\n"
                                               "CLOSED: [2023-11-21 Tue 09:12]\n"))))))

  (ert-deftest test-kf-lib-org-with-current-time-repeater ()
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Repeating Task\n"
              "  DEADLINE: <2023-11-01 Wed .+3d>\n")
      (kf-lib-org-with-current-time new-time
                                    (org-todo "DONE"))
      (should (equal (buffer-string) (concat "* TODO Repeating Task\n"
                                             "  DEADLINE: <2023-11-24 Fri .+3d>\n"
                                             "  :PROPERTIES:\n"
                                             "  :LAST_REPEAT: [2023-11-21 Tue 09:12]\n"
                                             "  :END:\n"))))))


;;;; Execute file

(let ((test-command-alist
          `(("other_project" . '())
            ("project" . (("\\.py" . ,(kf-lib-test-set-result 'python))
                          ("\\.rb" . ,(kf-lib-test-set-result 'ruby))))
            ((:type dragonruby) . (("\\.rb" . ,(kf-lib-test-set-result 'dragonruby))))
            (t . (("\\.rb" . ,(kf-lib-test-set-result 'default-ruby)))))))

  (ert-deftest test-kf-lib-execute-file-project-name ()
    (let* ((kf-lib-execute-file-command-alist test-command-alist)
           (buffer-file-name "script.rb")
           (kf-lib-project-type-function (lambda () nil))
           (kf-lib-project-name-function (lambda () "project"))
           (kf-lib-test-result nil))
      (kf-lib-execute-file)
      (should (equal kf-lib-test-result 'ruby))))

  (ert-deftest test-kf-lib-execute-file-default-commands ()
    (let* ((kf-lib-execute-file-command-alist test-command-alist)
           (buffer-file-name "script.rb")
            (kf-lib-project-type-function (lambda () nil))
           (kf-lib-project-name-function (lambda () "unknown"))
           (kf-lib-test-result nil))
      (kf-lib-execute-file)
      (should (equal kf-lib-test-result 'default-ruby))))

  (ert-deftest test-kf-lib-execute-file-no-match ()
    (let* ((kf-lib-execute-file-command-alist test-command-alist)
           (buffer-file-name "script.py")
            (kf-lib-project-type-function (lambda () nil))
           (kf-lib-project-name-function (lambda () "other_project"))
           (kf-lib-test-result nil))
      (condition-case err
          (progn
            (kf-lib-execute-file)
            (should nil)) ; Should not be reached
        ('error
         (should (string= (error-message-string err)
                          "Don’t know how to execute file ’script.py’"))))))

  (ert-deftest test-kf-lib-execute-file-project-type ()
    (let* ((kf-lib-execute-file-command-alist test-command-alist)
           (buffer-file-name "script.rb")
           (kf-lib-project-name-function (lambda () "gameproject"))
           (kf-lib-project-type-function (lambda () 'dragonruby))
           (kf-lib-test-result nil))
      (kf-lib-execute-file)
      (should (equal kf-lib-test-result 'dragonruby))))

  (ert-deftest test-kf-lib-execute-file-try-next-matching-group ()
    (let* ((kf-lib-execute-file-command-alist test-command-alist)
           (buffer-file-name "script.rb")
           (kf-lib-project-name-function (lambda () "other_project"))
           (kf-lib-project-type-function (lambda () nil))
           (kf-lib-test-result nil))
      (kf-lib-execute-file)
      (should (equal kf-lib-test-result 'default-ruby)))))


;;;; Find related file
(let ((test-command-alist
       '(("other_project" . '())
         ("project" . (("\\(.+\\)\\.py" . (lambda (filename basename) (concat filename basename "_related.py")))
                       ("\\(.+\\)\\.rb" . (lambda (filename basename) (concat filename basename "_related.rb")))))
          ((:type dragonruby) . (("\\(.+\\)\\.rb" . (lambda (filename basename)
                                                      (concat filename basename "_dr_related.rb"))))))))

  (cl-macrolet
      ((with-mocked-find-file (body)
         `(cl-letf (((symbol-function 'find-file) (lambda (file) (setq kf-lib-test-result file))))
            ,body)))

      (ert-deftest test-kf-lib-find-related-file-project-name ()
        (with-mocked-find-file
         (let* ((kf-lib-find-related-file-command-alist test-command-alist)
                (buffer-file-name "script.rb")
                (kf-lib-project-type-function (lambda () nil))
                (kf-lib-project-name-function (lambda () "project"))
                (kf-lib-test-result nil))
           (kf-lib-find-related-file)
           (should (equal kf-lib-test-result "script.rbscript_related.rb")))))

      (ert-deftest test-kf-lib-find-related-file-no-match ()
        (let* ((kf-lib-find-related-file-command-alist test-command-alist)
               (buffer-file-name "script.py")
               (kf-lib-project-type-function (lambda () nil))
               (kf-lib-project-name-function (lambda () "other_project"))
               (kf-lib-test-result nil))
          (condition-case err
              (progn
                (kf-lib-find-related-file)
                (should nil)) ; Should not be reached
            ('error
             (should (string= (error-message-string err)
                              "Don’t know how to find related file for ’script.py’"))))))

      (ert-deftest test-kf-lib-find-related-file-project-type ()
        (with-mocked-find-file
         (let* ((kf-lib-find-related-file-command-alist test-command-alist)
                (buffer-file-name "script.rb")
                (kf-lib-project-name-function (lambda () "gameproject"))
                (kf-lib-project-type-function (lambda () 'dragonruby))
                (kf-lib-test-result nil))
           (kf-lib-find-related-file)
           (should (equal kf-lib-test-result "script.rbscript_dr_related.rb")))))))
