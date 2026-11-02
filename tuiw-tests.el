;;; tuiw-tests.el --- Test definitions for tuiw  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/tuiw.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test definitions for `tuiw'.


;;; Code:

(require 'cort)
(require 'tuiw)

(defun tuiw-tests--run (output status fn)
  "Call FN with a fake tuiw that prints OUTPUT and exits with STATUS.
Return (RESULT . ARGS) where ARGS are the arguments tuiw received."
  (let* ((log (make-temp-file "tuiw-tests-args"))
         (out (make-temp-file "tuiw-tests-output" nil nil output))
         (script (make-temp-file "tuiw-tests-bin" nil nil
                                 (format "#!/bin/sh\nprintf '%%s\\n' \"$@\" > %s\ncat %s\nexit %d\n"
                                         log out status))))
    (unwind-protect
        (progn
          (set-file-modes script #o700)
          (let* ((tuiw-executable script)
                 (result (condition-case err (funcall fn) (error (list :error (cadr err))))))
            (cons result
                  (with-temp-buffer
                    (insert-file-contents log)
                    (split-string (buffer-string) "\n" t)))))
      (delete-file log)
      (delete-file out)
      (delete-file script))))

(cort-deftest tuiw/create
  '((:equal
     (tuiw-tests--run "{\"SessionId\": \"2a638ef5\"}\n" 0
                      (lambda () (tuiw--create "vim" "/tmp")))
     '("2a638ef5" "session" "create-session" "--command" "vim" "--cwd" "/tmp"))))

(cort-deftest tuiw/list
  '((:equal
     (tuiw-tests--run "{\"Sessions\": [{\"Command\": \"bash\", \"Cwd\": \"/tmp\", \"SessionId\": \"2a638ef5\"}]}\n" 0
                      (lambda () (tuiw--list)))
     '((("2a638ef5" "bash" "/tmp")) "session" "list-sessions"))
    (:equal
     (car (tuiw-tests--run "{\"Sessions\": []}\n" 0 (lambda () (tuiw--list))))
     nil)))

(cort-deftest tuiw/send
  '((:equal
     (cdr (tuiw-tests--run "" 0 (lambda () (tuiw--send "2a638ef5" "echo hi" t))))
     '("session" "send-keys" "--session-id" "2a638ef5" "--keys" "echo hi" "--no-newline"))))

(cort-deftest tuiw/view
  '((:equal
     (tuiw-tests--run "{\"Screen\": \"$ ls\\n\", \"SessionId\": \"2a638ef5\"}\n" 0
                      (lambda () (tuiw--view "2a638ef5" t)))
     '("$ ls\n" "session" "get-screen" "--session-id" "2a638ef5" "--no-color"))))

(cort-deftest tuiw/close-error
  '((:equal
     (tuiw-tests--run "session not found: 2a638ef5\n" 254
                      (lambda () (tuiw--close "2a638ef5")))
     '((:error "Tuiw session delete-session --session-id 2a638ef5 failed (254): session not found: 2a638ef5")
      "session" "delete-session" "--session-id" "2a638ef5"))))


;; (provide 'tuiw-tests)

;;; tuiw-tests.el ends here
