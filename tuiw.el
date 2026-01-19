;;; tuiw.el --- Integration for tuiw  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
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

;; tuiw integration.


;;; Code:


;;; Customization

(defgroup tuiw nil
  "Integration for tuiw."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/tuiw.el"))

(defcustom tuiw-executable "tuiw"
  "Path to tuiw executable."
  :type 'string
  :group 'tuiw)

(defcustom tuiw-terminal-backend 'vterm
  "Terminal backend for attaching to tuiw sessions."
  :type '(choice (const :tag "vterm" vterm))
  :group 'tuiw)

(defcustom tuiw-view-ansi-color t
  "If non-nil, apply ANSI color codes in tuiw-view buffer."
  :type 'boolean
  :group 'tuiw)



;;; Core Functions

(defun tuiw--call (&rest args)
  "Call tuiw with ARGS and return output as string."
  (with-temp-buffer
    (apply #'call-process tuiw-executable nil t nil args)
    (buffer-string)))

(defun tuiw--create (command &optional cwd)
  "Create a new tuiw session running COMMAND.
Optional CWD specifies the working directory.
Return session ID."
  (string-trim
   (if cwd
       (tuiw--call "create" "--cwd" cwd command)
     (tuiw--call "create" command))))

(defun tuiw--send (session-id keys &optional no-newline)
  "Send KEYS to SESSION-ID.
If NO-NEWLINE is non-nil, do not append newline."
  (if no-newline
      (tuiw--call "send" "--no-newline" session-id keys)
    (tuiw--call "send" session-id keys)))

(defun tuiw--list ()
  "List all tuiw sessions.
Return list of (session-id command cwd)."
  (let ((output (string-trim (tuiw--call "list"))))
    (when (not (string-empty-p output))
      (mapcar (lambda (line) (split-string line "\t"))
              (split-string output "\n")))))

(defun tuiw--list-session-ids ()
  "List all tuiw session IDs."
  (mapcar #'car (tuiw--list)))

(defun tuiw--read-session (prompt)
  "Read a session ID with PROMPT using completion with annotations."
  (let* ((sessions (tuiw--list))
         (annotate (lambda (id)
                     (when-let ((entry (assoc id sessions)))
                       (format "  %s [%s]" (nth 1 entry) (nth 2 entry)))))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (annotation-function . ,annotate))
                    (complete-with-action action (mapcar #'car sessions) string pred)))))
    (completing-read prompt table nil t)))

(defun tuiw--view (session-id &optional no-color)
  "View output of SESSION-ID.
If NO-COLOR is non-nil, strip ANSI color codes."
  (if no-color
      (tuiw--call "view" "--no-color" session-id)
    (tuiw--call "view" session-id)))

(defun tuiw--status (session-id)
  "Get status of SESSION-ID."
  (string-trim (tuiw--call "status" session-id)))

(defun tuiw--close (session-id)
  "Close SESSION-ID."
  (tuiw--call "close" session-id))



;;; Interactive Commands

(declare-function ansi-color-apply-on-region "ansi-color")

;;;###autoload
(defun tuiw-create (command &optional cwd)
  "Run COMMAND in a new tuiw session interactively.
With prefix argument, prompt for working directory CWD."
  (interactive
   (list (read-string "Command: ")
         (when current-prefix-arg
           (read-directory-name "Directory: "))))
  (let ((session-id (tuiw--create command cwd)))
    (message "Created tuiw session: %s" session-id)
    session-id))

;;;###autoload
(defun tuiw-view (session-id)
  "Show output of SESSION-ID in a buffer."
  (interactive (list (tuiw--read-session "Session: ")))
  (let ((buf (get-buffer-create (format "*tuiw:%s*" session-id))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (tuiw--view session-id (not tuiw-view-ansi-color)))
        (when tuiw-view-ansi-color
          (require 'ansi-color)
          (ansi-color-apply-on-region (point-min) (point-max)))
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buf)))

;;;###autoload
(defun tuiw-send (session-id keys)
  "Send KEYS to SESSION-ID interactively."
  (interactive
   (list (tuiw--read-session "Session: ")
         (read-string "Keys: ")))
  (tuiw--send session-id keys)
  (message "Sent keys to %s" session-id))

;;;###autoload
(defun tuiw-close (session-id)
  "Close SESSION-ID interactively."
  (interactive (list (tuiw--read-session "Session: ")))
  (tuiw--close session-id)
  (message "Closed session: %s" session-id))


;;; Send Buffer Mode

(defvar-local tuiw-send-with-temp-buffer--session-id nil)

(defvar tuiw-send-with-temp-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'tuiw-send-with-temp-buffer-commit)
    (define-key map (kbd "C-c C-k") #'tuiw-send-with-temp-buffer-cancel)
    map))

(define-derived-mode tuiw-send-with-temp-buffer-mode text-mode "Tuiw-Send"
  "Mode for composing text to send to tuiw session.
\\<tuiw-send-with-temp-buffer-mode-map>
\\[tuiw-send-with-temp-buffer-commit] to send, \\[tuiw-send-with-temp-buffer-cancel] to cancel.")

(defun tuiw-send-with-temp-buffer-commit ()
  "Send buffer contents to the tuiw session."
  (interactive)
  (let ((content (buffer-string))
        (session-id tuiw-send-with-temp-buffer--session-id))
    (tuiw--send session-id content)
    (message "Sent to %s" session-id)
    (quit-window t)))

(defun tuiw-send-with-temp-buffer-cancel ()
  "Cancel sending and close the buffer."
  (interactive)
  (quit-window t))

;;;###autoload
(defun tuiw-send-with-temp-buffer (session-id)
  "Open a buffer to compose text to send to SESSION-ID."
  (interactive (list (tuiw--read-session "Session: ")))
  (let ((buf (get-buffer-create (format "*tuiw--send:%s*" session-id))))
    (with-current-buffer buf
      (tuiw-send-with-temp-buffer-mode)
      (setq tuiw-send-with-temp-buffer--session-id session-id)
      (erase-buffer))
    (pop-to-buffer buf)))


;;; List Session Mode

(defvar tuiw-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'tuiw-list-show)
    (define-key map (kbd "s") #'tuiw-list-send)
    (define-key map (kbd "d") #'tuiw-list-close)
    (define-key map (kbd "g") #'tuiw-list-refresh)
    (define-key map (kbd "a") #'tuiw-list-attach)
    map))

(define-derived-mode tuiw-list-mode tabulated-list-mode "Tuiw-List"
  "Major mode for listing tuiw sessions."
  (setq tabulated-list-format [("ID" 10 t)
                               ("Command" 40 t)
                               ("Directory" 40 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun tuiw-list-refresh ()
  "Refresh tuiw session list."
  (interactive)
  (setq tabulated-list-entries
        (mapcar (lambda (entry)
                  (list (car entry)
                        (vector (nth 0 entry)
                                (or (nth 1 entry) "")
                                (or (nth 2 entry) ""))))
                (tuiw--list)))
  (tabulated-list-print t))

(defun tuiw-list--get-id ()
  "Get session ID at point."
  (tabulated-list-get-id))

(defun tuiw-list-show ()
  "Show output of session at point."
  (interactive)
  (when-let ((id (tuiw-list--get-id)))
    (tuiw-view id)))

(defun tuiw-list-send (keys)
  "Send KEYS to session at point."
  (interactive "sKeys: ")
  (when-let ((id (tuiw-list--get-id)))
    (tuiw--send id keys)
    (message "Sent keys to %s" id)))

(defun tuiw-list-close ()
  "Close session at point."
  (interactive)
  (when-let ((id (tuiw-list--get-id)))
    (tuiw--close id)
    (tuiw-list-refresh)))

(defun tuiw-list-attach ()
  "Attach to session at point using vterm."
  (interactive)
  (when-let ((id (tuiw-list--get-id)))
    (tuiw-attach id)))

;;;###autoload
(defun tuiw-list ()
  "Display tuiw sessions in a tabulated list."
  (interactive)
  (let ((buf (get-buffer-create "*tuiw-sessions*")))
    (with-current-buffer buf
      (tuiw-list-mode)
      (tuiw-list-refresh))
    (pop-to-buffer buf)))



;;; Terminal Integration

(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

(defun tuiw-attach--vterm (session-id)
  "Attach to tuiw SESSION-ID using vterm."
  (vterm (format "*tuiw-vterm:%s*" session-id))
  (vterm-send-string (format "exec tmux attach -t tuiw-%s" session-id))
  (vterm-send-return))

;;;###autoload
(defun tuiw-attach (session-id)
  "Attach to tuiw SESSION-ID using terminal backend."
  (interactive (list (tuiw--read-session "Session: ")))
  (pcase tuiw-terminal-backend
    ('vterm (tuiw-attach--vterm session-id))
    (_ (error "Unknown terminal backend: %s" tuiw-terminal-backend))))

(provide 'tuiw)

;;; tuiw.el ends here
