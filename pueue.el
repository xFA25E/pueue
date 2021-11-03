;;; pueue.el --- Interface for pueue                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords: processes
;; Version: 1.0.1
;; URL: https://github.com/xFA25E/pueue
;; Package-Requires: ((emacs "27.1") (transient "0.3.6"))

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

;; Manage pueue tasks.

;;; Code:

;;;; REQUIRES

(require 'pueue-command)
(require 'pueue-info)

(require 'iso8601)
(require 'map)
(require 'tabulated-list)

;;;; CUSTOMIZATION OPTIONS

(defgroup pueue nil
  "Manage pueue processes."
  :group 'external
  :group 'processes
  :group 'applications)

;;;;; VARIABLES

(defcustom pueue-buffer-name
  "*Pueue"
  "Default buffer name for Pueue mode."
  :type 'string
  :group 'pueue)

;;;;; FACES

(defface pueue-result-success
  '((t :inherit success))
  "Pueue mode face used for not failed result."
  :group 'pueue)

(defface pueue-result-error
  '((t :inherit error))
  "Pueue mode face used for failed result."
  :group 'pueue)

;;;; LOCAL VARIABLES

(defvar-local pueue--status nil
  "Variable contaning parsed result of pueue status command.")

(defvar-local pueue--marked-ids nil
  "Marked ids in tabulated list mode.")

;;;; UTILS

(defun pueue--marked-ids ()
  "Get a list with marked-ids or with item under point."
  (or pueue--marked-ids (when-let ((id (tabulated-list-get-id))) (list id))))

(defun pueue--compare-ids (a b)
  "Comparator for tabulated list entry ids.
A and B are tabulated list entries that have ids in car
positions."
  (< (car a) (car b)))

(defun pueue--status ()
  "Get parsed pueue states."
  (with-temp-buffer
    (save-excursion
      (call-process "pueue" nil t nil "status" "--json"))
    (json-parse-buffer :null-object nil :false-object nil)))

;;;; FORMATTERS

(defun pueue--extract-hm (time)
  "Extract hours and minutes from iso8601 TIME string.
TIME can be nil."
  (pcase time
    ((and (pred stringp) (app iso8601-parse (seq _ M H)))
     (format "%02d:%02d" H M))
    (_ "")))

(defun pueue--format-result (result)
  "Format RESULT of pueue task.
RESULT can be a string or a hash-table {string: number} or nil.
Propertize resulting string with result faces."
  (pcase result
    ((pred stringp) (list result 'face 'pueue-result-success))
    ((and (pred mapp) (app map-keys keys))
     (list (string-join keys " ") 'face 'pueue-result-error))
    (_ "")))

;;;; PRINTERS

(defun pueue--refresh ()
  "Refresh pueue status."
  (setq pueue--status (pueue--status)
        pueue--marked-ids nil
        tabulated-list-entries
        (map-apply
         (pcase-lambda (id (map ("id" task-id) ("status" status) ("result" result)
                                ("start" start) ("end" end) ("command" command)))
           (list task-id (vector id status (pueue--format-result result)
                                 (pueue--extract-hm start) (pueue--extract-hm end)
                                 command)))
         (map-elt pueue--status "tasks"))))

(defun pueue--print-entry (id cols)
  "Function used in `tabulated-list-printer'.
See it's documentation for ID and COLS."
  (tabulated-list-print-entry id cols)
  (when (memq id pueue--marked-ids)
    (save-excursion
      (forward-line -1)
      (tabulated-list-put-tag "*"))))

;;;; COMMANDS

(define-derived-mode pueue-mode tabulated-list-mode "Pueue"
  "Pueue mode used to manage pueueu tasks."
  :group 'pueue
  (setq tabulated-list-padding 2
        tabulated-list-sort-key (cons "ID" t)
        tabulated-list-printer 'pueue--print-entry
        tabulated-list-format [("ID" 5 pueue--compare-ids)
                               ("Status" 9 t)
                               ("Result" 8 t)
                               ("Start" 6 t)
                               ("End" 6 t)
                               ("Command" 0 t)])
  (add-hook 'tabulated-list-revert-hook 'pueue--refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun pueue ()
  "Main entry command for pueue task manager."
  (interactive)
  (let ((buffer (get-buffer-create "*Pueue*")))
    (with-current-buffer buffer
      (pueue-mode)
      (pueue--refresh)
      (tabulated-list-print))
    (pop-to-buffer buffer '((display-buffer-reuse-window
                             display-buffer-same-window)))))

(defun pueue-mark ()
  "Mark pueue task at point."
  (interactive)
  (cl-pushnew (tabulated-list-get-id) pueue--marked-ids)
  (with-silent-modifications
    (tabulated-list-put-tag "*" t)))

(defun pueue-unmark ()
  "Unmark pueue taks at point."
  (interactive)
  (cl-callf2 delete (tabulated-list-get-id) pueue--marked-ids)
  (with-silent-modifications
    (tabulated-list-put-tag " " t)))

;;;; BINDINGS

(define-key pueue-mode-map "m" #'pueue-mark)
(define-key pueue-mode-map "u" #'pueue-unmark)

(define-key pueue-mode-map "a" #'pueue-command-add)
(define-key pueue-mode-map "c" #'pueue-command-clean)
(define-key pueue-mode-map "e" #'pueue-command-edit)
(define-key pueue-mode-map "Q" #'pueue-command-enqueue)
(define-key pueue-mode-map "f" #'pueue-command-follow)
(define-key pueue-mode-map "G" #'pueue-command-group)
(define-key pueue-mode-map "k" #'pueue-command-kill)
(define-key pueue-mode-map "l" #'pueue-command-log)
(define-key pueue-mode-map "L" #'pueue-command-parallel)
(define-key pueue-mode-map "P" #'pueue-command-pause)
(define-key pueue-mode-map "R" #'pueue-command-remove)
(define-key pueue-mode-map "t" #'pueue-command-reset)
(define-key pueue-mode-map "r" #'pueue-command-restart)
(define-key pueue-mode-map "d" #'pueue-command-send)
(define-key pueue-mode-map "s" #'pueue-command-start)
(define-key pueue-mode-map "H" #'pueue-command-stash)
(define-key pueue-mode-map "w" #'pueue-command-switch)

(define-key pueue-mode-map "\C-m" #'pueue-info)

;;;; PROVIDE

(provide 'pueue)
;;; pueue.el ends here
