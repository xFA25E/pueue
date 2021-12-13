;;; pueue.el --- Interface for pueue                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords: processes
;; Version: 1.0.4
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
  "*Pueue*"
  "Default buffer name for Pueue mode."
  :type 'string
  :group 'pueue)

;;;;; FACES

(defface pueue-status-success
  '((t :inherit success))
  "Pueue mode face used for not failed result."
  :group 'pueue)

(defface pueue-status-error
  '((t :inherit error))
  "Pueue mode face used for failed result."
  :group 'pueue)

;;;; LOCAL VARIABLES

(defvar-local pueue--status nil
  "Variable contaning parsed result of pueue status command.")

(defvar-local pueue--marked-ids nil
  "Marked ids in tabulated list mode.")

(defvar-local pueue--group-filter nil
  "Display tasks only of this group.
If NIL, shouw all tasks.")

;;;; UTILS

(defun pueue--marked-ids ()
  "Get a list with marked-ids or with item under point."
  (or pueue--marked-ids (when-let ((id (tabulated-list-get-id))) (list id))))

(defun pueue--marked-ids-add (id)
  "Add ID to variable `pueue--marked-ids'."
  (cl-pushnew id pueue--marked-ids))

(defun pueue--marked-ids-remove (id)
  "Remove ID from variable `pueue--marked-ids'."
  (cl-callf2 delete id pueue--marked-ids))

(defun pueue--marking-action (fn tag)
  "Apply FN to task id at point and add TAG."
  (funcall fn (tabulated-list-get-id))
  (with-silent-modifications
    (tabulated-list-put-tag tag t)))

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

(defun pueue--push-button (_pos)
  "Push tabulated-list BUTTON."
  (call-interactively #'pueue-info))

;;;; FORMATTERS

(defun pueue--extract-hm (time)
  "Extract hours and minutes from iso8601 TIME string.
TIME can be nil."
  (pcase time
    ((and (pred stringp) (app iso8601-parse (seq _ M H)))
     (format "%02d:%02d" H M))
    (_ "")))

(defun pueue--format-status (status)
  "Format STATUS of pueue task.
STATUS can be a string or a hash-table.  Propertize resulting
string with status faces."
  (pcase status
    ((pred stringp) status)
    ((pred mapp)
     (pcase (seq-first (map-pairs status))
       (`("Done" . "Success") '("Done" face pueue-status-success action pueue--push-button))
       (`("Done" . ,_) '("Done" face pueue-status-error action pueue--push-button))
       (`(,status . ,_) status)))
    (_ "")))

;;;; PRINTERS

(defun pueue--task-matches-group-filter-p (task)
  "Return NIL if TASK doesn't match PUEUE--GROUP-FILTER."
  (or (null pueue--group-filter)
      (string= pueue--group-filter (map-elt task "group"))))

(defun pueue--make-entry (task)
  "Make tabulated-list entry from pueue TASK."
  (list (map-elt task "id")
        (seq-into (seq-map (pcase-lambda ((seq key fn))
                             (funcall fn (map-elt task key)))
                           [["id" number-to-string]
                            ["status" pueue--format-status]
                            ["start" pueue--extract-hm]
                            ["end" pueue--extract-hm]
                            ["group" identity]
                            ["label" concat]
                            ["command" identity]])
                  'vector)))

(defun pueue--refresh ()
  "Refresh pueue status."
  (setq pueue--status (pueue--status)
        pueue--marked-ids nil
        tabulated-list-entries
        (thread-last (map-values (map-elt pueue--status "tasks"))
          (seq-filter #'pueue--task-matches-group-filter-p)
          (seq-map #'pueue--make-entry))))

(defun pueue--print-entry (id cols)
  "Function used in `tabulated-list-printer'.
See it's documentation for ID and COLS."
  (tabulated-list-print-entry id cols)
  (when (memq id pueue--marked-ids)
    (save-excursion
      (forward-line -1)
      (tabulated-list-put-tag "*"))))

;;;; BINDINGS

(easy-mmode-defmap pueue-mode-map
  '(("m" . pueue-mark)
    ("u" . pueue-unmark)
    ("F" . pueue-filter-by-group)
    ("a" . pueue-command-add)
    ("c" . pueue-command-clean)
    ("e" . pueue-command-edit)
    ("Q" . pueue-command-enqueue)
    ("f" . pueue-command-follow)
    ("G" . pueue-command-group)
    ("k" . pueue-command-kill)
    ("l" . pueue-command-log)
    ("L" . pueue-command-parallel)
    ("P" . pueue-command-pause)
    ("R" . pueue-command-remove)
    ("t" . pueue-command-reset)
    ("r" . pueue-command-restart)
    ("d" . pueue-command-send)
    ("s" . pueue-command-start)
    ("H" . pueue-command-stash)
    ("w" . pueue-command-switch)
    ("?" . pueue-command-help)
    ("\C-m" . pueue-info))
  "Keymap for `pueue-mode'.")

;;;; COMMANDS

(define-derived-mode pueue-mode tabulated-list-mode
  '("Pueue" (pueue--group-filter ("[" pueue--group-filter "]")))
  "Pueue mode used to manage pueueu tasks."
  :group 'pueue
  (setq tabulated-list-padding 2
        tabulated-list-sort-key (cons "ID" t)
        tabulated-list-printer 'pueue--print-entry
        tabulated-list-format (vector (list "ID" 5 'pueue--compare-ids)
                                      (list "Status" 9 t)
                                      (list "Start" 6 t)
                                      (list "End" 6 t)
                                      (list "Group" 8 t)
                                      (list "Label" 6 t)
                                      (list "Command" 0 t)))
  (add-hook 'tabulated-list-revert-hook 'pueue--refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun pueue ()
  "Main entry command for pueue task manager."
  (interactive)
  (let ((buffer (get-buffer-create pueue-buffer-name)))
    (with-current-buffer buffer
      (pueue-mode)
      (pueue--refresh)
      (tabulated-list-print))
    (pop-to-buffer buffer '((display-buffer-reuse-window
                             display-buffer-same-window)))))

(defun pueue-mark ()
  "Mark pueue task at point."
  (interactive)
  (pueue--marking-action #'pueue--marked-ids-add "*"))

(defun pueue-unmark ()
  "Unmark pueue taks at point."
  (interactive)
  (pueue--marking-action #'pueue--marked-ids-remove " "))

(defun pueue-filter-by-group ()
  "Filter pueue tasks by group or remove current filter."
  (interactive)
  (if pueue--group-filter
      (setq pueue--group-filter nil)
    (let* ((_history nil)
           (groups (map-keys (map-elt (pueue--status) "groups")))
           (group (completing-read "Group: " groups nil t nil '_history)))
      (setq pueue--group-filter group)))
  (revert-buffer nil t))

;;;; PROVIDE

(provide 'pueue)
;;; pueue.el ends here
