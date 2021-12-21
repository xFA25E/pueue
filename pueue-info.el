;;; pueue-info.el --- Detailed view of Pueue tasks   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

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

;; Detailed view for Pueue tasks.

;;; Code:

;;;; REQUIRES

(require 'ewoc)
(require 'iso8601)
(require 'map)

(defvar pueue--status)
(declare-function pueue--marked-ids "peueu")

;;;; CUSTOMZATION OPTIONS

;;;;; VARIABLES

(defcustom pueue-info-buffer-name
  "*Pueue Info*"
  "Default buffer name for Pueue Info mode."
  :type 'string
  :group 'pueue)

;;;;; FACES

(defface pueue-info-label
  '((t :inherit font-lock-constant-face))
   "Pueue Info mode face used for field labels."
   :group 'pueue)

(defface pueue-info-environment-variable
  '((t :inherit font-lock-variable-name-face))
   "Pueue Info mode face used for environment variables."
   :group 'pueue)

;;;; LOCAL VARIABLES

(defvar-local pueue-info--ewoc nil
  "Pueue Info mode ewoc data.")

(defvar-local pueue-info--tasks nil
  "Pueue tasks.
It is set during the `pueue-info' call.  It is a hash-table with
string ids as keys and hash-tables describing tasks as values.
Each individual task has fields and values.  Fields are strings
and values have the following types:

 \"id\"               -> number
 \"command\"          -> string
 \"label\"            -> string or nil
 \"path\"             -> string
 \"status\"           -> string or hash-table
 \"start\"            -> iso8601 time string
 \"end\"              -> iso8601 time string
 \"group\"            -> string
 \"enqueue_at\"       -> iso8601 time string or nil
 \"dependencies\"     -> vector of numbers
 \"prev_status\"      -> string
 \"original_command\" -> string
 \"envs\"             -> hash-table {string: string}")

(defvar-local pueue-info--history nil
  "List containing history of visited tasks.
History is recorded only when tasks are visited from the same
buffer.  Every time `pueue-info' is called from different buffer,
history is reset.

Every history item has the following form:

(((ID . DRAW-ENVS-P)...) . POSITION)

See `pueue-info--draw-envs' for more information on
DRAW-ENVS-P.")

;;;; UTILS

(defun pueue-info--footer-p (node)
  "Check whether NODE is ewoc footer."
  (eq (ewoc--footer pueue-info--ewoc) node))

(defun pueue-info--make-node-visible (node)
  "Make ewoc NODE visible as much as possible."
  (let* ((ewoc pueue-info--ewoc)
         (next-node (or (ewoc-next ewoc node) (ewoc--footer ewoc)))
         (next-node-location (ewoc-location next-node)))
    (unless (pos-visible-in-window-p next-node-location)
      (let ((node-line-number (line-number-at-pos (ewoc-location node)))
            (next-node-line-number (line-number-at-pos next-node-location)))
        (recenter (- (min (- next-node-line-number node-line-number) (window-height))))))))

;;;; DRAW FUNCTIONS

(defun pueue-info--draw-number (number &optional _)
  "Draw NUMBER value."
  (insert " " (number-to-string number)))

(defun pueue-info--draw-string (string &optional _)
  "Draw STRING value.
STRING can be a string or nil."
  (when string (insert " " string)))

(defun pueue-info--draw-time (time &optional _)
  "Draw iso8601 time string.
TIME can be a string or nil."
  (when time
    (seq-let (S M H d m y) (iso8601-parse time)
      (insert (format " %d-%02d-%02d %02d:%02d:%02d" y m d H M S)))))

(defun pueue-info--draw-result (result &optional _)
  "Draw pueue task RESULT.
RESULT can be a string or a hash-table."
  (pcase result
    ((pred stringp) (pueue-info--draw-string result))
    ((pred mapp)
     (pcase (seq-first (map-pairs result))
       (`("Failed" . ,code)
        (insert " Failed")
        (pueue-info--draw-number code))
       (`("FailedToSpawn" . ,reason)
        (insert " FailedToSpawn")
        (pueue-info--draw-string reason))))))

(defun pueue-info--draw-status (status &optional _)
  "Draw pueue task STATUS.
STATUS can be a string or a hash-table."
  (pcase status
    ((pred stringp) (pueue-info--draw-string status))
    ((pred mapp)
     (pcase (seq-first (map-pairs status))
       (`("Done" . ,result)
        (insert " Done")
        (pueue-info--draw-result result))
       (`("Stashed" . ,(map ("enqueue_at" enqueue-at)))
        (insert " Stashed")
        (pueue-info--draw-time enqueue-at))))))

(defun pueue-info--push-dependency-button (pos)
  "Push button on task dependency at position POS.
This function is used as action in dependency buttons.  It
records history to `pueue-info--history'."
  (let ((data (ewoc-collect pueue-info--ewoc #'identity))
        (task-id (button-get (button-at pos) 'task-id)))
    (push (cons data (point)) pueue-info--history)
    (ewoc-filter pueue-info--ewoc #'ignore)
    (ewoc-enter-last pueue-info--ewoc (cons task-id nil))))

(defun pueue-info--draw-dependencies (dependencies &optional _)
  "Draw DEPENDENCIES.
DEPENDENCIES is a vector of numbers.  Each number represents an
id of some pueue task.  DEPENDENCIES are drawn as clickable
buttons."
  (let ((action #'pueue-info--push-dependency-button))
    (seq-do (pcase-lambda ((and (app number-to-string id) task-id))
              (insert " ")
              (insert-text-button id 'action action 'task-id task-id))
            dependencies)))

(defun pueue-info--draw-envs (envs draw-envs-p)
  "Draw pueue task environment variables.
ENVS is a hash-table {string: string} and DRAW-ENVS-P is a
boolean.  It will draw environment variables only if DRAW-ENVS-P
is non-nil."
  (if (not draw-envs-p)
      (insert "...")
    (insert "\n")
    (seq-do (pcase-lambda (`(,var . ,value))
              (let ((var (propertize var 'face 'pueue-info-environment-variable)))
                (insert "  " var "=" value "\n")))
            (seq-sort-by #'car #'string< (map-pairs envs)))))

(defun pueue-info--draw-task (data)
  "Draw pueue TASK.
This is a main function used as ewoc pretty printer.  DATA has
the following form: (ID . DRAW-ENVS-P).  ID is a number and
DRAW-ENVS-P is a boolean.

See `pueue-info--draw-envs' for more information on DRAW-ENVS-P."
  (pcase-let* ((`(,(app number-to-string id) . ,draw-envs-p) data)
               ((map (id task)) pueue-info--tasks))
    (seq-do
     (pcase-lambda ((seq key label fn))
       (insert (propertize label 'face 'pueue-info-label) ":")
       (funcall fn (map-elt task key) draw-envs-p)
       (insert "\n"))
     [["id" "ID" pueue-info--draw-number]
      ["command" "Command" pueue-info--draw-string]
      ["label" "Label" pueue-info--draw-string]
      ["path" "Path" pueue-info--draw-string]
      ["status" "Status" pueue-info--draw-status]
      ["start" "Start" pueue-info--draw-time]
      ["end" "End" pueue-info--draw-time]
      ["group" "Group" pueue-info--draw-string]
      ["enqueue_at" "Enqueue at" pueue-info--draw-time]
      ["dependencies" "Dependencies" pueue-info--draw-dependencies]
      ["prev_status" "Previous status" pueue-info--draw-status]
      ["original_command" "Original command" pueue-info--draw-string]
      ["envs" "Environment variables" pueue-info--draw-envs]])))

;;;; BINDINGS

(easy-mmode-defmap pueue-info-mode-map
  '(("n" . pueue-info-next-task)
    ("p" . pueue-info-previous-task)
    ("e" . pueue-info-toggle-envs)
    ("l" . pueue-info-backward-history))
  "Keymap for `pueue-info-mode'.")

;;;; COMMANDS

(define-derived-mode pueue-info-mode special-mode "PueueInfo"
  "Mode used to draw details pueue tasks information."
  :group 'pueue
  (buffer-disable-undo)
  (with-silent-modifications (erase-buffer))
  (setq pueue-info--ewoc (ewoc-create #'pueue-info--draw-task)))

(defun pueue-info (tasks task-ids)
  "Main entry command to display details of pueue TASK-IDS.
TASK-IDS is a sequence of numbers.  Each number corresponds to
some pueue task id in TASKS.

When this command is called from Pueue mode, it gathers
automatically all TASKS in that buffer.

See `pueue-info--tasks' for more information on TASKS."
  (interactive
   (if (not (derived-mode-p 'pueue-mode))
       (user-error "Not in Pueue mode")
     (list (map-elt pueue--status "tasks") (pueue--marked-ids))))

  (with-current-buffer (get-buffer-create pueue-info-buffer-name)
    (pueue-info-mode)
    (setq pueue-info--tasks tasks)
    (seq-doseq (task-id task-ids)
      (ewoc-enter-last pueue-info--ewoc (cons task-id nil)))
    (pop-to-buffer (current-buffer))))

(defun pueue-info-next-task (n)
  "Go to next Nth task."
  (interactive "p")
  (let ((node (ewoc-goto-next pueue-info--ewoc n)))
    (unless (pueue-info--footer-p node)
      (pueue-info--make-node-visible node))))

(defun pueue-info-previous-task (n)
  "Go to previous Nth taks."
  (interactive "p")
  (ewoc-goto-prev pueue-info--ewoc n))

(defun pueue-info-toggle-envs ()
  "Toggle display of environment variables of a task at point."
  (interactive)
  (when-let ((node (ewoc-locate pueue-info--ewoc)))
    (let ((data (ewoc-data node)))
      (setcdr data (not (cdr data))))
    (ewoc-invalidate pueue-info--ewoc node)
    (pueue-info--make-node-visible node)))

(defun pueue-info-backward-history ()
  "Go back in history."
  (interactive)
  (when-let ((item (pop pueue-info--history)))
    (ewoc-filter pueue-info--ewoc #'ignore)
    (mapc (apply-partially #'ewoc-enter-last pueue-info--ewoc) (car item))
    (goto-char (cdr item))))

;;;; PROVIDE

(provide 'pueue-info)
;;; pueue-info.el ends here
