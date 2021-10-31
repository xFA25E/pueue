;;; pueue-command.el --- Pueue interactive commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords: processes
;; Version: 1.0.0
;; URL: https://github.com/xFA25E/pueue
;; Package-Requires: ((emacs "27.1"))

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

;; Interactive pueue commands

;; TODO:
;; - error handling of commands
;; - comments
;; - show in transient commands currently selected task-ids
;; - better handling of task-ids and transient-args (sometimes "--all" means
;;   task-ids can be null, but pueue--marked-ids gives task at point anyway)
;; - refactor

;;; Code:

;;;; REQUIRES

(require 'seq)
(require 'transient)

(declare-function pueue--marked-ids "pueue")

;;;; COMMANDS

;;;;; ADD

;;;;; CLEAN

(defun pueue-command--clean (&rest args)
  (interactive (transient-args 'pueue-command-clean))
  (apply #'call-process "pueue" nil nil nil "clean" args)
  (revert-buffer nil t))

(transient-define-prefix pueue-command-clean ()
  ["Arguments"
   ("-s" "Only clean tasks that finished successfully" "--successful-only")]
  ["Actions"
   ("c" "Clean" pueue-command--clean)])

;;;;; COMPLETIONS

;;;;; EDIT

;;;;; ENQUEUE

;;;;; FOLLOW

(defun pueue-command--follow (task-id &rest args)
  (interactive
   (let ((marked-ids (pueue--marked-ids)))
     (if marked-ids
         (cons (seq-first marked-ids) (transient-args 'pueue-command-follow))
       (user-error "Not in Pueue mode"))))

  (let* ((buffer-name "*Pueue Follow*")
         (id (number-to-string task-id))
         (command (append (list "pueue" "follow") args (list id)))
         (command (string-join command " ")))
    (when-let ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer))
    (async-shell-command command buffer-name)))

(transient-define-prefix pueue-command-follow ()
  ["Arguments"
   ("-e" "Show stderr instead of stdout" "--err")]
  ["Actions"
   ("f" "Follow" pueue-command--follow)])

;;;;; GROUP

;;;;; HELP

;;;;; KILL

(defun pueue-command--kill (task-ids &rest args)
  (interactive
   (let ((args (transient-args 'pueue-command-kill))
         (marked-ids (pueue--marked-ids)))
     (cond (marked-ids
            (cons marked-ids args))
           ((or (transient-arg-value "--all" args) (transient-arg-value "--group=" args)) args)
           (t (user-error "Not in Pueue mode")))))

  (let ((ids (seq-map #'number-to-string task-ids)))
    (apply #'call-process "pueue" nil nil nil "kill" (append args ids))
    (revert-buffer nil t)))

(transient-define-prefix pueue-command-kill ()
  ["Arguments"
   ("-a" "Kill all running tasks across ALL groups. This also pauses all groups" "--all")
   ("-c" "Send the SIGTERM signal to all children as well" "--children")
   ("-g" "Kill all running tasks in a group. This also pauses the group" "--group=")]
  ["Actions"
   ("k" "Kill" pueue-command--kill)])

;;;;; LOG

;;;;; PARALLEL

;;;;; PAUSE

(defun pueue-command--pause (task-ids &rest args)
  (interactive (cons (pueue--marked-ids) (transient-args 'pueue-command-pause)))
  (let ((ids (seq-map #'number-to-string task-ids)))
    (apply #'call-process "pueue" nil nil nil "pause" (append args ids))
    (revert-buffer nil t)))

(transient-define-prefix pueue-command-pause ()
  ["Arguments"
   ("-a" "Pause all groups!" "--all")
   ("-c" "Also pause direct child processes of a task's main proces." "--children")
   ("-g" "Pause a specific group" "--group=")]
  ["Actions"
   ("k" "Kill" pueue-command--pause)])

;;;;; REMOVE

;;;;; RESET

;;;;; RESTART

(defun pueue-command--restart (task-ids &rest args)
  (interactive (cons (pueue--marked-ids) (transient-args 'pueue-command-restart)))
  (let ((ids (seq-map #'number-to-string task-ids)))
    (apply #'call-process "pueue" nil nil nil "restart" (append args ids))
    (revert-buffer nil t)))

(transient-define-prefix pueue-command-restart ()
  ["Arguments"
   ("-a" "Restart all failed tasks" "--all-failed")
   ("-e" "Edit the tasks' command before restarting" "--edit")
   ("-p" "Edit the tasks' path before restarting" "--edit-path")
   ("-i" "Restart the task by reusing the already existing tasks" "--in-place")
   ("-k" "Immediately start the tasks" "--start-immediately")
   ("-s" "Set the restarted task to a \"Stashed\" state" "--stashed")]
  ["Actions"
   ("r" "Restart" pueue-command--restart)])

;;;;; SEND

;;;;; SHUTDOWN

;;;;; START

(defun pueue-command--start (task-ids &rest args)
  (interactive (cons (pueue--marked-ids) (transient-args 'pueue-command-start)))
  (let ((ids (seq-map #'number-to-string task-ids)))
    (apply #'call-process "pueue" nil nil nil "start" (append args ids))
    (revert-buffer nil t)))

(transient-define-prefix pueue-command-start ()
  ["Arguments"
   ("-a" "Resume all groups" "--all")
   ("-c" "Also resume direct child processes of your paused tasks" "--children")
   ("-g" "Resume a specific group and all paused tasks in it" "--group=")]
  ["Actions"
   ("s" "Start" pueue-command--start)])

;;;;; STASH

;;;;; STATUS

;;;;; SWITCH

;;;;; WAIT

;;;; PROVIDE

(provide 'pueue-command)
;;; pueue-command.el ends here
