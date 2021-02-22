;;; pueue.el --- Interface for pueue                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <>
;; Keywords: processes

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

;; add commands
;; add faces
;; make pueue dependencies clickable
;; pueue edit should be asyncronous and update list in callback
;; set process filter for follow
;; make transient interfaces
;; add hints

;;; Code:

;;;; REQUIRES

(require 'bui)

;;;; API

(defun pueue-call (destination &rest args)
  (apply #'call-process "pueue" nil destination nil args))

(defun pueue-call-ids (command task-ids &optional childrenp)
  (cl-assert task-ids)
  (apply #'pueue-call nil command
         (append (when childrenp (list "--children"))
                 (mapcar #'number-to-string task-ids)))
  (bui-revert nil t))

(defun pueue-status (&optional object-type)
  (let ((object-type (or object-type 'alist)))
    (with-temp-buffer
      (save-excursion
        (pueue-call t "status" "--json"))
      (json-parse-buffer :object-type object-type))))

(defun pueue-groups ()
  (cl-loop with groups = (gethash "groups" (pueue-status 'hash-table))
           for group being the hash-keys of groups
           collect group))

;;;; BUI

(defun pueue-list-hint ()
  (bui-format-hints
   ;; '(("\\[buffers-list-switch-to-buffer]") " switch to buffer;\n"
   ;;   ("\\[buffers-list-kill-buffers]") " kill buffer(s);\n")
   (bui-default-hint)))

(defun pueue-get-entries (&rest task-ids)
  (let ((get-id (apply-partially #'alist-get 'id))
        (tasks (mapcar #'cdr (alist-get 'tasks (pueue-status)))))
    (if task-ids
        (mapcar (lambda (id) (cl-find id tasks :key get-id :test #'=)) task-ids)
      tasks)))

(defun pueue-list-describe (&rest task-ids)
  (bui-get-display-entries 'pueue 'info task-ids))

(defun pueue-filter-by-group (entry group)
  (interactive (list '<> (completing-read "Group: " (pueue-groups))))
  (equal group (bui-assq-value entry 'group)))

;;;;; FORMATTERS

;;;;;; LIST

(defun pueue-list-id-sort-predicate (lhs rhs)
  (< (car lhs) (car rhs)))

(defun pueue-list-result-value-fn (result &optional _)
  (pcase-exhaustive result
    ((pred stringp) result)
    (:null "")
    (`((,status . ,_)) (symbol-name status))))

(defun pueue-list-time-value-fn (time &optional _)
  (pcase-exhaustive time
    (:null "")
    ((app iso8601-parse `(,_ ,m ,h . ,_)) (format "%02d:%02d" h m))))

;;;;;; INFO

(defun pueue-info-result-insert-value (value)
  (bui-info-insert-value-simple
   (pcase-exhaustive value
     ((pred stringp) value)
     (:null "")
     (`((,status . ,info)) (format "%s (%s)" status info)))))

(defun pueue-info-time-insert-value (value)
  (bui-info-insert-value-simple
   (pcase-exhaustive value
     (:null "")
     ((app iso8601-parse `(,s ,m ,h ,d ,mo ,y . ,_))
      (format "%d-%02d-%02d %02d:%02d:%02d" y mo d h m s)))))

(defun pueue-info-group-insert-value (value)
  (bui-info-insert-value-simple
   (pcase-exhaustive value
     (:null "")
     ((pred stringp) value))))

(defun pueue-info-envs-insert-value (value)
  (bui-info-insert-value-simple
   (mapconcat (lambda (env) (concat (symbol-name (car env)) "=" (cdr env)))
              value "\n")))

;;;;; DEFINE

(bui-define-groups pueue
  :parent-group tools
  :parent-faces-group faces
  :group-doc "Settings for '\\[pueue]' command."
  :faces-group-doc "Faces for '\\[pueue]' command.")

(bui-define-entry-type pueue
  :titles '((prev_status . "Previous status")
            (enqueue_at . "Enqueue at"))
  :get-entries-function #'pueue-get-entries
  :filter-predicates (append bui-filter-predicates '(pueue-filter-by-group)))

(bui-define-interface pueue list
  :buffer-name "*Pueue*"
  :describe-function #'pueue-list-describe
  :format '((id nil 5 pueue-list-id-sort-predicate)
            (status nil 9 t)
            (result pueue-list-result-value-fn 8 t)
            (start pueue-list-time-value-fn 8 t)
            (end pueue-list-time-value-fn 6 t)
            (command nil 0 t))
  :hint 'pueue-list-hint
  :sort-key '(id . t))

(bui-define-interface pueue info
  :format '((id simple (simple))
            (command simple (simple))
            (path simple (simple))
            (status simple (simple))
            (prev_status simple (simple))
            (result simple (pueue-info-result-insert-value))
            (start simple (pueue-info-time-insert-value))
            (end simple (pueue-info-time-insert-value))
            (group simple (pueue-info-group-insert-value))
            (enqueue_at simple (pueue-info-time-insert-value))
            (dependencies simple (simple))
            (envs simple (pueue-info-envs-insert-value))))

;;;; COMMANDS

(defun pueue ()
  (interactive)
  (bui-get-display-entries 'pueue 'list))

(defun pueue-clean ()
  (interactive)
  (pueue-call nil "clean")
  (bui-revert nil t))

(defun pueue-kill (task-ids &optional childrenp)
  (interactive (list (bui-list-marked-or-current) current-prefix-arg))
  (pueue-call-ids "kill" task-ids childrenp))

(defun pueue-pause (task-ids &optional childrenp)
  (interactive (list (bui-list-marked-or-current) current-prefix-arg))
  (pueue-call-ids "pause" task-ids childrenp))

(defun pueue-restart (task-ids)
  (interactive (list (bui-list-marked-or-current)))
  (pueue-call-ids "restart" task-ids))

(defun pueue-start (task-ids &optional childrenp)
  (interactive (list (bui-list-marked-or-current) current-prefix-arg))
  (pueue-call-ids "start" task-ids childrenp))

(defun pueue-follow (id &optional stderrp)
  (interactive (list (bui-list-current-id) current-prefix-arg))
  (let ((buffer-name "*Pueue Follow*"))
    (when-let ((buffer (get-buffer "*Pueue Follow*")))
      (kill-buffer buffer))

    (async-shell-command
     (concat "pueue follow " (when stderrp "--err ") (number-to-string id))
     buffer-name)))

;;;; BINDINGS

(define-key pueue-list-mode-map (kbd "F") #'pueue-follow)
(define-key pueue-list-mode-map (kbd "c") #'pueue-clean)
(define-key pueue-list-mode-map (kbd "k") #'pueue-kill)
(define-key pueue-list-mode-map (kbd "P") #'pueue-pause)
(define-key pueue-list-mode-map (kbd "t") #'pueue-start)
(define-key pueue-list-mode-map (kbd "T") #'pueue-restart)

;;;; PROVIDE

(provide 'pueue)
;;; pueue.el ends here
