;;; org-urgency.el --- Sort TODOs by urgency

;; Author: Sheda <sheda@fsfe.org>
;; Url: https://github.com/shedatc/org-urgency

;;; Commentary:

;; This package is an attempt to integrate the idea behind Taskwarrior's urgency
;; measurement to the Org ecosystem. It compute an urgency score from the
;; various traits of a TODO entry. The score is stored as a property that can
;; then be used for sorting from a custom agenda.
;;
;; [1] https://taskwarrior.org/docs/urgency.html
;;
;; The following traits are currently supported:
;;
;;  - Priority
;;  - Deadline
;;  - Activity
;;  - Age
;;  - Tags
;;  - Blocking

;;; Usage:

;; Add a custom agenda:
;;
;; (org-urgency/add-custom-agenda-command (kbd "u"))
;;
;; and bind a function like the following one to some key:
;;
;; (defun show-todo-entries-sorted-by-urgency ()
;;   "Display TODOs sorted by urgency."
;;   (interactive)
;;   (org-agenda nil (kbd "u"))
;;   (switch-to-buffer "*Org Agenda*"))

;;; Tips:

;; If you want to know how an urgency score is computed, call
;; org-urgency/insert-description-block-at-point from the body of the
;; corresponding TODO entry. It will insert a block containing a table listing
;; the score of its various traits. E.g.,
;;
;; #+BEGIN: urgency :id t2
;; | Property    | Coefficient | Value                 | Score |
;; |-------------+-------------+-----------------------+-------|
;; | Priority    |         1.0 | 6.00 (A)              |  6.00 |
;; | Deadline    |       12.00 | 1.00 <2019-01-09 Wed> | 12.00 |
;; | Activity    |        4.00 | 0.00 (inactive)       |  0.00 |
;; | Age         |        2.00 | 0.00 (0d)             |  0.00 |
;; | Tag :Perl:  |        1.00 | 1.0                   |  1.00 |
;; | Blocking t1 |        2.00 | 1.0                   |  2.00 |
;; |-------------+-------------+-----------------------+-------|
;; | Total       |             |                       | 21.00 |
;; #+END

;;; Credits:

;; This package is inspired by Taskwarrior [1] originaly written by Paul Beckingham.
;;
;; [1] https://taskwarrior.org

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Vars

(defvar org-urgency/per-priority-scores
  '(("A" . 6.0)
    ("B" . 3.9)
    ("C" . 1.8))
  "The scores for each priority.")

(defvar org-urgency/deadline-coefficient 12.0
  "The coefficient for the deadline property.")

(defvar org-urgency/activity-coefficient 4.0
  "The coefficient for active tasks.")

(defvar org-urgency/age-coefficient 2.0
  "The coefficient for the age of tasks.")

(defvar org-urgency/max-age 365
  "The maximum age of a task in days.")

(defvar org-urgency/blocking-coefficient 2.0
  "The coefficient for blocking tasks.")

(defvar org-urgency/default-tag-score 1.0
  "The default score for a tag attached to a task.")

(defvar org-urgency/per-tag-scores
  '(("next" . 15.0))
  "The scores for specific tags attached to a task.")

(defvar org-urgency/parents-property-name
  "PARENTS"
  "The name of the property used to list the parents of the entry.")

(defvar org-urgency/children-property-name
  "CHILDREN"
  "The name of the property used to list the children of the entry.")

;;;; Functions

;;;;; Commands

(defun org-urgency/insert-description-block-at-point ()
  "Insert a block describing how the urgency score of the TODO entry at point is computed."
  (interactive)
  (let* ((block-start (point-marker))
         (id          (org-id-get nil t)))
    (insert (format "#+BEGIN: urgency :id %s\n" (org-id-get nil t)))
    (insert (org-urgency/describe-as-org-table id))
    (org-table-align)
    (insert "#+END\n")
    (org-indent-region block-start (point))))

(defun org-urgency/update-urgency-property ()
  "Update the URGENCY property of the TODO entry at point."
  (interactive)
  (save-excursion
    (org-id-goto (org-id-get nil t))
    (org-set-property "URGENCY"
                      (format "%.2f" (org-urgency/build-urgency-score)))))

(defun org-urgency/show-tasks-by-urgency ()
  "Display TODOs sorted by urgency."
  (interactive)
  (org-agenda nil (kbd "u"))
  (switch-to-buffer "*Org Agenda*"))

;;;;; Building the Urgency Score

(defun org-urgency/build-urgency-score ()
  "Build the urgency of the TODO entry at point."
  (+ (org-urgency/priority-score)
     (org-urgency/deadline-score)
     (org-urgency/activity-score)
     (org-urgency/age-score)
     (org-urgency/tags-score)
     (org-urgency/blocking-score)))

;;;;;; Priority

(defun org-urgency/priority-score ()
  "Extract the priority of the TODO entry at point and return its corresponding score."
  (let* ((priority (org-entry-get (point-marker) "PRIORITY"))
         (kv       (assoc priority org-urgency/per-priority-scores)))
    (if (null kv)
        0.0
      (cdr kv))))

(defun org-urgency/describe-priority-score ()
  "Describe how the priority score is computed for the TODO entry at point."
  (let* ((priority (org-entry-get (point-marker) "PRIORITY"))
         (score    (org-urgency/priority-score)))
    (when (not (null priority))
      (format-message "| Priority | 1.0 | %.2f (%s) | %.2f |\n"
                      score priority score))))

;;;;;; Deadline

(defun org-urgency/scaled-deadline ()
  "Extract the deadline of the TODO entry at point and scale it.

Map a range of 21 days to the range [0.2, 1.0]:

            Past          Present                                      Future
            Overdue       Due                                          Due

Distance    7 6 5 4 3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 days

        <-- 1.0                         linear                         0.2 -->
            capped                                                  capped

References:
- src/Task.cpp, Task::urgency_due().
"
  (let* ((deadline (org-entry-get (point-marker) "DEADLINE")))
    (if (or (null deadline)
            (string-empty-p deadline))
        0.0
      (let* ((distance (* -1.0 (org-time-stamp-to-now deadline))))
        ;; Distance is positive if deadline is in the past (i.e., the task is overdue).
        (cond
         ((>= distance 7.0)   1.0)      ;; Is overdue for more than one week so highest urgency.
         ((>= distance -14.0) (+ (/ (* (+ distance 14.0) 0.8) 21.0) 0.2))
         (t                   0.2)))))) ;; Still have more than 14 days to complete the task so lowest urgency.

(defun org-urgency/deadline-score ()
  "Extract the deadline of the TODO entry at point and return its corresponding score."
  (let* ((scaled-deadline (org-urgency/scaled-deadline)))
    (* org-urgency/deadline-coefficient scaled-deadline)))

(defun org-urgency/describe-deadline-score ()
  "Describe how the deadline score is computed for the TODO entry at point."
  (let* ((deadline        (org-entry-get (point-marker) "DEADLINE"))
         (scaled-deadline (org-urgency/scaled-deadline)))
    (when (not (null deadline))
      (format-message "| Deadline | %.2f | %.2f %s | %.2f |\n"
                      org-urgency/deadline-coefficient
                      scaled-deadline
                      deadline
                      (org-urgency/deadline-score)))))

;;;;;; Activity

;; FIXME Need to find the first keyword of the TODO sequence in case it's not TODO.
(defun org-urgency/is-active ()
  "Tell if the TODO entry at point is active."
  (let* ((state (org-entry-get (point-marker) "TODO")))
    (and (not (string= "TODO" state)))))

(defun org-urgency/activity-score ()
  "Extract the active state of the TODO entry at point and return its corresponding score."
  (if (org-urgency/is-active)
      org-urgency/activity-coefficient
    0.0))

(defun org-urgency/describe-activity-score ()
  "Describe how the activity score is computed for the TODO entry at point."
  (let* ((is-active (org-urgency/is-active)))
    (format-message "| Activity | %.2f | %.2f (%s) | %.2f |\n"
                    org-urgency/activity-coefficient
                    (if is-active 1.0 0.0)
                    (if is-active "active" "inactive")
                    (org-urgency/activity-score))))

;;;;;; Age

;; FIXME This function is not deterministic oO
(defun org-urgency/age-score ()
  "Extract the age of the TODO entry at point and return its corresponding score."
  (let* ((timestamp (org-entry-get (point-marker) "TIMESTAMP")))
    (if (null timestamp)
        0.0
      (let* ((age   (* -1.0 (org-time-stamp-to-now timestamp) ))
             (score (if (> age org-urgency/max-age)
                        org-urgency/age-coefficient
                      (* org-urgency/age-coefficient (/ age org-urgency/max-age)))))
        score))))

(defun org-urgency/describe-age-score ()
  "Describe how the age score is computed for the TODO entry at point."
  (let* ((timestamp  (org-entry-get (point-marker) "TIMESTAMP"))
         (age        (if (null timestamp)
                         0.0
                       (* -1.0 (org-time-stamp-to-now timestamp))))
         (scaled-age (if (> age org-urgency/max-age)
                         1.0
                       (/ age org-urgency/max-age))))
    (format-message "| Age | %.2f | %.2f (%dd) | %.2f |\n"
                    org-urgency/age-coefficient
                    scaled-age
                    age
                    (org-urgency/age-score))))

;;;;;; Tags

(defun org-urgency/tag-score (tag)
  "Return the score of the given TAG."
  (let* ((kv (assoc tag org-urgency/per-tag-scores)))
    (if (null kv)
        org-urgency/default-tag-score
      (cdr kv))))

(defun org-urgency/get-entry-tags ()
  "Return the list of all the tags bound to the entry at point. Include inherited tags."
  (let* ((tags (org-entry-get (point-marker) "ALLTAGS" t)))
    (if (null tags)
        (list)
      (split-string tags ":" t))))

(defun org-urgency/tags-score ()
  "Extract the tags attached to the TODO entry at point and return their aggregated scores."
  (let* ((tags (org-urgency/get-entry-tags))
         (per-tag-scores (mapcar 'org-urgency/tag-score tags)))
    (apply '+ per-tag-scores)))

(defun org-urgency/describe-tag-score (tag)
  "Describe the score of the given TAG."
  (let* ((score (org-urgency/tag-score tag)))
    (format-message "| Tag :%s: | %.2f | 1.0 | %.2f |\n" tag score score)))

(defun org-urgency/describe-tags-score ()
  "Describe how the tags score is computed for the TODO entry at point."
  (let* ((tags (org-urgency/get-entry-tags)))
    (s-join "" (mapcar 'org-urgency/describe-tag-score tags))))

;;;;;; Blocking

(defun org-urgency/parents-ids ()
  "Return the IDs of the parents of the entry at point."
  (let* ((ids (org-entry-get (point-marker) org-urgency/parents-property-name)))
    (if (null ids)
        (list)
      (split-string ids " "))))

(defun org-urgency/children-ids ()
  "Return the IDs of the children of the entry at point."
  (let* ((ids (org-entry-get (point-marker) org-urgency/children-property-name)))
    (if (null ids)
        (list)
      (split-string ids " "))))

(defun org-urgency/is-not-done-p (id)
  "Tell if the entry with the given ID is a TODO entry that is not done. Will return nil if the entry is not a TODO entry."
  (save-excursion
    (org-id-goto id)
    (and (org-entry-is-todo-p)
         (not (org-entry-is-done-p)))))

(defun org-urgency/blocking-score ()
  "List the TODOs blocked by the one at point and return the corresponding score."
  (let* ((parents (org-urgency/parents-ids)))
    (apply '+ (mapcar (lambda (parent-id)
                        (if (org-urgency/is-not-done-p parent-id)
                            org-urgency/blocking-coefficient
                          0.0))
                      parents))))

(defun org-urgency/describe-blocking-score ()
  "Describe how the blocking score is computed for the TODO entry at point."
  (let* ((parents (org-urgency/parents-ids)))
    (s-join "" (mapcar (lambda (parent-id)
                         (when (org-urgency/is-not-done-p parent-id)
                           (format-message "| Blocking %s | %.2f | 1.0 | %.2f |\n"
                                           parent-id
                                           org-urgency/blocking-coefficient
                                           org-urgency/blocking-coefficient)))
                       parents))))

;;;;; URGENCY Property Manipulation

(defun org-urgency/get-urgency-score ()
  "Return the urgency of the TODO entry at point. If the urgency is unknown, build it."
  (save-excursion
    (org-id-goto (org-id-get nil t))
    (let* ((urgency-score (org-urgency/get-urgency-property)))
      (if (null urgency-score)
          (progn
            (org-urgency/update-urgency-property)
            (org-urgency/get-urgency-property))
        urgency-score))))

(defun org-urgency/get-urgency-property ()
  "Get the value of the URGENCY property of the TODO entry at point."
  (save-excursion
    (org-id-goto (org-id-get nil t))
    (let* ((raw-urgency-score (org-entry-get (point-marker) "URGENCY")))
      (if (or (null raw-urgency-score)
              (string-empty-p raw-urgency-score))
          nil
        (string-to-number raw-urgency-score)))))

;;;;; Agenda Integration

(defun org-urgency/get-urgency-score-at-marker (marker)
  "Return the urgency of the TODO entry at the given MARKER.

If the urgency is unknown, build it and update the URGENCY property."
  (with-current-buffer (marker-buffer marker)
    (goto-char (marker-position marker ))
    (org-urgency/get-urgency-score)))

(defun org-urgency/cmp-urgency-scores (a b)
  "Compare two TODOs, A and B, by comparing their urgency scores.

If A is before B, return -1. If A is after B, return 1. If they are equal return nil."
  (let* ((a-marker        (get-text-property 0 'org-marker a))
         (b-marker        (get-text-property 0 'org-marker b))
         (a-urgency-score (org-urgency/get-urgency-score-at-marker a-marker))
         (b-urgency-score (org-urgency/get-urgency-score-at-marker b-marker)))
    (progn
      (cond ((= a-urgency-score b-urgency-score) nil)
            ((> a-urgency-score b-urgency-score) -1)
            ((< a-urgency-score b-urgency-score) 1)))))

(defun org-urgency/add-custom-agenda-command (key)
  "Add a custom agenda command to list the TODOs sorted by urgency."
  (add-to-list 'org-agenda-custom-commands
               (list key "All TODOs sorted by urgency"
                     'alltodo ""
                     '((org-agenda-cmp-user-defined 'org-urgency/cmp-urgency-scores)
                       (org-agenda-sorting-strategy '(user-defined-up))))))

;;;;; Misc

(defun org-urgency/describe-as-org-table (id)
  "Return an Org table describing the score of the TODO entry with the given ID."
  (save-excursion
    (org-id-goto id)
    (concat
     "| Property | Coefficient | Value | Score |\n"
     "|----\n"
     (org-urgency/describe-priority-score)
     (org-urgency/describe-deadline-score)
     (org-urgency/describe-activity-score)
     (org-urgency/describe-age-score)
     (org-urgency/describe-tags-score)
     (org-urgency/describe-blocking-score)
     "|----\n"
     (format-message "| Total | | | %.2f |\n" (org-urgency/build-urgency-score)))))

(provide 'org-urgency)
