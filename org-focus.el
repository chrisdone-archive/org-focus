;;; org-focus.el --- Focused time tracking for org-mode

;; Copyright (c) 2016 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; It adds an extra field to the SCHEDULED property in org-mode:
;;
;; * TODO Test
;;   SCHEDULED: <2016-05-01 Sun> <1 hour>
;;
;; You schedule an item in for a day and then set a plan of how long
;; you expect to spend on it.
;;
;; We add an extra property specific to this workflow called ESTIMATE,
;; which pertains the the task overall.
;;
;; * TODO Test
;;   ESTIMATE: <2 hours>
;;   SCHEDULED: <2016-05-01 Sun> <1 hour>
;;   SCHEDULED: <2016-05-02 Mon> <1 hour>
;;
;; This allows us to have a task that spans many days, with an overall
;; estimate. Then each day that you're working on the task, you can
;; put in an amount of time you plan to spend on it (either hopeful or
;; a pestimate, as you prefer).
;;
;; (define-key org-mode-map (kbd "C-c C-e") 'org-focus-estimate)
;; (define-key org-mode-map (kbd "C-c C-s") 'org-focus-schedule)

;;; Code:

(require 'cl-lib)
(require 'font-lock)
(require 'org)
(require 'org-clock)

(font-lock-add-keywords
 'org-mode
 '(("\\<ESTIMATE:" . '(0 'org-special-keyword t))))

(defconst org-focus-estimate-regex
  "<\\([0-9]+\\) hr>"
  "Regex matching the estimate.")

(defconst org-focus-clock-regex
  "^[ ]+CLOCK: \\[\\([0-9]+-[0-9]+-[0-9]+ [A-Za-z]+ [0-9]+:[0-9]+\\).+ =>  \\([0-9]+:[0-9]+\\)$"
  "Regex matching the CLOCK tracking.")

(defconst org-focus-clock-current-regex
  "^[ ]+CLOCK: \\[\\([0-9]+-[0-9]+-[0-9]+ [A-Za-z]+ [0-9]+:[0-9]+\\)\\]$"
  "Regex matching a current CLOCK in.")

(defvar org-focus-mode-current-time
  nil
  "Current week in the buffer.")
(make-variable-buffer-local 'org-focus-mode-current-time)

(define-derived-mode org-focus-mode
  fundamental-mode "Org-Focus"
  "Major mode for viewing your focused tasks by week.
 \\{hypertext-mode-map}")

(define-key org-focus-mode-map (kbd "q") 'bury-buffer)
(define-key org-focus-mode-map (kbd "f") 'org-focus-forward)
(define-key org-focus-mode-map (kbd "b") 'org-focus-backward)
(define-key org-focus-mode-map (kbd "B") 'org-focus-bump)
(define-key org-focus-mode-map (kbd "g") 'org-focus-current)
(define-key org-focus-mode-map (kbd "RET") 'org-focus-goto)
(define-key org-focus-mode-map (kbd "C-c C-x C-i") 'org-focus-clock-in)
(define-key org-focus-mode-map (kbd "C-c C-s") 'org-focus-jump-and-schedule)
(define-key org-focus-mode-map (kbd "C-x C-s") 'org-save-all-org-buffers)
(define-key org-focus-mode-map (kbd "C-c C-x C-o") 'org-focus-clock-out)
(define-key org-focus-mode-map (kbd "r") 'org-focus-retroactive)

(defun org-focus-bump ()
  "Bump current item to the next day."
  (interactive)
  (save-window-excursion
    (org-focus-goto)
    (save-excursion
      (org-back-to-heading t)
      (end-of-line)
      (let ((date
             (plist-get (car (plist-get (org-focus-current-item) :schedule)) :date)))
        (org-schedule t (time-add date (seconds-to-time (* 60 60 24)))))))
  (org-focus-current))

(defun org-focus-retroactive ()
  "Retroactively add entries that you forgot to clock."
  (interactive)
  (let* ((this-time (get-text-property (point) 'time))
         (file (ido-completing-read "Pick file: "(org-agenda-files nil)))
         (items (with-current-buffer (find-file-noselect file t nil nil)
                  (org-focus-buffer-items)))
         (item-title (ido-completing-read "Item: "
                                          (mapcar (lambda (item) (plist-get item :title))
                                                  items)))
         (item (car (remove-if-not (lambda (item) (string= item-title (plist-get item :title)))
                                   items)))
         (amount (string-to-number (read-from-minibuffer "Hours clocked: " "1"))))
    (with-current-buffer (find-file-noselect file t nil nil)
      (goto-char (plist-get item :point))
      (forward-line 1)
      (insert (format "  CLOCK: [%s]--[%s] => %2d:00\n"
                      (format-time-string "%Y-%m-%d %a %H:%M" this-time)
                      (format-time-string "%Y-%m-%d %a %H:%M" this-time)
                      amount))
      (save-buffer))
    (org-focus-current)))

(defun org-focus-goto ()
  "Go to the item."
  (interactive)
  (let ((item (save-excursion (goto-char (line-beginning-position))
                              (get-text-property (point) 'org-focus-item))))
    (switch-to-buffer (plist-get item :buffer))
    (goto-char (plist-get item :point))))


(defun org-focus-jump-and-schedule (&optional arg)
  "Jump and schedule."
  (interactive "P")
  (save-window-excursion
    (org-focus-goto)
    (org-focus-schedule arg))
  (org-focus-current))

(defun org-focus-clock-in ()
  "Go to the item and clock in."
  (interactive)
  (save-window-excursion
    (org-focus-goto)
    (org-clock-in))
  (org-focus-current))

(defun org-focus-clock-out ()
  "Go to the item and clock out."
  (interactive)
  (save-window-excursion
    (org-focus-goto)
    (org-clock-out))
  (org-focus-current))

(defun org-focus-mode-current-time ()
  "Get the current buffer time."
  (or org-focus-mode-current-time
      (setq org-focus-mode-current-time
            (current-time))))

(defun org-focus-current ()
  "Go forward by one week."
  (interactive)
  (org-focus-update
   (org-focus-mode-current-time)))

(defun org-focus-forward ()
  "Go forward by one week."
  (interactive)
  (org-focus-update
   (setq org-focus-mode-current-time
         (time-add (org-focus-mode-current-time)
                   (seconds-to-time
                    (* (* 24 60 60)
                       7))))))

(defun org-focus-backward ()
  "Go forward by one week."
  (interactive)
  (org-focus-update
   (setq org-focus-mode-current-time
         (time-add (org-focus-mode-current-time)
                   (seconds-to-time
                    (- (* (* 24 60 60)
                          7)))))))

(defun org-focus ()
  "Bring up a buffer and display the current week."
  (interactive)
  (with-current-buffer
      (or (when (eq major-mode 'org-focus-mode)
            (current-buffer))
          (let ((buffer (switch-to-buffer-other-window
                         (get-buffer-create "*Org Focus*"))))
            (org-focus-mode)
            (setq buffer-read-only t)
            buffer))
    (org-focus-update)))

(defun org-focus-update (&optional base-time)
  "Update the focus buffer."
  (interactive)
  (when (eq major-mode 'org-focus-mode)
    (let ((inhibit-read-only t))
      (let ((items (org-focus-all-items))
            (base-time (or base-time (current-time)))
            (base-day (string-to-number (format-time-string "%u" base-time)))
            (unfinished-items (list))
            (line (line-number-at-pos))
            (total-week 0))
        (erase-buffer)
        (remove-overlays)
        (insert
         (propertize
          (format-time-string "Clock-week-agenda (W%W):\n" base-time)
          'face 'org-agenda-structure))
        (cl-loop for i from 1 to 7
                 do (setq total-week
                          (+ total-week
                             (org-focus-render-day base-day
                                                   i
                                                   (org-focus-add-day base-time i)
                                                   items
                                                   unfinished-items))))
        (save-excursion
          (goto-char (point-min))
          (forward-line 1)
          (insert (propertize "Total "
                              'face 'org-agenda-structure))
          (insert (propertize (org-focus-format-hours total-week)
                              'face (if (< total-week org-focus-per-week)
                                        'font-lock-error
                                      'org-agenda-structure)))
          (insert (propertize ", remaining "
                              'face 'org-agenda-structure))
          (insert (propertize (org-focus-format-hours (- org-focus-per-week total-week))
                              'face (if (< total-week org-focus-per-week)
                                        'font-lock-error
                                      'org-agenda-structure)))
          (insert "\n"))
        (goto-char (point-min))
        (forward-line (1- line))))))

(defvar org-focus-per-week 40
  "Ideal hours per week.")

(defun org-focus-render-day (base-day i this-time items unfinished-items)
  "Render a day of the week planner."
  (let ((title (concat
                (format "%-11s" (format-time-string "%A" this-time))
                (format-time-string "%d %B %Y" this-time)
                (if (= i 1)
                    (format-time-string " W%W" this-time)
                  "")
                "\n")))
    (insert (propertize title
                        'face (if (= i base-day)
                                  'org-agenda-date-today
                                'org-agenda-date)
                        'time this-time))
    (let ((total-planned 0)
          (total-done 0)
          (todo-count 0)
          (total-unplanned 0))
      (cl-loop for item in items
               do (let* ((status (plist-get item :status))
                         (clocks (plist-get item :clocks))
                         (this-is-current nil)
                         (hours (apply '+
                                       (mapcar (lambda (clock)
                                                 (let ((date (plist-get clock :date))
                                                       (hours (plist-get clock :hours))
                                                       (current (plist-get clock :current)))
                                                   (if (org-focus-day= this-time date)
                                                       (progn (when current
                                                                (setq this-is-current t))
                                                              hours)
                                                     0)))
                                               clocks)))
                         (incomplete (and (member status org-todo-keywords-for-agenda)
                                          (not (member status org-done-keywords-for-agenda))))
                         (holdover nil))
                    (when (or (cl-remove-if-not
                               (lambda (entry)
                                 (let ((date (plist-get entry :date))
                                       (hours (plist-get entry :hours)))
                                   (when (org-focus-day= date this-time)
                                     (setq total-planned
                                           (+ total-planned
                                              (or hours 0)))
                                     t)))
                               (plist-get item :schedule))
                              (cl-remove-if-not
                               (lambda (entry)
                                 (let ((date (plist-get entry :date)))
                                   (org-focus-day= date this-time)))
                               (plist-get item :clocks))
                              (when (and incomplete
                                         (= i base-day)
                                         (= hours 0)
                                         (org-focus-day= this-time (current-time))
                                         (not (cl-remove-if-not
                                               (lambda (entry)
                                                 (let ((date (plist-get entry :date)))
                                                   (org-focus-day< this-time date)))
                                               (plist-get item :schedule))))
                                (setq holdover t)))
                      (let* ((planned-hours
                              (or (plist-get (car (cl-remove-if-not
                                                   (lambda (entry)
                                                     (let ((date (plist-get entry :date)))
                                                       (org-focus-day= date this-time)))
                                                   (plist-get item :schedule)))
                                             :hours)
                                  0)))
                        (setq total-unplanned
                              (+ total-unplanned
                                 (if (and (= 0 planned-hours)
                                          (> hours 0))
                                     hours
                                   0)))
                        (setq total-done (+ total-done hours))
                        (setq todo-count
                              (+ todo-count
                                 (cond ((null status)
                                        0)
                                       ((not (member status org-done-keywords-for-agenda))
                                        1)
                                       (t 0))))
                        (org-focus-render-item
                         base-day i
                         this-time item planned-hours hours
                         this-is-current
                         holdover)))))
      (org-focus-render-day-totals base-day i
                                   total-done total-planned total-unplanned
                                   todo-count)
      total-done)))

(defun org-focus-render-day-totals (base-day i done planned unplanned todos)
  "Render the totals for a day."
  (unless (and (= done 0) (= planned 0))
    (let ((remaining (if (and (= base-day i))
                         (format (if (> todos 0)
                                     "(%s planned, %s unplanned, %s remaining today)"
                                   "")
                                 (org-focus-format-hours planned)
                                 (org-focus-format-hours unplanned)
                                 ;; (if (> planned done)
                                 ;;     (- (+ planned unplanned) done)
                                 ;;   0)
                                 (org-focus-format-hours (max 0 (- (/ org-focus-per-week 5) done))))
                       (if (> unplanned 0)
                           (format "(%s unplanned)" (org-focus-format-hours unplanned))
                         ""))))
      (insert (propertize (format "  %-10.10s  "
                                  "Total")
                          'face 'org-agenda-structure))
      (insert (propertize (format "%s"
                                  (org-focus-format-hours done))
                          'face (if (< done (/ org-focus-per-week 5))
                                    'font-lock-error
                                  'org-agenda-structure)))
      (insert (propertize (format " / %s %s\n"
                                  (org-focus-format-hours (+ planned unplanned))
                                  remaining)
                          'face 'org-agenda-structure)))))

(defun org-focus-render-item (base-day i this-time item planned-hours hours current holdover)
  "Render an item for a day of the week."
  (let* ((title (plist-get item :title))
         (status (plist-get item :status))
         (category (plist-get item :category))
         (unscheduled (= 0 planned-hours))
         (is-done (member status org-done-keywords-for-agenda))
         (face (cond
                (is-done
                 'org-agenda-done)
                (unscheduled
                 'org-agenda-done)
                ((= base-day i)
                 'org-scheduled-today)
                (t 'org-scheduled)))
         (planned (if ( = 0 planned-hours)
                      (propertize (org-focus-format-hours  hours)
                                  'face 'org-agenda-structure)
                    (if planned-hours
                        (propertize (org-focus-format-hours  planned-hours) 'face face)
                      (propertize (org-focus-format-hours hours)
                                  'face 'org-agenda-structure)))))
    (let ((start (point)))
      (insert (propertize (format "  %-10.10s  %s / "
                                  category
                                  (org-focus-format-hours hours))
                          'face face
                          'org-focus-item item)
              planned
              "  "
              (format "%-10.10s"
                      (if (or (> planned-hours 0)
                              is-done)
                          (propertize (format "%-10.10s"
                                              (if (and (< i base-day)
                                                       (time-less-p this-time (current-time)))
                                                  (if is-done
                                                      "DONE"
                                                    "WORKED ON")
                                                (if status
                                                    (concat status " ")
                                                  "GENERAL ")))
                                      'face
                                      (if is-done
                                          'org-done
                                        (if status
                                            'org-todo
                                          'org-agenda-structure)))
                        (propertize "UNPLANNED" 'face 'org-agenda-structure)))
              (propertize (format "%-40.40s" (org-focus-limit-string 40 title))
                          'face face)
              (if holdover
                  (propertize " [holdover]" 'face 'org-agenda-structure)
                "")
              "\n")
      (when current
        (let ((o (make-overlay start (point))))
          (overlay-put o 'face 'org-agenda-clocking))))))

(defun org-focus-limit-string (n string)
  "Limit STRING's length to N."
  (if (> n (length string))
      string
    (concat (substring string 0 (1- n)) "…")))

(defun org-focus-schedule (&optional arg)
  "Schedule an item with a planned hours. This always adds a new scheduled date."
  (interactive "P")
  (save-excursion
    (org-back-to-heading t)
    (unless arg
      (when (search-forward-regexp org-todo-line-regexp nil t 1)
        (let* ((depth (length (match-string 1))))
          (forward-line 1)
          (insert "\n")
          (forward-line -1)
          (insert (format "SCHEDULED: <%s>" (format-time-string "%Y-%m-%d %a" (current-time))))
          (goto-char (line-beginning-position))
          (indent-to (1+ depth)))))
    (org-schedule nil)
    (unless arg
      (goto-char (line-end-position))
      (insert (format " <%d hr>" (org-focus-read-estimate))))))

(defun org-focus-estimate ()
  "Insert a totalling, over all time, estimate for the task."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (when (search-forward-regexp org-todo-line-regexp nil t 1)
      (let* ((depth (length (match-string 1))))
        (forward-line 1)
        (let ((hours (org-focus-read-estimate)))
          (insert "\n")
          (forward-line -1)
          (insert (format "ESTIMATE: <%d hr>" hours))
          (goto-char (line-beginning-position))
          (indent-to (1+ depth)))))))

(defun org-focus-read-estimate ()
  "Read in an estimate."
  (string-to-number (read-from-minibuffer "Estimate in hours: " "1")))

(defun org-focus-all-items ()
  "Collect all clocked items from all org buffers (via
`org-focus-buffer-items') in a flat list."
  (let ((all-items nil))
    (cl-loop for file in (cl-remove-duplicates (mapcar (lambda (o) (expand-file-name o "/")) (org-agenda-files nil t)) :test 'string=)
             do (with-current-buffer (find-file-noselect file t nil nil)
                  (setq all-items
                        (append all-items
                                (org-focus-buffer-items)))))
    all-items))

(defun org-focus-buffer-items ()
  "The items in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     while (search-forward-regexp org-todo-line-regexp nil t 1)
     collect (org-focus-current-item))))

(defun org-focus-current-item ()
  (let* ((item-point (line-beginning-position))
         (item-buffer (current-buffer))
         (status (match-string 2))
         (title (match-string 3))
         (boundary (save-excursion
                     (if (search-forward-regexp org-todo-line-regexp nil t 1)
                         (max 0 (1- (line-beginning-position)))
                       (point-max))))
         (category (when title
                     (file-name-nondirectory (buffer-file-name))))
         (scheduled-dates (org-focus-item-schedule boundary))
         (clocks (org-focus-item-clocks boundary)))
    (list :status (when status
                    (substring-no-properties status))
          :title (when title
                   (substring-no-properties title))
          :category category
          :schedule scheduled-dates
          :clocks clocks
          :point item-point
          :buffer item-buffer)))

(defun org-focus-item-schedule (boundary)
  "Get all scheduled dates and planned hours for an item."
  (when (> boundary (point))
    (save-excursion
      (cl-loop
       while (search-forward-regexp
              org-scheduled-time-regexp
              boundary t 1)
       collect (let ((date (org-focus-parse-time
                            (substring-no-properties (match-string 1))))
                     (hours (when (search-forward-regexp
                                   (concat " " org-focus-estimate-regex)
                                   boundary t 1)
                              (string-to-number (match-string 1)))))
                 (list :date date
                       :hours hours))))))

(defun org-focus-parse-time (string)
  "Parse the time STRING."
  (save-excursion
    (apply 'encode-time (org-parse-time-string string))))

(defun org-focus-item-clocks (boundary)
  "Get all clocks for the task."
  (save-excursion
    (cl-loop
     while (or (search-forward-regexp org-focus-clock-current-regex boundary t 1)
               (search-forward-regexp org-focus-clock-regex boundary t 1))
     collect (let (;; (year (string-to-number (match-string 1)))
                   ;; (month (string-to-number (match-string 2)))
                   ;; (day (string-to-number (match-string 3)))
                   (date-time (match-string 1))
                   (current (not (match-string 2)))
                   (hours-stamp (match-string 2))
                   (hours (let ((string (match-string 2)))
                            (if string
                                (org-focus-parse-hours string)
                              (org-focus-current-clock)))))
               (list :date (org-focus-parse-time
                            date-time)
                     :hours hours
                     :hours-stamp hours-stamp
                     :current current)))))

(defun org-focus-current-clock ()
  "Get the clocked time for the current item, does not include
  previous clocking intervals."
  (if (and org-clock-start-time
           (when (stringp org-clock-start-time)
             (not (string= "" org-clock-start-time))))
      (/ (/
          (- (org-float-time)
             (org-float-time org-clock-start-time))
          60)
         60)
    0))

(defun org-focus-date->week (base-time)
  "Given a BASE-TIME, return the day of the week."
  (string-to-number (format-time-string "%u" base-time)))

(defun org-focus-add-day (base-time target-day)
  "Make a new time by taking BASE-TIME and adding/subtracting days
from it to get TARGET-DAY of week."
  (let* ((base-day (string-to-number (format-time-string "%u" base-time)))
         (new-time (time-add base-time
                             (seconds-to-time
                              (* (* 24 60 60)
                                 (- target-day base-day))))))
    new-time))

(defun org-focus-parse-hours (string)
  "Parse STRING hh:mm into a number of hours."
  (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" string)
    (let ((hours (match-string 1 string))
          (minutes (match-string 2 string)))
      (string-to-number
       (format "%0.2f"
               (+ (string-to-number hours)
                  (/ (+ 0.0 (string-to-number minutes)) 60)))))))

(defun org-focus-format-hours (hours)
  "Print a decimal number of hours into base 60 e.g. 2.5 hours is 2:30."
  (format "%d:%02d" (floor hours) (* 60 (- hours (floor hours)))))

(defun org-focus-day= (x y)
  "Are two dates equal by the day?"
  (string= (format-time-string "%Y%m%d" x)
           (format-time-string "%Y%m%d" y)))

(defun org-focus-day< (x y)
  "x < y on date"
  (string< (format-time-string "%Y%m%d" x)
           (format-time-string "%Y%m%d" y)))

(provide 'org-focus)
