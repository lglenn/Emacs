;;; my-walmart-utils.el --- Walmart fiscal calendar utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for working with Walmart's fiscal calendar system.
;; Walmart weeks run Saturday to Friday, with Week 1 starting on the
;; Saturday on or before February 1st. The fiscal year is named after
;; the calendar year in which it ends (e.g., FY2026 ends in Jan 2026).

;;; Code:

(defun walmart-week--saturday-on-or-before-feb-1 (year)
  "Find the day-number of the Saturday on or before February 1st in YEAR.
If February 1st is a Saturday, returns Feb 1. Otherwise returns the 
Saturday immediately before Feb 1."
  (let* ((feb-1-days (time-to-days (encode-time 0 0 0 1 2 year)))
         ;; Day of week: 0=Sunday, 6=Saturday
         (feb-1-dow (mod feb-1-days 7))
         ;; Days back to the previous Saturday (0 if Feb 1 is Saturday)
         (days-back (mod (- feb-1-dow 6) 7)))
    (- feb-1-days days-back)))

(defun walmart-week (&optional date)
  "Return the Walmart week number for DATE (or current date if nil).
Returns a cons cell (WEEK-NUMBER . FISCAL-YEAR).

A Walmart week runs from Saturday to Friday. Week 1 begins on the
Saturday on or before February 1st. The fiscal year is named after the
calendar year in which it ends (approximately one year after Week 1 starts)."
  (interactive)
  (let* ((target-time (if date
                          (encode-time 0 0 0 (nth 2 date) (nth 1 date) (nth 0 date))
                        (current-time)))
         (target-days (time-to-days target-time))
         (target-year (nth 5 (decode-time target-time)))
         ;; Try possible years for Week 1
         (candidate-years (list target-year (1- target-year)))
         best-week-1-days
         best-fiscal-year)
    ;; Find the Week 1 that contains or precedes the target date
    (dolist (year candidate-years)
      (let ((week-1-days (walmart-week--saturday-on-or-before-feb-1 year)))
        (when (>= target-days week-1-days)
          (when (or (null best-week-1-days)
                    (> week-1-days best-week-1-days))
            (setq best-week-1-days week-1-days
                  best-fiscal-year (1+ year))))))
    ;; Calculate week number
    (let* ((days-diff (- target-days best-week-1-days))
           (week-number (1+ (/ days-diff 7))))
      (when (called-interactively-p 'any)
        (message "Walmart Week %d, FY%d" week-number best-fiscal-year))
      (cons week-number best-fiscal-year))))

(defun walmart-week-number (&optional date)
  "Return just the Walmart week number for DATE (or current date if nil).
This is a convenience function that returns only the week number as an integer."
  (car (walmart-week date)))

(defun walmart-fiscal-year (&optional date)
  "Return the Walmart fiscal year for DATE (or current date if nil).
This is a convenience function that returns only the fiscal year as an integer."
  (cdr (walmart-week date)))

(defun walmart-week-show-current ()
  "Display the current Walmart week with 'It's' prefix."
  (interactive)
  (let* ((result (walmart-week))
         (week (car result))
         (fy (cdr result)))
    (message "It's Walmart Week %d, FY%d" week fy)))

(defun walmart-week-for-date ()
  "Prompt for a date and display the Walmart week for that date.
Date should be entered in yyyy-mm-dd format."
  (interactive)
  (let* ((date-str (read-string "Enter date (yyyy-mm-dd): "))
         (date-parts (split-string date-str "-"))
         (year (string-to-number (nth 0 date-parts)))
         (month (string-to-number (nth 1 date-parts)))
         (day (string-to-number (nth 2 date-parts)))
         (date-list (list year month day))
         (result (walmart-week date-list))
         (week (car result))
         (fy (cdr result)))
    (message "Date %s: Walmart Week %d, FY%d" date-str week fy)))

(provide 'my-walmart-utils)
;;; my-walmart-utils.el ends here
