;;; my-walmart-utils.el --- Walmart fiscal calendar utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for working with Walmart's fiscal calendar system.
;; Walmart weeks run Saturday to Friday, with Week 1 starting on the
;; Saturday closest to February 1st. The fiscal year is named after
;; the calendar year in which it ends (e.g., FY2026 ends in Jan 2026).

;;; Code:

(defun walmart-week--saturday-closest-to-feb-1 (year)
  "Find the Saturday closest to February 1st in YEAR.
Returns a list (MONTH DAY YEAR) representing the date."
  (let* ((feb-1 (encode-time 0 0 0 1 2 year))
         (feb-1-dow (string-to-number (format-time-string "%w" feb-1)))
         ;; Days until next Saturday (0=Sunday, 6=Saturday)
         ;; m.
         (days-to-sat (mod (- 6 feb-1-dow) 7))
         ;; Days since last Saturday
         (days-from-sat (mod (+ feb-1-dow 1) 7)))
    ;; Find the closest Saturday
    (if (<= days-to-sat days-from-sat)
        ;; Next Saturday is closer or equal
        (let ((target-time (time-add feb-1 (days-to-time days-to-sat))))
          (decode-time target-time))
      ;; Previous Saturday is closer
      (let ((target-time (time-subtract feb-1 (days-to-time days-from-sat))))
        (decode-time target-time)))))

(defun walmart-week--fiscal-year-for-week-1 (week-1-time)
  "Determine the fiscal year for Week 1 starting at WEEK-1-TIME.
The fiscal year is named after the calendar year in which it ends.
Since fiscal years start around February and last ~52 weeks, they end
in the following calendar year."
  (let* ((decoded (decode-time week-1-time))
         (start-year (nth 5 decoded)))
    ;; The fiscal year ends in the following calendar year
    (1+ start-year)))

(defun walmart-week--find-week-1-start (target-time)
  "Find the start of Walmart Week 1 for the fiscal year containing TARGET-TIME.
Returns (WEEK-1-START FISCAL-YEAR) where WEEK-1-START is an encoded time."
  (let* ((decoded (decode-time target-time))
         (target-year (nth 5 decoded))
         (target-month (nth 4 decoded))
         ;; Try the current year first
         (candidate-years (list target-year (1- target-year) (1+ target-year)))
         (best-start nil)
         (best-fiscal-year nil))
    ;; Find the Week 1 start that puts target-time in a valid week
    (dolist (year candidate-years)
      (let* ((week-1-decoded (walmart-week--saturday-closest-to-feb-1 year))
             (week-1-start (encode-time 0 0 12
                                       (nth 3 week-1-decoded)
                                       (nth 4 week-1-decoded)
                                       (nth 5 week-1-decoded)))
             (fiscal-year (walmart-week--fiscal-year-for-week-1 week-1-start)))
        ;; Check if target-time is on or after this Week 1 start
        (when (or (time-less-p week-1-start target-time)
                  (equal (time-to-days week-1-start) (time-to-days target-time)))
          (when (or (null best-start)
                    (time-less-p best-start week-1-start))
            (setq best-start week-1-start
                  best-fiscal-year fiscal-year)))))
    ;; If target is before all candidates, use the earliest one
    (unless best-start
      (let* ((week-1-decoded (walmart-week--saturday-closest-to-feb-1 (car candidate-years)))
             (week-1-start (encode-time 0 0 12
                                       (nth 3 week-1-decoded)
                                       (nth 4 week-1-decoded)
                                       (nth 5 week-1-decoded))))
        (setq best-start week-1-start
              best-fiscal-year (walmart-week--fiscal-year-for-week-1 week-1-start))))
    (list best-start best-fiscal-year)))

(defun walmart-week (&optional date)
  "Return the Walmart week number for DATE (or current date if nil).
Returns a cons cell (WEEK-NUMBER . FISCAL-YEAR).

A Walmart week runs from Saturday to Friday. Week 1 begins on the
Saturday closest to February 1st. The fiscal year is named after the
calendar year in which it ends (approximately one year after Week 1 starts)."
  (interactive)
  (let* ((target-time (if date
                          (apply #'encode-time
                                (append '(0 0 12) (reverse date)))
                        (current-time)))
         (week-1-info (walmart-week--find-week-1-start target-time))
         (week-1-start (car week-1-info))
         (fiscal-year (cadr week-1-info))
         ;; Calculate days since Week 1 start
         ;; Round to nearest day to handle DST and floating-point issues
         (days-diff (round (/ (float-time (time-subtract target-time week-1-start))
                             86400)))
         ;; Calculate week number (1-based)
         (week-number (1+ (floor (/ days-diff 7.0)))))
    (when (called-interactively-p 'any)
      (message "Walmart Week %d, FY%d" week-number fiscal-year))
    (cons week-number fiscal-year)))

(defun walmart-week-number (&optional date)
  "Return just the Walmart week number for DATE (or current date if nil).
This is a convenience function that returns only the week number as an integer."
  (car (walmart-week date)))

(defun walmart-fiscal-year (&optional date)
  "Return the Walmart fiscal year for DATE (or current date if nil).
This is a convenience function that returns only the fiscal year as an integer."
  (cdr (walmart-week date)))

(provide 'my-walmart-utils)
;;; my-walmart-utils.el ends here
