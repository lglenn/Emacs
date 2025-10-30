;;; walmart-utils-demo.el --- Demonstration of Walmart week utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; This file demonstrates the usage of the Walmart fiscal calendar utilities.
;; Evaluate this buffer to see the functions in action.

;;; Code:

(require 'my-walmart-utils)

(message "\n=== Walmart Fiscal Calendar Demonstration ===\n")

;; Current date
(let* ((result (walmart-week))
       (week (car result))
       (fy (cdr result)))
  (message "Today is: %s" (format-time-string "%A, %B %d, %Y"))
  (message "Current Walmart Week: %d" week)
  (message "Current Fiscal Year: %d" fy)
  (message "Formatted: WW%02d-FY%d\n" week fy))

;; Test dates around February (Week 1 boundaries)
(message "=== Week 1 Boundaries (February) ===")
(dolist (date '((2024 1 27)   ; Saturday before Feb 1
                (2024 2 1)    ; February 1
                (2024 2 3)    ; Saturday after Feb 1
                (2024 2 9)    ; Week 2
                (2025 1 25)   ; Saturday before Feb 1, 2025
                (2025 2 1)    ; February 1, 2025
                (2025 2 1)))  ; Saturday after Feb 1, 2025
  (let* ((result (walmart-week date))
         (week (car result))
         (fy (cdr result))
         (date-str (format "%04d-%02d-%02d (%s)"
                          (car date) (cadr date) (caddr date)
                          (format-time-string "%A"
                                            (encode-time 0 0 0
                                                        (caddr date)
                                                        (cadr date)
                                                        (car date))))))
    (message "%s -> Week %2d, FY%d" date-str week fy)))

;; Test dates around calendar year boundaries
(message "\n=== Calendar Year Boundaries (December/January) ===")
(dolist (date '((2024 12 28)  ; Late December
                (2024 12 31)  ; New Year's Eve
                (2025 1 1)    ; New Year's Day
                (2025 1 4)    ; Early January
                (2025 1 11))) ; Mid January
  (let* ((result (walmart-week date))
         (week (car result))
         (fy (cdr result))
         (date-str (format "%04d-%02d-%02d (%s)"
                          (car date) (cadr date) (caddr date)
                          (format-time-string "%A"
                                            (encode-time 0 0 0
                                                        (caddr date)
                                                        (cadr date)
                                                        (car date))))))
    (message "%s -> Week %2d, FY%d" date-str week fy)))

;; Test Saturday transitions (new week starts on Saturday)
(message "\n=== Saturday Week Transitions ===")
(let ((base-date '(2024 6 7))) ; June 7, 2024 is a Friday
  (dotimes (i 3)
    (let* ((day (+ (caddr base-date) i))
           (date (list (car base-date) (cadr base-date) day))
           (result (walmart-week date))
           (week (car result))
           (fy (cdr result))
           (date-str (format "%04d-%02d-%02d (%s)"
                            (car date) (cadr date) (caddr date)
                            (format-time-string "%A"
                                              (encode-time 0 0 0
                                                          (caddr date)
                                                          (cadr date)
                                                          (car date))))))
      (message "%s -> Week %2d, FY%d" date-str week fy))))

;; Using convenience functions
(message "\n=== Convenience Functions ===")
(message "Week number only: %d" (walmart-week-number))
(message "Fiscal year only: %d" (walmart-fiscal-year))
(message "Formatted string: WW%02d-FY%d"
         (walmart-week-number)
         (walmart-fiscal-year))

;; Calculating for specific important dates
(message "\n=== Specific Dates ===")
(let ((important-dates '((2024 7 4)     ; Independence Day 2024
                        (2024 11 28)    ; Thanksgiving 2024
                        (2024 12 25)    ; Christmas 2024
                        (2025 1 1)      ; New Year 2025
                        (2025 11 1))))  ; November 1, 2025 (should be FY2026)
  (dolist (date important-dates)
    (let* ((result (walmart-week date))
           (week (car result))
           (fy (cdr result))
           (date-str (format "%04d-%02d-%02d" (car date) (cadr date) (caddr date))))
      (message "%s -> WW%02d-FY%d" date-str week fy))))

(message "\n=== End of Demonstration ===\n")
(message "To use these functions:")
(message "  M-x walmart-week             - Show current week interactively")
(message "  (walmart-week)               - Get current week programmatically")
(message "  (walmart-week '(2024 6 15))  - Get week for specific date")
(message "  (walmart-week-number)        - Get just the week number")
(message "  (walmart-fiscal-year)        - Get just the fiscal year")

(provide 'walmart-utils-demo)
;;; walmart-utils-demo.el ends here
