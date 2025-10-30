;;; my-walmart-utils-test.el --- Tests for Walmart fiscal calendar -*- lexical-binding: t; -*-

;;; Commentary:
;; Test cases for the Walmart week calculation functions.

;;; Code:

(require 'my-walmart-utils)
(require 'ert)

(ert-deftest walmart-week-test-basic ()
  "Test basic Walmart week calculation."
  ;; Test a date in the middle of the fiscal year
  (let* ((result (walmart-week '(2024 6 15)))
         (week (car result))
         (fiscal-year (cdr result)))
    (should (numberp week))
    (should (>= week 1))
    (should (<= week 53))
    (should (numberp fiscal-year))))

(ert-deftest walmart-week-test-week-1-boundaries ()
  "Test Week 1 calculation for various years."
  ;; 2024: Feb 1 is Thursday, so closest Saturday is Feb 3 (2 days after)
  ;; or Jan 27 (5 days before). Feb 3 is closer.
  (let* ((feb-3-2024 (walmart-week '(2024 2 3)))
         (feb-2-2024 (walmart-week '(2024 2 2))))
    ;; Feb 3, 2024 should be Week 1 of FY2025 (since FY ends in 2025)
    (should (= (car feb-3-2024) 1))
    (should (= (cdr feb-3-2024) 2025))
    ;; Feb 2 should be the last week of FY2024
    (should (> (car feb-2-2024) 50))
    (should (= (cdr feb-2-2024) 2024))))

(ert-deftest walmart-week-test-year-span ()
  "Test weeks that span calendar years."
  ;; Test dates around the new year
  (let* ((dec-31 (walmart-week '(2024 12 31)))
         (jan-1 (walmart-week '(2025 1 1))))
    ;; Both should be in the same Walmart week if they're within the same Sat-Fri period
    (should (numberp (car dec-31)))
    (should (numberp (car jan-1)))))

(ert-deftest walmart-week-number-test ()
  "Test the convenience function for getting just the week number."
  (let ((week-num (walmart-week-number '(2024 6 15))))
    (should (numberp week-num))
    (should (>= week-num 1))
    (should (<= week-num 53))))

(ert-deftest walmart-fiscal-year-test ()
  "Test the convenience function for getting just the fiscal year."
  (let ((fy (walmart-fiscal-year '(2024 6 15))))
    (should (numberp fy))
    (should (>= fy 2020))
    (should (<= fy 2030))))

(ert-deftest walmart-week-test-consistency ()
  "Test that consecutive days in the same week return the same week number."
  (let ((week-mon (walmart-week '(2024 6 10))) ; Monday
        (week-tue (walmart-week '(2024 6 11))) ; Tuesday
        (week-wed (walmart-week '(2024 6 12)))) ; Wednesday
    ;; If these are in the same week, they should have the same week number
    ;; (assuming June 10-12, 2024 are in the same Walmart week)
    (should (= (car week-mon) (car week-tue)))
    (should (= (car week-tue) (car week-wed)))))

(ert-deftest walmart-week-test-saturday-transitions ()
  "Test that Saturday starts a new week."
  ;; June 8, 2024 is a Saturday
  (let ((friday (walmart-week '(2024 6 7)))
        (saturday (walmart-week '(2024 6 8))))
    ;; Saturday should start a new week (one more than Friday)
    (should (= (car saturday) (1+ (car friday))))))

(ert-deftest walmart-week-test-fiscal-year-naming ()
  "Test that fiscal years are named after the year in which they end."
  ;; Nov 1, 2025 (Saturday) should be Week 40 of FY2026
  (let ((nov-1-2025 (walmart-week '(2025 11 1))))
    (should (= (car nov-1-2025) 40))
    (should (= (cdr nov-1-2025) 2026)))
  ;; Feb 3, 2025 (Saturday, Week 1 start) should be Week 1 of FY2026
  (let ((feb-3-2025 (walmart-week '(2025 2 3))))
    (should (= (car feb-3-2025) 1))
    (should (= (cdr feb-3-2025) 2026))))

;; Interactive test function
(defun walmart-week-run-tests ()
  "Run all Walmart week tests interactively."
  (interactive)
  (ert "^walmart-week-"))

;; Print some example calculations for manual verification
(defun walmart-week-print-examples ()
  "Print example Walmart week calculations for manual verification."
  (interactive)
  (let ((test-dates '((2024 2 1)   ; February 1, 2024
                     (2024 2 3)   ; February 3, 2024 (Saturday)
                     (2024 6 15)  ; Mid-year
                     (2024 12 31) ; End of calendar year
                     (2025 1 1)   ; Start of calendar year
                     (2025 1 31)  ; End of January
                     (2025 2 1)))) ; February 1, 2025
    (dolist (date test-dates)
      (let* ((result (walmart-week date))
             (week (car result))
             (fy (cdr result)))
        (message "%04d-%02d-%02d: Week %2d, FY%d"
                (car date) (cadr date) (caddr date) week fy)))))

(provide 'my-walmart-utils-test)
;;; my-walmart-utils-test.el ends here
