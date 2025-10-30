# Walmart Fiscal Calendar Utilities

Elisp functions for calculating Walmart week numbers according to Walmart's fiscal calendar system.

## Overview

Walmart uses a specialized fiscal calendar where:
- **Week boundaries**: Each week runs from Saturday to Friday
- **Week numbering**: Weeks are numbered 1-52 (or 1-53 in years with 53 weeks)
- **Fiscal year start**: Week 1 begins on the Saturday closest to February 1st
- **Fiscal year naming**: The fiscal year is named after the calendar year in which it ends. For example, FY2026 starts around February 2025 and ends in January 2026

## Functions

### `walmart-week (&optional date)`

Main function that returns the Walmart week number and fiscal year for a given date.

**Parameters:**
- `date` (optional): A list in the format `(YEAR MONTH DAY)`. If omitted, uses the current date.

**Returns:**
- A cons cell `(WEEK-NUMBER . FISCAL-YEAR)`

**Examples:**
```elisp
;; Get current Walmart week
(walmart-week)
;; => (19 . 2024)

;; Get Walmart week for a specific date
(walmart-week '(2024 6 15))
;; => (19 . 2024)

;; Interactive use - displays in minibuffer
M-x walmart-week
;; Displays: "Walmart Week 19, FY2024"
```

### `walmart-week-number (&optional date)`

Convenience function that returns only the week number as an integer.

**Examples:**
```elisp
(walmart-week-number)
;; => 19

(walmart-week-number '(2024 2 3))
;; => 1
```

### `walmart-fiscal-year (&optional date)`

Convenience function that returns only the fiscal year as an integer.

**Examples:**
```elisp
(walmart-fiscal-year)
;; => 2024

(walmart-fiscal-year '(2024 12 31))
;; => 2025
```

## Usage in Org Mode

You can use these functions in Org mode for various purposes:

### In Org Babel code blocks:
```org
#+begin_src elisp
(walmart-week)
#+end_src

#+RESULTS:
: (19 . 2024)
```

### In Org capture templates:
```elisp
(setq org-capture-templates
      '(("w" "Work Item" entry
         (file+headline "work.org" "Items")
         "* TODO %?\n:PROPERTIES:\n:WalmartWeek: %(walmart-week-number)\n:FiscalYear: %(walmart-fiscal-year)\n:END:\n")))
```

### In header properties:
```elisp
;; Add to capture template or use interactively
(format "WW%02d-FY%d" (walmart-week-number) (walmart-fiscal-year))
;; => "WW19-FY2024"
```

## Testing

The package includes comprehensive tests. To run them:

```elisp
;; Load the test file
(load-file "~/.doom.d/site-lisp/my-walmart-utils-test.el")

;; Run all tests
M-x walmart-week-run-tests

;; Or run tests programmatically
(ert "^walmart-week-")

;; Print example calculations
M-x walmart-week-print-examples
```

## Edge Cases

The implementation correctly handles:

1. **Week 1 spanning calendar years**: When the Saturday closest to February 1st falls in late January or early February, Week 1 may span two calendar years. The fiscal year is always named after the year in which it ends.

2. **53-week years**: Some fiscal years have 53 weeks instead of 52.

3. **Saturday transitions**: Each Saturday starts a new Walmart week.

4. **Fiscal year boundaries**: Dates in November-January are in the fiscal year that ends in the following calendar year. For example, November 1, 2025 is in FY2026.

## Implementation Details

The implementation uses three internal helper functions:

- `walmart-week--saturday-closest-to-feb-1`: Finds the Saturday closest to February 1st
- `walmart-week--fiscal-year-for-week-1`: Returns the fiscal year as (Week 1 start year + 1), since fiscal years are named after the year in which they end
- `walmart-week--find-week-1-start`: Finds the appropriate Week 1 start for any given date

## Installation

The module is automatically loaded in your `config.el`:

```elisp
;;; Walmart fiscal calendar utilities
(load! "site-lisp/my-walmart-utils")
```

After adding or modifying this configuration, reload Emacs or evaluate the configuration.

## License

Part of your personal Emacs configuration.
