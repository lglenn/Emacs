;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Larry Glenn"
      user-mail-address "lawrence.glenn@walmart.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Inconsolata" :size 14)
      doom-variable-pitch-font (font-spec :family "Gentium Book Basic" :size 14 :weight 'semi-light))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; Stolen from:
;; - https://github.com/daviwil/emacs-from-scratch/blob/1a13fcf0dd6afb41fce71bf93c5571931999fed8/init.el
;; - https://emacs.stackexchange.com/questions/62987/cannot-set-correct-size-for-variable-pitch-font-in-doom-emacs
;; Ensure that anything that should be fixed-pitch in Org files appears that way
(use-package! mixed-pitch
              :hook (org-mode . mixed-pitch-mode)
              :config
              (setq mixed-pitch-set-heigth t)
              (set-face-attribute 'variable-pitch nil)
              (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
              (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
              (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
              (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
              (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
              (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
              (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; Set location, because what good is a text editor if it doesn't know what
;; time sunrise and sunset are?
(setq calendar-latitude 40.7)
(setq calendar-longitude -74.0)
(setq calendar-location-name "New York, NY")

;; Don't need to know about Baha'i holidays
(setq holiday-bahai-holidays nil)

;; Don't add duplicate kills to the kill ring
(setq kill-do-not-save-duplicates t)

;; Dedupe command history
(setq history-delete-duplicates t)

;; Put a clock in the modeline
(display-time)
;; Locations for world clock
(setq display-time-world-list '(("America/New_York" "New York")
                                ("UTC/UTC" "UTC")
                                ("Asia/Calcutta" "Bangalore")))

;; Battery info in modeline
(display-battery-mode)

;;; Keybindings

(map! "M-+" 'calc)
(map! "M-=" 'calendar)

;;; Org
(load! "site-lisp/my-org-config")
(load! "site-lisp/my-roam-config")

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(after! flycheck
  (flycheck-define-checker vale
    "A checker for prose"
    :command ("vale" "--output" "line"
              source)
    :standard-input nil
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
    :modes (markdown-mode org-mode text-mode)
    )
  (add-to-list 'flycheck-checkers 'vale 'append)
  (setq flycheck-global-modes '(not org-mode)))

(after! evil
  ;; SOme ideas from https://juanjoalvarez.net/posts/2014/vim-emacsevil-chaotic-migration-guide/
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow)))

(after! evil-snipe
  (setq evil-snipe-scope 'line))

(use-package! evil-surround
  :config (global-evil-surround-mode 1))

(add-hook! text-mode 'abbrev-mode)

;;; Dictionary lokups go to dict.org by default
(setq dictionary-server "dict.org")

;; Select a different theme
;;(setq doom-theme 'doom-solarized-light)

(provide 'config)

;;; config.el ends here
