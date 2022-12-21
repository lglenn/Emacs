;;; my-org-config --- Tweaks for org-mode
;;;

;;; Code:

(after! org

  (load! "local-org-config" doom-private-dir)

  (map! "\C-cl" 'org-store-link)
  (map! "\C-ca" 'org-agenda)
  (map! "\C-cc" 'org-capture)

  ;;; Tell org how to open attachments
  (add-to-list 'org-file-apps '("\\.docx\\'" . default))
  (add-to-list 'org-file-apps '("\\.xlsx\\'" . default))

  ;;; Use org-habit for tracking habits
  (require 'org-habit)

  ;; Enable hyperlinks to info pages
  (require 'ol-info)

  ;; Show entries from the emacs diary in agenda by default
  (setq org-agenda-include-diary t)

  ;; Use abbrev-mode
  (add-hook! org-mode 'abbrev-mode)

  ;; Wrap lines
  (add-hook! org-mode 'visual-line-mode)

  ;; Pretty Bullets
  (setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))

  ;; Hide text formatters such as / * etc
  (setq org-hide-emphasis-markers t)

  ;; Log todo state changes
  (setq org-log-into-drawer "LOGBOOK")

  (setq org-log-done 'time)

  (setq org-tag-alist '(
                        ("@work" . ?w)
                        ("@home" . ?h)
                        ("@laptop" . ?l)
                        ("@office" . ?o)
                        ))

  (setq org-tag-persistent-alist org-tag-alist)

  (setq org-todo-keywords
        '((sequence "TODO(t!)"
                    "WAITING(w@/!)"
                    "|"
                    "CANCELLED(c@)"
                    "DELEGATED(l@/!)"
                    "EXPIRED(e!)"
                    "GARBAGE(g)"
                    "NOT ACTIONABLE(n)"
                    "DUPE(u!)"
                    "DONE(D!)")))

  ;; Stick archive files in their own directory
  (setq org-archive-location "./archive/archive.org::* From %s")

  ;; Custom Agenda Views

  (defun skip-no-priority ()
    (and
     (not
      (member (org-entry-get nil "PRIORITY") '("A" "C")))
     (point-at-eol)))

  (setq org-agenda-custom-commands
        '(("n" . "Prefix for agendas with todos")
          ("na" "Agenda and all TODOs" ((agenda "") (alltodo "")))
          ("nA" "Agenda and all TODO by tag" ((agenda "") (tags-todo "@work") (tags-todo "@laptop") (tags-todo "@home") (tags-todo "-@work-@home-@laptop")))
          ("nw" "Agenda and work-related TODOs" ((agenda "") (tags-todo "@work")))
          ("np" "Personal agenda and TODOs"
           ((agenda "") (tags-todo "@home") (tags-todo "@laptop"))
           ((org-agenda-skip-function '(org-agenda-skip-subtree-if 'notregexp ":@\\(home\\|laptop\\):"))))
          ("p" "Agenda for items with non-default priority (A or C)" ((agenda "") (alltodo "")) ((org-agenda-skip-function 'skip-no-priority)))
          ("x" agenda)
          ("y" agenda*)
          ("w" todo "WAITING|DELEGATED")
          ("@" . "Special tag searches: @h: @home @l: @laptop @o: office @w: @work")
          ("@h" tags "+@home")
          ("@l" tags "+@laptop")
          ("@o" tags "+@office")
          ("@w" tags "+@work")))

  ;; Ideas from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html and modified
  (let* ((inbox "inbox.org")
         (tickler "tickler.org")
         (someday "someday.org")
         (ideas "ideas.org")
         (tasks "gtd.org")
         (glossary "Glossary/glossary.org")
         (coach "Coach/coach.org")
         (time-tracking "time-tracking.org")
         (gtd-inbox-file (concat gtd-directory inbox))
         (gtd-tickler-file (concat gtd-directory tickler))
         (gtd-someday-file (concat gtd-directory someday))
         (gtd-ideas-file (concat gtd-directory ideas))
         (gtd-tasks-file (concat gtd-directory tasks))
         (time-tracking-file (concat gtd-directory time-tracking))
         (incidents-file time-tracking-file)
         (glossary-file (concat work-directory glossary))
         (coach-file (concat personal-directory coach))
         (meeting-notes-file (concat work-directory "MeetingNotes/meetings.org"))
         (interviews-file (concat work-directory "Interviews/interviews.org"))
         (feedback-file (concat work-directory "Feedback/feedback.org"))
         (drafts-file (concat work-directory "Drafts/drafts.org"))
         (journal-file (concat personal-directory "Journal/journal.org"))
         (daily-summary-file (concat personal-directory "Journal/daily_summaries.org"))
         (capture-templates-dir "capture-templates/")
         (food-diary-file (concat personal-directory "Diet/food_diary.org")))
    (setq org-journal-dir (concat work-directory "Journal"))
    (setq org-journal-file-type 'weekly)
    (setq org-agenda-files (list gtd-tasks-file gtd-tickler-file))

    ;; Call this from a capture template using the %(EXP) expansion to get a timestamp that
    ;; always reflects the current time.
    ;; If you invoke a capture template with the C-1 prefix to select a date other than today
    ;; in a datetree entry, all expansions such as %u or %<FORMAT> will use the time you selected,
    ;; not now. This function allows you to have, for example, a :created: property that always
    ;; reflects the time the entry was created. For some reason, not all date time format
    ;; conversions (such as %a) work properly when invoked from inside a template. Calling this
    ;; function works though.
    (defun current-time-timestamp ()
      (format-time-string "%Y-%m-%d %a %H:%M"))

    (setq org-capture-templates (list
                                 (list '"c" '"Coaching Observation" 'entry
                                       (list 'file+headline coach-file '"Capture")
                                       '"** %?\n")
                                 (list '"C" '"Time Tracking Item" 'entry
                                       (list 'file+headline time-tracking-file '"Time Tracking")
                                       '"* %^{Task}\n:PROPERTIES:\n:created: %U\n:END:\n%?\n"
                                       ':empty-lines '1)
                                 (list '"d" '"Draft" 'entry
                                       (list 'file+headline drafts-file '"Drafts")
                                       '"* %^{Subject}\n** Date: %^U\n** Notes\n%?"
                                       ':empty-lines '1)
                                 (list '"D" '"Discuss" 'entry
                                       (list 'file+headline gtd-inbox-file '"Inbox")
                                       '"* TODO Talk with %^{Person} about %^{Topic} %^g\n:PROPERTIES:\n:created: %U\n:talks: true\n:person: %\\1\n:END:\n%?\n")
                                 (list '"f" '"Feedback" 'entry
                                       (list 'file+headline feedback-file '"Feedback")
                                       '"* %^{Person}\n:PROPERTIES:\n:person: %\\1\n:END:\n** Date: %^U\n** Notes\n%?\n"
                                       ':empty-lines '1)
                                 (list '"F" '"Food Diary (to select a date, invoke org-capture (C-c c) with a C-1 prefix)" 'entry
                                       (list 'file+olp+datetree food-diary-file)
                                       '"** %^{Meal?}\n   - %?\n")
                                 (list '"g" '"Glossary" 'entry
                                       (list 'file+headline glossary-file '"Glossary")
                                       '"** %^{Term}\n:PROPERTIES:\n:term: %\\1\n:END:\n %?"
                                       ':empty-lines '1)
                                 (list '"h" '"Habit" 'entry
                                       (list 'file+headline gtd-tasks-file '"Habits")
                                       '"** TODO %^{Habit} %^g\nSCHEDULED: %^t\n:PROPERTIES:\n:STYLE: habit\n:END:\n %?"
                                       ':empty-lines '1)
                                 (list '"i" '"Interview" 'entry
                                       (list 'file+headline interviews-file '"Interviews")
                                       '"* %^{Candidate Name}\n** Date: %^U\n** Notes\n  - %?"
                                       ':empty-lines '1)
                                 (list '"I" '"Incident" 'entry
                                       (list 'file+olp incidents-file '"Time Tracking" '"Operational Excellence" '"Incidents")
                                       (list 'file (concat capture-templates-dir "incident.txt")) ':empty-lines '1)
                                 (list '"j" '"Journal Entry" 'entry
                                       (list 'file+olp+datetree journal-file)
                                       '"** %<%k:%M %p>\n%?\n" :tree-type 'week)
                                 (list '"m" '"Meeting" 'entry
                                       (list 'file+headline meeting-notes-file '"Meetings")
                                       '"* %^{Description}\n** Date: %^U\n** Agenda\n   - \n** Attendees\n   - \n** Notes\n   - %? \n** To-Do's\n"
                                       ':empty-lines '1)
                                 (list '"o" '"One on One" 'entry
                                       (list 'file+headline meeting-notes-file '"Meetings")
                                       '"* 1:1: %^{Description}\n** Date: %^U\n** Agenda\n   - %? \n** Notes\n   -  \n** To-Do's\n"
                                       ':empty-lines '1)
                                 (list '"p" '"Todo [projects]" 'entry
                                       (list 'file+headline gtd-tasks-file '"Projects")
                                       '"* %^{Brief Description} [/] %^g\n:PROPERTIES:\n:created: %U\n:END:\n%?\n")
                                 (list '"P" '"Todo [serial projects]" 'entry
                                       (list 'file+headline gtd-tasks-file '"Serial Projects")
                                       '"* %^{Brief Description} [%] %^g\n:PROPERTIES:\n:created: %U\n:END:\n** TODO %?\n")
                                 (list '"s" '"My Staff Meeting" 'entry
                                       (list 'file+headline meeting-notes-file '"Meetings")
                                       '"* Staff Meeting\n** Date: %^U\n** Agenda\n   - \n** Attendees\n   - \n** Notes\n   - %? \n** To-Do's\n"
                                       ':empty-lines '1)
                                 (list '"S" '"Staff Meeting as Attendee" 'entry
                                       (list 'file+headline meeting-notes-file '"Meetings")
                                       (list 'file (concat capture-templates-dir "staff_meeting_as_attendee.txt")) ':empty-lines '1)
                                 (list '"t" '"Todo [inbox]" 'entry
                                       (list 'file+headline gtd-inbox-file '"Inbox")
                                       '"* TODO %^{Brief Description} %^g\n:PROPERTIES:\n:created: %U\n:END:\n%?\n")
                                 (list '"T" '"Tickler" 'entry
                                       (list 'file+headline gtd-tickler-file '"Tickler")
                                       '"* %^{Brief Description} %^g\n:PROPERTIES:\n:created: %U\n:END:\nSCHEDULED: %^t\n%?\n")
                                 (list '"u" '"Daily Summary (prefix with C-1 to force a date other than today)" 'entry
                                       (list 'file+olp+datetree daily-summary-file)
                                       (list 'file (concat capture-templates-dir "daily_summary.txt")) :tree-type 'week)
                                 (list '"w" '"Film and TV" 'entry
                                       (list 'file+headline gtd-someday-file '"Movies")
                                       '"** %?\n")))

    (setq org-default-notes-file gtd-inbox-file)
    (setq org-refile-targets
          (let ((refile-target-files (list (cons nil '(:maxlevel . 9))
                                           (cons tasks '(:maxlevel . 3))
                                           (cons someday '(:level . 1))
                                           (cons ideas '(:level . 1))
                                           (cons tickler '(:maxlevel . 2))
                                           (cons time-tracking '(:maxlevel . 4))
                                           (cons inbox '(:maxlevel . 2))))
	        (prepend-directory-if-string
	         (lambda (e)
		   (let ((file (car e))
		         (params (cdr e)))
		     (cond ((stringp file)
			    (cons (concat gtd-directory file) params))
			   (t e))))))
            (mapcar prepend-directory-if-string refile-target-files)))))

  (provide 'my-org-config)
