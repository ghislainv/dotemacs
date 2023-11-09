;;; package --- org-caldav-config.el --- Emacs configuration--------------------
;;; Commentary:
;;; Ghislain Vieilledent <ghislain.vieilledent@cirad.fr> / <ghislainv@gmail.com>
;;; Code:
;; -----------------------------------------------------------------------------

;; Package repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'org)

;; Define org-icalendar variables
(use-package ox-icalendar
  :ensure nil
  :config
  (setq org-icalendar-timezone "Europe/Paris")
  (setq org-icalendar-alarm-time 10)
  (setq org-icalendar-include-todo t
	org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
	org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
	org-icalendar-with-timestamps "active"))

;; Set UID to each headlines in event.org
(with-current-buffer (org-get-agenda-file-buffer "~/.config/emacs/org-caldav/events.org")
  (org-map-entries 'org-id-get-create)
  (save-buffer))

;; Exporting to CalDAV
(use-package org-caldav
  :ensure t
  :after org
  :config
  (setq org-caldav-delete-calendar-entries 'always)
  (setq org-caldav-save-directory "~/.config/emacs/org-caldav")
  (setq org-caldav-url "https://sync.infomaniak.com/calendars/GV01026")
  (setq org-caldav-calendars
	'((:calendar-id "0846b7bd-b7f0-4529-93aa-98dfcc7e99aa"
			:files ("~/.config/emacs/org-caldav/events.org")
			:select-tags ("work")
			:inbox nil
			:sync-direction org->cal)
	  (:calendar-id "bdaabd2f-ba1b-4b20-9b93-9e7ac9b713fc"
			:files ("~/.config/emacs/org-caldav/events.org")
			:select-tags ("perso")
			:inbox nil
			:sync-direction org->cal)
	  (:calendar-id "bba57716-10f9-4119-9145-6252bf8aaef7"
			:files ("~/.config/emacs/org-caldav/events.org")
			:select-tags ("birthday")
			:inbox nil
			:sync-direction org->cal))))

;; Sync Org with calendar
(org-caldav-sync)

;; Export Org to ICS calendar locally
(setq org-agenda-files '("~/.config/emacs/org-caldav/events.org"))
;; work
(setq org-icalendar-exclude-tags '("birthday" "perso"))
(org-icalendar-export-agenda-files)
(copy-file "~/.config/emacs/org-caldav/events.ics"
	   "~/Infomaniak/Notes/ics_calendars/work.ics" t)
;; perso
(setq org-icalendar-exclude-tags '("birthday" "work"))
(org-icalendar-export-agenda-files)
(copy-file "~/.config/emacs/org-caldav/events.ics"
	   "~/Infomaniak/Notes/ics_calendars/perso.ics" t)
;; birthday
(setq org-icalendar-exclude-tags '("work" "perso"))
(org-icalendar-export-agenda-files)
(copy-file "~/.config/emacs/org-caldav/events.ics"
	   "~/Infomaniak/Notes/ics_calendars/birthday.ics" t)

;;; org-caldav-config.el ends here
