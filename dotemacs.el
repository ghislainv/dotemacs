;;; package --- init.el --- Emacs configuration--------------------------------
;;; Commentary:
;;; Ghislain Vieilledent <ghislain.vieilledent@cirad.fr> / <ghislainv@gmail.com>
;;; Code:
;; ----------------------------------------------------------------------------

;; --------------------------------------
;; CUSTOM FILE
;; --------------------------------------

;; Define the custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; --------------------------------------
;; FONT
;; --------------------------------------

;; Set default font
(when (member "Bitstream Vera Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Bitstream Vera Sans Mono"))

;; --------------------------------------
;; INSTALL PACKAGES
;; --------------------------------------

(defvar my-package-list)
(setq my-package-list '(auctex
			better-defaults
			citeproc
			exec-path-from-shell
			flycheck
			htmlize
			jedi
			olivetti
			org
			org-contrib
			ox-hugo
			ox-rst
			ox-twbs
			pretty-mode
			py-autopep8
			pylint
			use-package
			xterm-color
			yaml-mode))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install
(dolist (i-package my-package-list)
  (unless (package-installed-p i-package)
    (package-install i-package)))

;; Use-package
;; (always install packages if not installed)
(require 'use-package)
(setq use-package-always-ensure 't)

;; Absolute value for package-user-dir
(setq package-user-dir (concat user-emacs-directory "elpa/"))

;; --------------------------------------
;; BASIC CUSTOMIZATION
;; --------------------------------------

;; Themes
(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))
;; (load-theme 'material t)
;; (load-theme 'zenburn t)
;; (load-theme 'tango-2 t)

;; Generalities
(setq user-full-name "Ghislain Vieilledent")
(setq user-mail-address "ghislainv@mtloz.fr")
(setq inhibit-startup-message t) ;; hide the startup message
(global-display-line-numbers-mode 1) ;; enable line numbers globally
(fset 'yes-or-no-p 'y-or-n-p) ;; Treat 'y' or <CR> as yes, 'n' as no.
(setq make-backup-files nil) ;; don't save backup ~files
(setq scroll-step 1) ;; line by line scrolling (not half of the window)
(global-unset-key (kbd "C-z")) ;; disable iconification bindings
(global-visual-line-mode 1) ;; line wrapping
(tool-bar-mode 0) ;; toolbar inactive
(menu-bar-mode 1) ;; menubar active
(setq next-line-add-newlines 1) ;; add new line with C-n at buffer end
(desktop-save-mode 1) ;; save desktop
;; On affiche les colonnes dans la modeline
(column-number-mode)
;; Highlighting the current line for all programming major modes
(add-hook 'prog-mode-hook #'hl-line-mode)
;; On ne demande pas confirmation pour tuer les sous-processus en sortant d'Emacs
(setq confirm-kill-processes nil)
(setq undo-limit 80000000                     ; Raise undo-limit to 80Mb
      tab-width 4                             ; set the tab width
      tab-always-indent 'complete)            ; make tab key do indent first then completion

;; Encodage en UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; System locale to use for formatting time values.
(setq system-time-locale "C")         ; Make sure that the weekdays in the
                                      ; time stamps of your Org mode files and
                                      ; in the agenda appear in English.

;; Size and position of the window
(if window-system
    (setq initial-frame-alist '((fullscreen . maximized)))) ;; maximize screen

;; Fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
(global-set-key [f6] 'toggle-fullscreen)

;; Permanently enable syntax checking with Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Unfill paragraph
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Unfill a pragraph of text.
REGION: region of the text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
;; https://emacs.stackexchange.com/questions/29695/key-mapping-push-m-q-once-for-fill-paragraph-and-twice-for-unfill-paragraph
(defun my-fill-paragraph (&optional arg)
  "Fill or unfill a paragraph.
If repeated, alternate.  A prefix ARG for filling means
justify (as for `fill-paragraph')."
  (interactive "P")
  (let ((fillp  (not (eq last-command 'fill-paragraph))))
    (apply (setq this-command  (if fillp 'fill-paragraph 'unfill-paragraph))
           (and fillp  arg  '(full t)))))
;; Handy key definition
(define-key global-map (kbd "M-q") 'my-fill-paragraph)

;; Diminish, a feature that removes certain minor modes from mode-line.
(use-package diminish
  :ensure t)

;; Company for auto-completion
(use-package company
  :ensure t
  :diminish company-mode
  :custom
  (company-minimum-prefix-length 4)
  (company-idle-delay 0.1)
  (company-dabbrev-minimum-length 8)
  (company-selection-wrap-around t)
  :config
  (global-company-mode 1))

;; Company box
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Rainbow delimiter
(use-package rainbow-delimiters
  :ensure t)

;; Auto-revert
;; Keeping buffers automatically up-to-date
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
(require 'autorevert)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; -------------------------------------
;; Emojify
;; -------------------------------------

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
  :config
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c ;") #'emojify-insert-emoji))

;; -------------------------------------
;; Mastodon
;; -------------------------------------

(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://ecoevo.social"
        mastodon-active-user "ghislainv"))

;; -------------------------------------
;; Mu4e
;; -------------------------------------

;; https://f-santos.gitlab.io/2020-04-24-mu4e.html
;; https://git.mmk2410.org/mmk2410/dotfiles/commit/879b26f5c2c12b58b67b598d98d98903b5a7f1bd
;; https://www.djcbsoftware.nl/code/mu/mu4e.html

;; See also
;; https://github.com/munen/emacs.d/blob/master/configuration.org#mu4e

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/elpa/mu4e-1.12.9/"
  :ensure nil
  :init
  (add-hook 'mu4e-headers-mode-hook (lambda () (display-line-numbers-mode 0)))
  :bind (("C-c m" . mu4e)
	 :map mu4e-headers-mode-map
	 ("C-c c" . mu4e-org-store-and-capture)
	 :map mu4e-view-mode-map
	 ("C-c c" . mu4e-org-store-and-capture))
  :config
  ;; Mail user agent
  (setq mail-user-agent 'mu4e-user-agent)
  ;; SMTP settings:
  (setq send-mail-function 'smtpmail-send-it)       ; should not be modified
  (setq smtpmail-smtp-server "mail.infomaniak.com") ; host running SMTP server
  (setq smtpmail-smtp-service 465)                  ; SMTP service port number
  (setq smtpmail-stream-type 'ssl)                  ; type of SMTP connections to use
  ;; Mail folders:
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-trash-folder  "/Trash")
  (setq mu4e-refile-folder "/Archives")
  ;; The command used to get your emails (adapt this line, see section 2.3):
  (setq mu4e-get-mail-command "mbsync --config ~/.config/emacs/mu4e/.mbsyncrc mtloz")
  ;; Further customization:
  (setq mu4e-update-interval (* 5 60)             ; refresh email every 5 minutes
	mu4e-headers-auto-update t                ; avoid to type `g' to update
	mu4e-use-fancy-chars nil                  ; allow fancy icons for mail threads
	mu4e-search-include-related nil           ; Do not include related messages in headers
	mu4e-search-threads nil                   ; Do not show threads
	mu4e-change-filenames-when-moving t       ; This is set to 't' to avoid mail syncing issues when using mbsync
        mu4e-trash-without-flag t)                ; Trash moves message but does not set flag T
  
  ;; Faster reindexing (not with mtloz)
  ;;(setq mu4e-index-cleanup nil    ; don't do a full cleanup check
  ;;	mu4e-index-lazy-check t)  ; don't consider up-to-date dirs
  ;; Date format
  (setq mu4e-headers-date-format "%Y-%m-%d")
  (setq mu4e-headers-time-format "%H:%M")
  ;; Headers
  (setq mu4e-headers-fields
		'((:human-date . 12)
		  (:flags . 6)
		  (:mailing-list . 10)
		  (:from-or-to . 22)
		  (:subject)))
  ;; Maildir shortcuts
  (setq	mu4e-maildir-shortcuts
		'((:maildir "/INBOX" :key ?i)
		  (:maildir "/Drafts" :key ?d)
		  (:maildir "/Archives" :key ?a)
		  (:maildir "/Promotions" :key ?p)
		  (:maildir "/Trash" :key ?t)
		  (:maildir "/Sent" :key ?s)))
  ;; Signature
  (defvar ghvi/signature
	(concat
	 "-------------------------------------------------------------------\n"
	 "Ghislain VIEILLEDENT\n"
	 "Phone: +687.97.18.15 (New Caledonian No.)\n"
	 "WhatsApp: +33.6.24.62.65.07 (French No.)\n"
	 "E-mail: ghislainv(at)mtloz(dot)fr\n"
	 "Web site: https://ghislainv.fr\n"
	 "-------------------------------------------------------------------\n")
	"My email signature")
  (setq message-signature ghvi/signature)
  ;; Do not reply to yourself
  ;; A value of nil means exclude ‘user-mail-address’ only
  (setq message-dont-reply-to-names nil))

;; -------------------------------------
;; org-msg
;; -------------------------------------

(use-package org-msg
  :ensure t
  :config
  (org-msg-mode 0))

;; -------------------------------------
;; Icons
;; -------------------------------------

;; Gestion des icônes par all-the-icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; -------------------------------------
;; Dired
;; -------------------------------------

(use-package dired
  :ensure nil
  :defines image-dired-external-viewer
  :config
  ;; Split window, Dired tries to guess a default target directory
  (setq dired-dwim-target t
	delete-by-moving-to-trash t)
  ;; Activate dired-x
  (require 'dired-x)
  ;; Define external image viewer/editor
  (setq image-dired-external-viewer "/usr/bin/gimp")
  ;; Human readable
  (setq dired-listing-switches "-alFh"))

(use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)
    :config (setq all-the-icons-dired-monochrome nil))

;; -------------------------------------
;; Disk usage
;; -------------------------------------

(use-package disk-usage
  :ensure t)

;; -------------------------------------
;; IBUFFER
;; -------------------------------------

;; Using the ibuffer mode
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;; -------------------------------------
;; elfeed
;; -------------------------------------

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  )

;; Configure Elfeed with org mode
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (defvar ghvi/elfeed-org-files
    (list (expand-file-name "elfeed/elfeed.org" user-emacs-directory))
    "List of elfeed.org files.")
  (setq rmh-elfeed-org-files ghvi/elfeed-org-files)
  )

;; -------------------------------------
;; PDF-TOOLS
;; -------------------------------------
;; Replace DocView for pdf
;; https://github.com/vedang/pdf-tools

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (defun ghvi/turn-off-line-numbers ()
    "Disable line numbering in the current buffer."
    (display-line-numbers-mode -1))
  :hook (pdf-view-mode . ghvi/turn-off-line-numbers)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))
       
;; -------------------------------------
;; MAGIT
;; -------------------------------------

(use-package magit
  :if (executable-find "git")
  :ensure t
  :bind ("C-x g" . magit-status))

;; ;; -------------------------------------
;; ;; TREE CONFIGURATION
;; ;; -------------------------------------

;; ;; Configuration de Treemacs
;; ;; https://medspx.fr/blog/Debian/emacs_2020
;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :bind ("C-²" . treemacs-select-window)
;;   :config
;;   (setq treemacs-width 30
;; 	treemacs-indentation '(6 px)
;; 	treemacs-is-never-other-window t
;; 	treemacs-width-is-initially-locked nil
;; 	treemacs-space-between-root-nodes nil
;; 	treemacs-collapse-dirs 4
;; 	treemacs-sorting 'alphabetic-case-insensitive-asc
;; 	treemacs-text-scale -1)
;;   ;;(treemacs-indent-guide-mode)
;;   (treemacs-resize-icons 14)
;;   (treemacs-follow-mode t)
;;   (treemacs-tag-follow-mode t)
;;   (treemacs-filewatch-mode t)
;;   (treemacs-fringe-indicator-mode 'always)
;;   (treemacs-hide-gitignored-files-mode nil))

;; ;; Treemacs avec support magit
;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)

;; -------------------------------------
;; YAML
;; -------------------------------------

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
	  #'(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; -------------------------------------
;; R/ESS CONFIGURATION
;; -------------------------------------

;; Insert pipe function
(defun ghvi/insert-r-pipe ()
  "Insert the pipe operator in R, |>."
  (interactive)
  (just-one-space 1)
  (insert "|>")
  (reindent-then-newline-and-indent))

;; Controlling buffer display
;; (see section in http://ess.r-project.org/ess.pdf)
(defun ghvi/ess-display-buffer ()
  "Displaying buffers with ESS."
  (setq display-buffer-alist
	`(("^\\*R"
	   (display-buffer-reuse-window display-buffer-in-side-window)
	   (side . right)
	   (slot . -1)
           (window-width . 0.50)
           (reusable-frames . t)
	   (dedicated . nil))
          ("^\\*Help"
           (display-buffer-reuse-window display-buffer-in-side-window)
	   (side . right)
           (slot . 1)
           (window-width . 0.50)
           (reusable-frames . t))))
  ;; (setq display-buffer-alist
  ;; 	'(("*R"
  ;; 	   nil
  ;; 	   (dedicated . t))))
  )

;; Configure ESS package
(use-package ess
  :ensure t
  :defer t
  :defines ess-r-mode-map inferior-ess-r-mode-map
  :init
  (require 'ess-site)
  :mode ("\\.[rR]\\'" . R-mode)
  :hook ((ess-mode . ghvi/ess-display-buffer)
	 (ess-mode . rainbow-delimiters-mode))
  :commands R
  :bind (:map ess-r-mode-map
              (";" . ess-insert-assign)
              ("C-c =" . ess-insert-assign)
              ("C-c :" . ghvi/insert-r-pipe)
              :map inferior-ess-r-mode-map
              (";" . ess-insert-assign)
              ("C-c =" . ess-insert-assign)
              ("C-c :" . ghvi/insert-r-pipe))
  :config
  ;; Style
  (setq ess-style 'RStudio)
  ;; Set locales
  (unless (getenv "LANG") (setenv "LANG" "fr_FR.UTF-8"))
  (unless (getenv "LC_ALL") (setenv "LC_ALL" "fr_FR.UTF-8"))
  ;; Stop R repl eval from blocking emacs.
  (setq ess-eval-visibly 'nowait)
  ;; Syntax highlighting
  (setq ess-R-font-lock-keywords
	'((ess-R-fl-keyword:modifiers . t)
	  (ess-R-fl-keyword:fun-defs . t)
	  (ess-R-fl-keyword:keywords . t)
	  (ess-R-fl-keyword:assign-ops . t)
	  (ess-R-fl-keyword:constants . t)
	  (ess-fl-keyword:fun-calls . t)
	  (ess-fl-keyword:numbers . nil)
	  (ess-fl-keyword:operators . t)
	  (ess-fl-keyword:delimiters . nil)
	  (ess-fl-keyword:= . t)
	  (ess-R-fl-keyword:F&T . t)
	  (ess-R-fl-keyword:%op% . t)))
  )

;; -------------------------------------
;; POLYMODE
;; -------------------------------------

;; rmarkdown
(use-package poly-R
  :ensure t
  :config
  ;; associate the new polymode to Rmd files:
  (add-to-list 'auto-mode-alist
               '("\\.[rR]md\\'" . poly-gfm+r-mode))
  ;; uses braces around code block language strings:
  (setq markdown-code-block-braces t)
  (setq polymode-weaver-output-file-format "%s")
  (setq polymode-exporter-output-file-format "%s"))

;; -------------------------------------
;; HUNSPELL CONFIGURATION
;; -------------------------------------

;; Spell checking
;; Requires Hunspell
(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure French and English.
  (setq ispell-dictionary "fr_FR,en_GB,en_US,es_ES")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "fr_FR,en_GB,en_US,es_ES")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal")
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

;; Correction grammaticale (pour le français)
;; https://github.com/milouse/flycheck-grammalecte
(use-package flycheck-grammalecte
  :ensure t
  :after flycheck
  :hook (fountain-mode . flycheck-mode)
  :init
  (setq flycheck-grammalecte-report-apos nil
        flycheck-grammalecte-report-esp nil
        flycheck-grammalecte-report-nbsp nil)
  :config
  (add-to-list 'flycheck-grammalecte-enabled-modes 'fountain-mode)
  (grammalecte-download-grammalecte)
  (flycheck-grammalecte-setup))

;; -------------------------------------
;; OLIVETTI MINOR MODE
;; -------------------------------------

;; Source:
;; https://lucidmanager.org/productivity/emacs-for-distraction-free-writing/

;; Distraction-free screen
(use-package olivetti
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t)
          (setq olivetti-body-width 100))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("<f9>" . distraction-free)))

;; -------------------------------------
;; LATEX/BIBTEX CONFIGURATION
;; -------------------------------------
;; https://lucidmanager.org/productivity/emacs-bibtex-mode/

;; Customizing bibtex
(use-package bibtex
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file." "" )))
  (bibtex-align-at-equal-sign t))

;; Biblio package for adding BibTeX records and download publications
(use-package biblio
  :ensure t
  :config
  (setq biblio-crossref-user-email-address "ghislainv@mtloz.fr")
  (defun add-doi () "Adding a doi to .bib file"
	(interactive)
	(progn
	  (defvar ghvi/mydoi "" "DOI variable")
      (setq ghvi/mydoi (read-string "DOI "))
      (find-file "~/Documents/Bibliography/biblio.bib")
      (goto-char (point-max))
	  (forward-line -1)
      (biblio-doi-insert-bibtex ghvi/mydoi))))

;; citar
;; https://github.com/emacs-citar/citar#configuration
(use-package citar
  :ensure t
  :no-require
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography (list (expand-file-name "~/Documents/Bibliography/biblio.bib")))
  (citar-library-paths '("~/Documents/Bibliography/Articles/"))
  (citar-library-file-extensions '("pdf" "jpg" "docx"))
  (citar-file-additional-files-separator "-")
  (citar-notes-paths '("~/Documents/Bibliography/Notes/"))
  (citar-citeproc-csl-styles-dir org-cite-csl-styles-dir)
  (citar-citeproc-csl-style "ecology.csl")
  (citar-format-reference-function 'citar-citeproc-format-reference)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

;; The citar-embark package adds contextual access actions in the
;; minibuffer and at-point via the citar-embark-mode minor mode. When
;; using Embark, the Citar actions are generic, and work the same
;; across org, markdown, and latex modes.
(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;; -------------------------------------
;; PDF viewer
;; -------------------------------------

;; This is to use pdf-tools instead of doc-viewer
(use-package pdf-tools
  :config
  (pdf-tools-install)
  ;; This means that pdfs are fitted to width by default when you open them
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; -------------------------------------
;; SHELL CONFIGURATION
;; -------------------------------------

;; xterm-color
;; https://github.com/atomontage/xterm-color
(require 'xterm-color)
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))
(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;; zsh with M-x shell
(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(defvar explicit-zsh-args)
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  "Setup zsh shell mode."
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

;; Path from shell
;; https://github.com/purcell/exec-path-from-shell
;;(dolist (var '("WDPA_KEY"))
;;   (add-to-list 'exec-path-from-shell-variables var))
;; Remove "-i" from exec-path-from-shell-arguments for non-interactive shell
(setq exec-path-from-shell-arguments '("-l"))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; essh is for bash what ess is for R
;; https://www.emacswiki.org/emacs/essh.el
;; Load additional libraries
(defun ghvi/essh-sh ()
  "Define essh shorcut keys."
  (define-key sh-mode-map (kbd "C-<return>") 'pipe-region-to-shell)
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
  (define-key sh-mode-map (kbd "C-<return>") 'pipe-line-to-shell-and-step)
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory))
(use-package essh
  :load-path "essh/essh.el"
  :init
  :hook (sh-mode . ghvi/essh-sh))

;; --------------------------------------
;; ORG-MODERN
;; --------------------------------------

;; (use-package org-modern
;;   :ensure t
;;   :hook
;;   (org-mode . global-org-modern-mode)
;;   :custom
;;   (org-modern-star 'replace)
;;   (org-modern-list nil)
;;   ;;(org-modern-list '((?- . "•")))
;;   (org-modern-timestamp nil)
;;   (org-modern-todo nil)
;;   (org-modern-tag nil)
;;   (org-modern-block-name nil)
;;   (org-modern-keyword nil)
;;   (org-modern-checkbox nil)
;;   (org-modern-table nil))

;; --------------------------------------
;; TRAMP
;; --------------------------------------

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (tramp-terminal-type "tramp")
  :config
  ;; Open file with sudo
  (defun sudo ()
    "Use TRAMP to `sudo' the current buffer."
    (interactive)
    (when buffer-file-name
      (find-alternate-file
       (concat "/sudo:root@localhost:"
			   buffer-file-name)))))

;; --------------------------------------
;; ELPY
;; --------------------------------------

;; ;; Enable Elpy
;; (elpy-enable)

;; ;; Elpy RPC
;; (setq elpy-rpc-python-command "python3")

;; ;; Python standard interpreter
;; (setq python-shell-interpreter "python"
;;       python-shell-interpreter-args "-i")
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;               "python")

;; ;; Add a function to send a single line to the Python console
;; (defun python-shell-send-line ()
;;   "Send a single line to the Python console."
;;   (interactive)
;;   (save-mark-and-excursion
;;     (move-beginning-of-line nil)
;;     (set-mark-command nil)
;;     (move-end-of-line nil)
;;     (python-shell-send-region
;;      (region-beginning)
;;      (region-end))))

;; ;; Define key
;; (defun elpy-hook ()
;;   "Define shortcut keys."
;;   (define-key elpy-mode-map (kbd "C-<return>") 'elpy-shell-send-region-or-buffer)
;;   (define-key elpy-mode-map (kbd "C-<backspace>") 'python-shell-send-line))
;; (add-hook 'elpy-mode-hook 'elpy-hook)

;; ;; use flycheck not flymake with elpy
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ;; enable autopep8 formatting on save
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-mode)

;; (setq python-indent-offset 4)

;; --------------------------------------
;; PYTHON
;; --------------------------------------

(use-package python
  :ensure nil
  :init
  ;; Add a function to send a single line to the Python console
  (defun ghvi/python-shell-send-line ()
	"Send a single line to the Python console."
	(interactive)
	(save-mark-and-excursion
      (move-beginning-of-line nil)
      (set-mark-command nil)
      (move-end-of-line nil)
      (python-shell-send-region
       (region-beginning)
       (region-end))))
  :bind (:map python-mode-map
			  ("C-<return>" . python-shell-send-region)
			  ("C-<backspace>" . ghvi/python-shell-send-line))
  :config
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt --profile=dev")
  (setq python-shell-completion-native-enable nil))
  
;; --------------------------------------
;; PYTHON VIRTUAL ENVIRONMENT
;; --------------------------------------

;; Virtual Env
(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/venvs")
  :config
  (pyvenv-mode 1))

;; --------------------------------------
;; EGLOT
;; --------------------------------------

(use-package eglot
  :ensure t
  :commands eglot
  :hook
  ((python-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (setq eglot-autoshutdown t))

;; --------------------------------------
;; ORG MODE
;; --------------------------------------
;; Sources:
;; https://gitlab.inria.fr/learninglab/mooc-rr/mooc-rr-ressources/-/blob/master/module2/ressources/rr_org/init.el
;; http://doc.norang.ca/org-mode.html

;; Open in org-mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;; Default directories and files
(setq org-directory "~/kDrive/Notes/")
(setq org-default-notes-file (concat org-directory "/notes_work.org"))

;; Configure org-refile
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-default-notes-file :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path (quote file))       ; Show full paths for refiling

;; Standard key bindings
(global-set-key (kbd "C-c i") 'org-id-get-create)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c S-t") 'org-babel-execute-subtree)
(global-set-key (kbd "C-c o")
		(lambda () (interactive) (find-file (concat org-directory "/notes_work.org"))))

;; Settings
(setq org-hide-leading-stars t
      org-startup-indented t)
;;(setq org-alphabetical-lists t)
(setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
(setq org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
(setq org-fontify-whole-heading-line t) ;; Fontify the whole line for headings (with a background color).
(setq org-src-tab-acts-natively t) ;; you want this to have completion in blocks
(setq org-src-preserve-indentation t)
(setq org-export-with-smart-quotes t)
;;(setq org-edit-src-content nil)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width 600)
(setq org-use-sub-superscripts "{}")

;; The org-appear package helps by displaying the markers while the cursor is on a rich text word.
(use-package org-appear
  :after org
  :ensure t
  :custom
  (org-appear-autolinks t)
  (org-appear-autoemphasis t)
  :hook
  (org-mode . org-appear-mode))

;; In css mode, htmlize uses cascading style sheets to specify colors
(require 'htmlize)
(setq org-html-htmlize-output-type 'css)
(setq org-html-postamble nil)
(setq org-export-allow-bind-keywords t)

;; Function to execute the code bock and move to the next
(defun execute-code-block-and-move-to-next ()
  "Execute the code bock and move to the next."
  (interactive)
  (org-ctrl-c-ctrl-c)
  (org-babel-next-src-block 1)
  (recenter))
(add-hook 'org-mode-hook
	  (lambda () (local-set-key (kbd "M-n") #'execute-code-block-and-move-to-next)))

;; Export to LaTeX
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
;; LaTeX classes
;; article
(add-to-list 'org-latex-classes
             '("article" "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; koma article (more modern design than the standard LaTeX classes)
(add-to-list 'org-latex-classes
             '("koma-article" "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; koma article (more modern design than the standard LaTeX classes)
(add-to-list 'org-latex-classes
             '("koma-article" "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; PNAS article
(add-to-list 'org-latex-classes
	     '("pnas-article" "\\documentclass{pnas-new}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; Pinp Is Not PNAS
(add-to-list 'org-latex-classes
	     '("pinp-article" "\\documentclass{pinp}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; Beamer
(use-package ox-beamer
  :after org
  :ensure nil
  :config
  (add-to-list 'org-latex-classes
	       '("beamer" "\\documentclass{beamer}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; We also translate bold into beamer 'structure'
;; https://xgarrido.github.io/emacs-starter-kit/starter-kit-org.html
(defun ghvi/beamer-bold (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\structure" contents)))
(defun ghvi/beamer-underline (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))
(defun ghvi/beamer-strike (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\alert" contents)))
(add-to-list 'org-export-filter-bold-functions 'ghvi/beamer-bold)
(add-to-list 'org-export-filter-underline-functions 'ghvi/beamer-underline)
(add-to-list 'org-export-filter-strike-through-functions 'ghvi/beamer-strike)


;; Export process from orgmode to LaTeX to PDF
(setq org-latex-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))

;; Export to MS-Word
;; Need to have LibreOffice on your computer
;;(setq org-odt-preferred-output-format "doc")
(setq org-odt-preferred-output-format nil)

;; Export to HTML5
(require 'ox-html)
(setq org-html-html5-fancy t
      org-html-doctype "html5")

;; In org-mode 9 you need to have #+PROPERTY: header-args :eval never-export
;; in the beginning or your document to tell org-mode not to evaluate every
;; code block every time you export.
(setq org-confirm-babel-evaluate nil) ;; Do not ask for confirmation all the time!!

;; Image previews
(setq org-image-actual-width '(600)
      org-startup-with-inline-images nil)

;; Languages evaluated
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
  (shell . t)
  (python . t)
  (R . t)
  (screen . t)
  (sql .t)
  (org . t)
  (makefile . t)))

;; Org to RST
(require 'ox-rst)

;; Bibliography
(require 'ox-bibtex)

;; Add :ignore: tag to ignore headline
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

;; Processors for org-cite
(require 'oc-csl)
(require 'oc-natbib)

;; Biblio
(setq org-cite-global-bibliography
	  (list (expand-file-name "~/Documents/Bibliography/biblio.bib")))

;; CSL directory
(defvar ghvi/csldir (expand-file-name "~/Documents/Bibliography/csl")
  "CSL directory.")
(setq org-cite-csl-styles-dir ghvi/csldir)

;; Multilines bold
(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 1000) ;; Up to 100 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

;; Templates
(require 'org-tempo)

(defun ghvi/add-org-template (template)
  "Adding new org templates.

TEMPLATE: Template to be added in the form of an association,
i.e. \\='(a . b)."
  (add-to-list 'org-structure-template-alist template))

(ghvi/add-org-template
 '("S" . "src shell"))

(ghvi/add-org-template
 '("m" . "src emacs-lisp"))

(ghvi/add-org-template
 '("r" . "src R :results output :session *R* :exports both"))

(ghvi/add-org-template
 '("R" . "src R :results graphics file :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R*"))

(ghvi/add-org-template
 '("RR" . "src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R*"))

(ghvi/add-org-template
 '("p" . "src python :results output :exports both"))

(ghvi/add-org-template
 '("P" . "src python :results output :session :exports both"))

;; The following needs to be updated for Python with figure
;; (ghvi/add-org-template
;;  '("PP" . "src python :results file :session :var matplot_lib_filename=(org-babel-temp-file \"figure\" \".png\") :exports both\nimport matplotlib.pyplot as plt\n\nimport numpy\nx=numpy.linspace(-15,15)\nplt.figure(figsize=(10,5))\nplt.plot(x,numpy.cos(x)/x)\nplt.tight_layout()\n\nplt.savefig(matplot_lib_filename)\nmatplot_lib_filename"))

;; Markdown to org
;; http://yummymelon.com/devnull/converting-a-markdown-region-to-org-revisited.html
(defun ghvi/markdown-to-org-region (start end)
  "Convert Markdown formatted text in region (START, END) to Org.

This command requires that pandoc (man page `pandoc(1)') be
installed."
  (interactive "r")
  (shell-command-on-region
   start end
   "pandoc -f markdown -t org --wrap=preserve" t t))


;; --------------------------------------
;; KOMA LETTER
;; --------------------------------------

(require 'ox-koma-letter)

;; --------------------------------------
;; Navigating
;; --------------------------------------

;; Between windows with ace-window
(use-package ace-window
  :ensure t
  :demand t
  :init
  (setq aw-keys '(?q ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
  :bind* ("M-o" . ace-window))

;; --------------------------------------
;; ORG AGENDA
;; --------------------------------------

;; Define org-icalendar variables
(use-package ox-icalendar
  :ensure nil
  :config
  (setq org-icalendar-timezone "Europe/Paris"))

;; Define org-agenda variables
(use-package org-agenda
  :ensure nil
  :after org
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-include-diary t) ;; See .config/emacs_ghvi/diary file
  (setq diary-show-holidays-flag nil) ;; US holidays in calendar-holidays variables
  (setq org-agenda-files '("~/kDrive/Notes/notes_work.org"
			   "~/kDrive/Notes/notes_perso.org"
			   "~/kDrive/Notes/todos.org"
			   "~/kDrive/Notes/events.org")))

;; Casual calendar
(use-package casual
  :ensure t)

;; To import an ics calendar
;;https://etalab.github.io/jours-feries-france-data/ics/jours_feries_nouvelle-caledonie.ics
;;(icalendar-import-file "~/Téléchargements/jours_feries_nouvelle-caledonie.ics"
;;                       "~/.config/emacs_ghvi/diary")

;; Using org-caldav to export events using CalDAV
;; https://github.com/dengste/org-caldav
;; See ~/.config/emacs/org-caldav/org-caldav-config.el on fraisedesbois

;; --------------------------------------
;; EXPORT TO DOCX
;; --------------------------------------

(defun ox-export-to-docx-and-open ()
 "Export the current org file as a docx via markdown."
 (interactive)
 (let* ((current-file (buffer-file-name))
        (basename (file-name-sans-extension current-file))
        (docx-file (concat basename ".docx")))
   (save-buffer)
   (when (file-exists-p docx-file) (delete-file docx-file))
   (shell-command (format
                   "pandoc -s %s -f org+smart -o %s"
                   current-file docx-file))
   (org-open-file docx-file '(16))))

;; --------------------------------------
;; ORG CAPTURE
;; --------------------------------------

;; Files
(defvar ghvi/org-default-todos-file "~/kDrive/Notes/todos.org"
  "Default file for todos.")
(defvar ghvi/org-default-events-file "~/kDrive/Notes/events.org"
  "Default file for events.")

;; org-capture-templates
(setq org-capture-templates
      '(("t" "Task (work)" entry
	 (file+headline ghvi/org-default-todos-file "Work tasks")
	 "*** TODO %?")
	("T" "Task (perso)" entry
	 (file+headline ghvi/org-default-todos-file "Personal tasks")
	 "*** TODO %?")
	("m" "Meeting (work)" entry
	 (file+headline ghvi/org-default-events-file "Work meetings, TZ Nouméa")
	 "** %?\n%t")
	("M" "Meeting (personal appointment)" entry
	 (file+headline ghvi/org-default-events-file "Personal appointments, TZ Nouméa")
	 "** %?\n%t")
	("e" "Event (non professional event)" entry
	 (file+headline ghvi/org-default-events-file "Other events")
	 "** %?\n%t")
	("n" "Note" entry
	 (file+headline org-default-notes-file "Notes")
	 "** %?\n%t" :empty-lines 1)
	("A" "Answer email (perso)" entry
	 (file+headline ghvi/org-default-todos-file "Personal tasks")
	 "*** TODO %:fromname: %a %?")))

;; todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "STRT(r)" "WAIT(w)" "SDAY(s)" "|" "DONE(d)" "CANX(c)")))

;; faces for specific TODO keywords
(setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("STRT" . ghvi/org-inprogress-kwd)
        ("WAIT" . ghvi/org-waiting-for-kwd)
        ("SDAY" . ghvi/org-someday-kwd)
        ("DONE" . org-done)
        ("CANX" . org-done)))

;; Org non-standard faces
(defface ghvi/org-inprogress-kwd
  '((t (:weight bold :box (:line-width 1 :color "#D9D14A")
		:foreground "#D9D14A" :background "#FCFCDC")))
  "Face used to display state STRT.")
(defface ghvi/org-waiting-for-kwd
  '((t (:weight bold :box (:line-width 1 :color "#89C58F")
		:foreground "#89C58F" :background "#E2FEDE")))
  "Face used to display state WAIT.")
(defface ghvi/org-someday-kwd
  '((t (:weight bold :box (:line-width 1 :color "#9EB6D4")
		:foreground "#9EB6D4" :background "#E0EFFF")))
  "Face used to display state SDAY.")

;; --------------------------------------
;; COMPLETION
;; --------------------------------------

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Show more candidates
  (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Help with keyboard shortcuts
(use-package which-key
  :config
  (which-key-mode t)
  (setq which-key-sort-uppercase-first nil
	max-mini-window-height 15)
  (which-key-setup-side-window-bottom))

;; The `consult' package provides lots of commands that are enhanced
;; variants of basic, built-in functionality.  One of the headline
;; features of `consult' is its preview facility, where it shows in
;; another Emacs window the context of what is currently matched in
;; the minibuffer.  Here I define key bindings for some commands you
;; may find useful.  The mnemonic for their prefix is "alternative
;; search" (as opposed to the basic C-s or C-r keys).
(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))

;; The `embark' package lets you target the thing or context at point
;; and select an action to perform on it.  Use the `embark-act'
;; command while over something to find relevant commands.
;;
;; When inside the minibuffer, `embark' can collect/export the
;; contents to a fully fledged Emacs buffer.  The `embark-collect'
;; command retains the original behaviour of the minibuffer, meaning
;; that if you navigate over the candidate at hit RET, it will do what
;; the minibuffer would have done.  In contrast, the `embark-export'
;; command reads the metadata to figure out what category this is and
;; places them in a buffer whose major mode is specialised for that
;; type of content.  For example, when we are completing against
;; files, the export will take us to a `dired-mode' buffer; when we
;; preview the results of a grep, the export will put us in a
;; `grep-mode' buffer.
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.
(use-package embark-consult
  :ensure t)

;;; ----------------
;;; world-clock
;;; ----------------

(setq world-clock-list
      '(("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Europe/Sofia" "Sofia")
        ("Asia/Calcutta" "Bangalore")
        ("Asia/Tokyo" "Tokyo")
	("Pacific/Noumea" "Noumea")))

;;; ----------------
;;; EIN
;;; ----------------

(use-package ein
  :ensure t)

;;; -------------------------
;;; PO mode for translations
;;; -------------------------

(use-package po-mode
  :ensure nil
  :config
  (autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)
  (setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist)))

;;; -------------------------
;;; Denote
;;; -------------------------

(use-package denote
  :ensure t
  :bind ("C-c d" . denote-open-or-create)
  :custom
  (denote-directory "/home/ghislain/kDrive/Notes")
  :config
  (setq denote-org-front-matter
  "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
#+options: ^:{}
\n"))

;;; -------------------------
;;; Translate
;;; -------------------------

;; Install the curl program and the plz.el package.
;; The request from go-translate will then be sent through curl,
;; which is much better than the built-in url.el
(use-package plz
  :ensure t)

(use-package go-translate
  :ensure t
  :after plz
  :config
  (setq gt-langs '(en es))
  (setq gt-default-translator
	(gt-translator
	 :engines (gt-deepl-engine :key "7a3ff352-c4cb-4efe-9f71-94a992e5352b:fx")
	 :render (gt-insert-render :type 'replace))))

;; --------------------------
;; Ebooks
;; --------------------------

;; Read ePub files
(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; --------------------------
;; Calendar
;; --------------------------

;; Latitude and longitude for sunrise and sunset
;; M-x sunrise-sunset
(setq calendar-latitude 43.63) ;; -22.28
(setq calendar-longitude 3.86) ;; 166.46
(setq calendar-location-name "Montpellier, France")
;; Weeks start on Monday
(setq calendar-week-start-day 1)

;; --------------------------
;; org-player
;; --------------------------

(use-package org-player
  :load-path "~/.config/emacs/org-player"
  :config
  (load "org-player.el"))

;; -------------------------
;; org-ai
;; -------------------------

(use-package org-ai
  :load-path "~/.config/emacs/org-ai/"
  :commands (org-ai-mode
             org-ai-global-mode)
  :defines org-ai-default-chat-model org-ai-openai-api-token
  :hook (org-mode-hook . org-ai-mode) ; enable org-ai in org-mode
  :init
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  ;; API token is in .authinfo
  (setq org-ai-default-chat-model "gpt-4o-mini")
  (setq org-ai-image-model "dall-e-3")
  (setq org-ai-image-default-size "1792x1024")
  (setq org-ai-image-default-count 1)
  (setq org-ai-image-default-style 'vivid)
  (setq org-ai-image-default-quality 'hd)
  (setq org-ai-image-directory (expand-file-name "images/org-ai/" org-directory)))

;; Setting up speech input / output
(use-package whisper
  :load-path "~/.config/emacs/whisper/"
  :bind ("M-s-r" . whisper-run) ; s is Windows key
  :init
  (load "whisper.el")
  :config
  (setq whisper-install-directory "/home/ghislain/Applications/"
	whisper-model "base"
        whisper-language "en"
        whisper-translate nil
	whisper-use-threads (/ (num-processors) 2)))

(use-package greader
  :ensure t)
(require 'whisper)
(require 'org-ai-talk)

;;; ----------------
;;; dotemacs.el ends here
