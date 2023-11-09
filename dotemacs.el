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
			bibtex-completion
			citeproc
			exec-path-from-shell
			flycheck
			helm-bibtex
			htmlize
			jedi
			leuven-theme
			olivetti
			org
			org-contrib
			org-ref
			org-roam
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

;; --------------------------------------
;; BASIC CUSTOMIZATION
;; --------------------------------------

;; Themes
(load-theme 'leuven t)
;; (load-theme 'material t)
;; (load-theme 'zenburn t)
;; (load-theme 'tango-2 t)

;; Generalities
(setq user-full-name "Ghislain Vieilledent")
(setq user-mail-address "ghislain.vieilledent@cirad.fr")
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
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  :config
  (global-company-mode 1))

;; Company box
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Help with keyboard shortcuts
(use-package which-key
  :config
  (which-key-mode t)
  (setq which-key-sort-uppercase-first nil
	max-mini-window-height 15)
  (which-key-setup-side-window-bottom))

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
;; Icons
;; -------------------------------------

;; Gestion des icônes par all-the-icons
(use-package all-the-icons
  :ensure t)

;; -------------------------------------
;; Dired
;; -------------------------------------

(use-package dired
  :ensure nil
  :defines image-dired-external-viewer
  :config
  (use-package treemacs-icons-dired
    :ensure t
    :if (display-graphic-p)
    :hook (dired-mode . treemacs-icons-dired-mode))
  ;; Split window, Dired tries to guess a default target directory
  (setq dired-dwim-target t
	delete-by-moving-to-trash t)
  ;; Activate dired-x
  (require 'dired-x)
  ;; Define external image viewer/editor
  (setq image-dired-external-viewer "/usr/bin/gimp")
  )

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
  (defvar elfeed-org-files
    (list (expand-file-name "elfeed/elfeed.org" user-emacs-directory))
    "List of elfeed.org files.")
  (setq rmh-elfeed-org-files elfeed-org-files)
  )

;; -------------------------------------
;; PDF-TOOLS
;; -------------------------------------

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (defun gv/turn-off-line-numbers ()
	"Disable line numbering in the current buffer."
	(display-line-numbers-mode -1))
  :hook (pdf-view-mode . gv/turn-off-line-numbers)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))
       
;; -------------------------------------
;; MAGIT
;; -------------------------------------

;; Magit
(use-package magit
  :if (executable-find "git")
  :ensure t
  :bind ("C-x g" . magit-status))

;; -------------------------------------
;; TREE CONFIGURATION
;; -------------------------------------

;; Configuration de Treemacs
;; https://medspx.fr/blog/Debian/emacs_2020
(use-package treemacs
  :ensure t
  :defer t
  :bind ("C-²" . treemacs-select-window)
  :config
  (setq treemacs-width 30
	treemacs-indentation '(6 px)
	treemacs-is-never-other-window t
	treemacs-width-is-initially-locked nil
	treemacs-space-between-root-nodes nil
	treemacs-collapse-dirs 4
	treemacs-sorting 'alphabetic-case-insensitive-asc
	treemacs-text-scale -1)
  ;;(treemacs-indent-guide-mode)
  (treemacs-resize-icons 14)
  (treemacs-follow-mode t)
  (treemacs-tag-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode nil))

;; Treemacs avec support magit
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

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
(defun gv/insert-r-pipe ()
  "Insert the pipe operator in R, |>."
  (interactive)
  (just-one-space 1)
  (insert "|>")
  (reindent-then-newline-and-indent))

;; Controlling buffer display
;; (see section in http://ess.r-project.org/ess.pdf)
(defun gv/ess-display-buffer ()
  "Displaying buffers with ESS."
  (setq display-buffer-alist
	`(("^\\*R"
           (display-buffer-reuse-window display-buffer-in-side-window)
	   (side . right)
	   (slot . -1)
           (window-width . 0.33)
           (reusable-frames . nil)
	   (dedicated . t))
          ("^\\*Help"
           (display-buffer-reuse-window display-buffer-in-side-window)
	   (side . right)
           (slot . 1)
           (window-width . 0.33)
           (reusable-frames . nil))))
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
  :hook ((ess-mode . gv/ess-display-buffer)
	 (ess-mode . rainbow-delimiters-mode))
  :commands R
  :bind (:map ess-r-mode-map
              (";" . ess-insert-assign)
              ;; RStudio equivalents
              ("M--" . ess-insert-assign)
              ("C-S-m" . gv/insert-r-pipe)
              :map inferior-ess-r-mode-map
              (";" . ess-insert-assign)
              ("M--" . ess-insert-assign)
              ("C-S-m" . gv/insert-r-pipe))
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
  (setq ispell-dictionary "fr_FR,en_GB,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "fr_FR,en_GB,en_US")
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

;; Directories for bibliography
(setq bib-files (directory-files
                 (concat (getenv "HOME") "/Documents/Bibliography") t ".bib$")
      bib-file-default (concat (getenv "HOME") "/Documents/Bibliography/biblio.bib")
      bib-files-directory (concat (getenv "HOME") "/Documents/Bibliography/")
      pdf-files-directory (concat (getenv "HOME") "/Documents/Bibliography/Articles")
      bib-notes-directory (concat (getenv "HOME") "/Documents/Bibliography/Notes")
      bib-notes-file (concat (getenv "HOME") "/Documents/Bibliography/Notes/notes_work.org"))

(require 'tex-site)
(setq reftex-bibpath-environment-variables
      bib-files-directory) ; biblio
(setq reftex-default-bibliography
      bib-file-default)
(setq TeX-PDF-mode t) ; PDFLatex by default

;; Max 90 characters
(setq tex-mode-hook
      (lambda ()
	(auto-fill-mode t)
 	(setq fill-column 90)
 	))

;; BibLaTeX settings
;; bibtex-mode
(setq bibtex-dialect 'biblatex)

;; Spell checking (requires the ispell software)
(add-hook 'bibtex-mode-hook 'flyspell-mode)

;; helm-bibtex
(use-package helm-bibtex
    :ensure t
    :config
    (setq bibtex-completion-bibliography bib-files
          bibtex-completion-library-path pdf-files-directory
          bibtex-completion-pdf-field "File"
          bibtex-completion-notes-path bib-notes-directory)
    :bind
    (("<menu>" . helm-command-prefix)
     :map helm-command-map
     ("b" . helm-bibtex)
     ("<menu>" . helm-resume)))

;; org-ref !! PROBLEM with org-bibtex <=> ol-bibtex
(use-package org-ref
    :config
    (setq org-ref-completion-library 'org-ref-helm-cite
          org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
          org-ref-default-bibliography bib-file-default
          org-ref-notes-directory bib-notes-directory))

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
;;(dolist (var '("WDPA_KEY"))
;;   (add-to-list 'exec-path-from-shell-variables var))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; essh is for bash what ess is for R
;; https://www.emacswiki.org/emacs/essh.el
;; Load additional libraries
(defun gv/essh-sh ()
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
  :hook (sh-mode . gv/essh-sh))

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
  (defun gv/python-shell-send-line ()
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
			  ("C-<backspace>" . gv/python-shell-send-line))
  :config
  (setq python-indent-offset 4))
  
;; --------------------------------------
;; PYTHON VIRTUAL ENVIRONMENT
;; --------------------------------------

;; Virtual Env
(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions/miniconda3-latest/envs")
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
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c S-t") 'org-babel-execute-subtree)
(global-set-key (kbd "C-c o")
		(lambda () (interactive) (find-file (concat org-directory "/notes_work.org"))))

;; Settings
(setq org-hide-leading-stars t
      org-startup-indented t)
(setq org-alphabetical-lists t)
;;(setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
(setq org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
(setq org-fontify-whole-heading-line t) ;; Fontify the whole line for headings (with a background color).
(setq org-src-tab-acts-natively t) ;; you want this to have completion in blocks
(setq org-src-preserve-indentation t)
(setq org-export-with-smart-quotes t)
(setq org-edit-src-content nil)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width 600)
;; In css mode, htmlize uses cascading style sheets to specify colors
(require 'htmlize)
(setq org-html-htmlize-output-type 'css)

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
(defun gv/beamer-bold (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\structure" contents)))
(defun gv/beamer-underline (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))
(defun gv/beamer-strike (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\alert" contents)))
(add-to-list 'org-export-filter-bold-functions 'gv/beamer-bold)
(add-to-list 'org-export-filter-underline-functions 'gv/beamer-underline)
(add-to-list 'org-export-filter-strike-through-functions 'gv/beamer-strike)


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

;; Processor for org-cite
(require 'oc-csl)
(require 'oc-natbib)

;; Multilines bold
(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 1000) ;; Up to 100 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

;; Templates
(defvar rrmooc/new-org-templates (version<= "9.2" (org-version))
  "Boolean for version of org <= 9.2.")
(when  rrmooc/new-org-templates
  (require 'org-tempo))

(require 'subr-x)
(defun rrmooc/add-org-template (old-style-template)
  "Defining new org templates.
This function trasnorms OLD-STYLE-TEMPLATE in new style template"
  (add-to-list 'org-structure-template-alist
	       (if rrmooc/new-org-templates
		   (cons
		    (nth 0 old-style-template)
		    (string-trim-right (substring (nth 1 old-style-template) 8 -9)))
		 old-style-template)))

(unless rrmooc/new-org-templates
  ;; this template is predefined in the new templating system
  (rrmooc/add-org-template
   '("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")))

(rrmooc/add-org-template
 '("m" "#+begin_src emacs-lisp\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

(rrmooc/add-org-template
 '("r" "#+begin_src R :results output :session *R* :exports both\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(rrmooc/add-org-template
 '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(rrmooc/add-org-template
 '("RR" "#+begin_src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(rrmooc/add-org-template
 '("p" "#+begin_src python :results output :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(rrmooc/add-org-template
 '("P" "#+begin_src python :results output :session :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(rrmooc/add-org-template
 '("PP" "#+begin_src python :results file :session :var matplot_lib_filename=(org-babel-temp-file \"figure\" \".png\") :exports both\nimport matplotlib.pyplot as plt\n\nimport numpy\nx=numpy.linspace(-15,15)\nplt.figure(figsize=(10,5))\nplt.plot(x,numpy.cos(x)/x)\nplt.tight_layout()\n\nplt.savefig(matplot_lib_filename)\nmatplot_lib_filename\n#+end_src" "<src lang=\"python\">\n\n</src>"))

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
  (setq org-agenda-include-diary t)
  (setq org-agenda-files '("~/kDrive/Notes/notes_work.org"
						   "~/kDrive/Notes/notes_perso.org"
						   "~/kDrive/Notes/todos.org"
						   "~/kDrive/Notes/events.org")))

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
(defvar gv/org-default-todos-file "~/kDrive/Notes/todos.org"
  "Default file for todos.")
(defvar gv/org-default-events-file "~/kDrive/Notes/events.org"
  "Default file for events.")

;; org-capture-templates
(setq org-capture-templates
      '(("t" "Task (work)" entry
	 (file+headline gv/org-default-todos-file "Work tasks")
	 "*** TODO %?")
	("T" "Task (perso)" entry
	 (file+headline gv/org-default-todos-file "Personal tasks")
	 "*** TODO %?")
	("m" "Meeting (work)" entry
	 (file+headline gv/org-default-events-file "Work meetings")
	 "** %?\n%t")
	("M" "Meeting (personal appointment)" entry
	 (file+headline gv/org-default-events-file "Personal appointments")
	 "** %?\n%t")
	("e" "Event (non professional event)" entry
	 (file+headline gv/org-default-events-file "Other events")
	 "** %?\n%t")
	("n" "Note" entry
	 (file+headline org-default-notes-file "Notes")
	 "** %?\n%t" :empty-lines 1)))

;; todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "STRT(r)" "WAIT(w)" "SDAY(s)" "|" "DONE(d)" "CANX(c)")))

;; faces for specific TODO keywords
(setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("STRT" . leuven-org-inprogress-kwd)
        ("WAIT" . leuven-org-waiting-for-kwd)
        ("SDAY" . leuven-org-someday-kwd)
        ("DONE" . org-done)
        ("CANX" . org-done)))

;; Org non-standard faces
(defface leuven-org-inprogress-kwd
  '((t (:weight bold :box (:line-width 1 :color "#D9D14A")
		:foreground "#D9D14A" :background "#FCFCDC")))
  "Face used to display state STRT.")
(defface leuven-org-waiting-for-kwd
  '((t (:weight bold :box (:line-width 1 :color "#89C58F")
		:foreground "#89C58F" :background "#E2FEDE")))
  "Face used to display state WAIT.")
(defface leuven-org-someday-kwd
  '((t (:weight bold :box (:line-width 1 :color "#9EB6D4")
		:foreground "#9EB6D4" :background "#E0EFFF")))
  "Face used to display state SDAY.")

;; No bookmark with capture
(setq org-capture-bookmark nil)

;; --------------------------------------
;; ORG ROAM
;; --------------------------------------

(use-package org-roam
  :ensure t
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (concat (getenv "HOME") "/kDrive/Notes"))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

;; --------------------------------------
;; HELM
;; --------------------------------------

;; icons (treemacs icons by default)
;; https://github.com/yyoncho/helm-icons
(use-package helm-icons
  :ensure t
  :config
  ;; set either to 'treemacs or 'all-the-icons
  (setq helm-icons-provider 'treemacs))
  
;; helm completion system
(use-package helm
  :ensure t
  :after helm-icons
  :init
  (helm-mode 1)
  (helm-icons-enable)
  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-mini)
   ("C-x C-r" . helm-recentf)
   ("C-c i"   . helm-imenu)
   ("M-y"     . helm-show-kill-ring)
   :map helm-map
   ("C-z" . helm-select-action)
   ("<tab>" . helm-execute-persistent-action)))

;;; ----------------
;;; dotemacs.el ends here
