;;; $DOOMDIR/+user-info.el -*- lexical-binding: t; -*-

;; User Identify (optional)
;; e.g. GPG configuration, email clients, file templates and snippets
(setq user-full-name "junghanacs"
      user-mail-address "junghanacs@gmail.com")

(setq doom-font (font-spec :family "Monoplex KR Nerd" :size 14.0)
      doom-big-font (font-spec :family "Monoplex KR Nerd" :size 24.0)
      doom-variable-pitch-font (font-spec :family "Pretendard Variable" :size 16.0)
      doom-unicode-font (font-spec :family "Symbola" :size 14.0)
      )

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)

;;;; directory path

(defconst user-org-directory (if (getenv "ORG_DIRECTORY")
                                 (getenv "ORG_DIRECTORY")
                               "~/org/"))

(defconst user-project-directory (if (getenv "PROJECT_DIRECTORY")
                                     (getenv "PROJECT_DIRECTORY")
                                   "~/git/"))

;; org-hugo-base-dir
(defconst user-hugo-blog-dir (concat user-project-directory "blog/"))

;;;; directories

(if (boundp 'user-org-directory)
    (setq org-directory user-org-directory)
  (setq org-directory "~/org/"))

(defun my/expand-org-file-name (filename)
  (expand-file-name filename org-directory))

(defun my/org-index-file () (my/expand-org-file-name "20240429T165725--index.org"))
(defun my/org-now-file () (my/expand-org-file-name "20240618T125104--now.org"))
(defun my/org-about-file () (my/expand-org-file-name "20240326T053829--about.org"))
(defun my/org-contacts-file () (my/expand-org-file-name "20230303T030300--contacts.org"))
(defun my/org-links-file () (my/expand-org-file-name "20230219T035500--links.org"))

(defun my/org-mobile-file () (my/expand-org-file-name "agenda/20240312T111900--mobile.org")) ;; agenda

(defun my/org-inbox-file () (my/expand-org-file-name "20230202T020200--inbox.org"))
(defun my/org-tasks-file () (my/expand-org-file-name "20230101T010100--tasks.org"))
(defun my/org-diary-file () (my/expand-org-file-name "20220101T010100--diary.org"))
(defun my/org-drill-file () (my/expand-org-file-name "20240124T164402--drill.org"))
(defun my/org-quote-file () (my/expand-org-file-name "20240312T031200--quote.org"))
(defun my/org-life-file () (my/expand-org-file-name "20240327T112315--life.org"))

;; org-directory

(defun my/org-reading-file () (my/expand-org-file-name "20240329T154123--reading__lists.org"))

;; meta
(defun my/org-kdc-file () (my/expand-org-file-name "meta/20240312T142358--kdc__meta.org"))
(defun my/org-tags-file () (my/expand-org-file-name "meta/20231005T133900--tags__meta.org"))
(defun my/org-glossary-file () (my/expand-org-file-name "dict/ithink.org"))

;; blog
(defun my/org-blog-file () (my/expand-org-file-name "blog/20240104T061355--blog.org"))

;; talks
(defun my/org-talks-file () (my/expand-org-file-name "talks/20240827T150414--talks.org"))

(defun my/org-remark-file () (my/expand-org-file-name "20231111T094444--remark.org"))
(defun my/org-remember-file () (my/expand-org-file-name "20231020T210500--remember.org"))

;; directory
(defun my/org-calendar-directory () (my/expand-org-file-name ".calendar/"))
(defun my/org-attachment-directory () (my/expand-org-file-name ".attach/"))
(defun my/org-screenshot-directory () (my/expand-org-file-name "screenshot"))

(defvar org-user-agenda-files (list user-org-directory))
(defvar org-screenshot-path (concat user-org-directory "screenshot/"))

;; bib
(setq citar-notes-paths (list (concat user-org-directory "bib/")))
;; (defvar config-bibfiles (list (concat user-org-directory "bib/zotero-biblatex.bib")))
(defvar config-bibfiles (list
                         (concat user-org-directory "resources/zotero-my-library.bib")
                         (concat user-org-directory "resources/zotero-group-junghanacs.bib")))

;; elisp-demos
(setq elisp-demos-user-files (list (concat org-directory "notes/20240926T170706--이맥스리스프-데모__demo_emacslisp_notes.org")))

(defvar org-user-contacts-files (list (my/org-contacts-file)))

(defvar +org-journal-today-file nil)
