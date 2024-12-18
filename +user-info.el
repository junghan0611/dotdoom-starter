;;; $DOOMDIR/+user-info.el -*- lexical-binding: t; -*-

;; User Identify (optional)
;; e.g. GPG configuration, email clients, file templates and snippets
(setq user-full-name "junghanacs"
      user-mail-address "junghanacs@gmail.com")

(setq doom-font (font-spec :family "Monoplex KR Nerd" :size 14.0)
      doom-big-font (font-spec :family "Monoplex KR Nerd" :size 24.0)
      doom-variable-pitch-font (font-spec :family "Pretendard Variable" :size 16.0)
      doom-unicode-font (font-spec :family "Symbola" :size 14.0))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; (setq doom-theme 'doom-homage-white)

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

(defconst user-inbox-file "20230202T020200--inbox-now__aprj.org")
(defun my/org-inbox-file () (my/expand-org-file-name user-inbox-file))

(defun my/org-contacts-file () (my/expand-org-file-name "20230303T030300--contacts.org"))
(defun my/org-mobile-file () (my/expand-org-file-name "agenda/20240312T111900--mobile.org")) ;; agenda
(defun my/org-diary-file () (my/expand-org-file-name "20220101T010100--diary.org"))
(defun my/org-remember-file () (my/expand-org-file-name "20231020T210500--remember.org"))

;; directory
(defun my/org-calendar-directory () (my/expand-org-file-name ".calendar/"))
(defun my/org-attachment-directory () (my/expand-org-file-name ".attach/"))
(defun my/org-screenshot-directory () (my/expand-org-file-name "screenshot"))

(defvar org-user-agenda-files (list user-org-directory))
(defvar org-screenshot-path (concat user-org-directory "screenshot/"))

;; bib
(setq citar-notes-paths (list (concat user-org-directory "bib/")))
(defvar config-bibfiles (list
                         (concat user-org-directory "resources/Slipbox.bib")
                         (concat user-org-directory "resources/Book.bib")
                         (concat user-org-directory "resources/Category.bib")))

;; elisp-demos
(setq elisp-demos-user-files
      (list (concat org-directory "/notes/20240926T170706--elisp-demos__emacslisp_notes.org")))

(defvar org-user-contacts-files (list (my/org-contacts-file)))

(defvar +org-journal-today-file nil)

;;;; fortune

(setq user-initial-scratch-message
      (format "%s"
              (if (executable-find "fortune")
                  (string-join
                   (mapcar
                    (lambda (l) (concat "\n " (string-fill l 72)))
                    (if (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
                        (string-lines (shell-command-to-string "fortune"))
                      (string-lines
                       (shell-command-to-string
                        "fortune -c 90% advice 10% .")))))
                ("\nLearn how to take a 20-minute power nap without embarrassment.\n"))
              "\n"))
