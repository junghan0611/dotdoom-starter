;;; $DOOMDIR/+user-info.el -*- lexical-binding: t; -*-

;; User Identify (optional)
;; e.g. GPG configuration, email clients, file templates and snippets
(setq user-full-name "junghanacs"
      user-mail-address "junghanacs@gmail.com")

(setq doom-font (font-spec :family "Monoplex Nerd" :size 14.0)
      doom-big-font (font-spec :family "Monoplex Nerd" :size 24.0)
      doom-variable-pitch-font (font-spec :family "Pretendard Variable" :size 14.0)
      doom-unicode-font (font-spec :family "Symbola" :size 14.0))

(unless (display-graphic-p) ; terminal
  (setq doom-font (font-spec :family "Sarasa Term K Nerd Font" :size 15.1)))

;;;; directory path

(defconst user-org-directory (if (getenv "ORG_DIRECTORY")
                                 (getenv "ORG_DIRECTORY")
                               "~/org/"))

(defconst user-project-directory (if (getenv "PROJECT_DIRECTORY")
                                     (getenv "PROJECT_DIRECTORY")
                                   "~/repos/"))

;;;; directories

(if (boundp 'user-org-directory)
    (setq org-directory user-org-directory)
  (setq org-directory "~/org/"))

;;;; cc/url-bookmarks

(setq cc/url-bookmarks
      '(("Google" . "https://www.google.com")
        ("GitHub" . "https://github.com")
        ("Emacs Home" . "https://www.gnu.org/software/emacs/")
        ;; 여기에 원하는 URL을 추가하세요
        ))
