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

;;;; directories

(if (boundp 'user-org-directory)
    (setq org-directory user-org-directory)
  (setq org-directory "~/org/"))

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


;;;; cc/url-bookmarks

(setq cc/url-bookmarks
      '(("Google" . "https://www.google.com")
        ("GitHub" . "https://github.com")
        ("Emacs Home" . "https://www.gnu.org/software/emacs/")
        ;; 여기에 원하는 URL을 추가하세요
        ))
