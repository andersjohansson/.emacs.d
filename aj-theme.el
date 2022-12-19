;;; aj-theme.el --- Custom face settings applied on top of modus themes -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'modus-themes)
(require 'cl-lib)

(deftheme aj "Customizations on top of other themes."
          :background-mode 'light
          :kind 'color-scheme
          :theme-immediate t)

(modus-themes-with-colors
  (custom-theme-set-faces
   'aj
   '(default ((t (:height 113))))
   '(variable-pitch ((t (:family "Inter"))))

   ;; modus-operandi sets it to inherit fixed-pitch, but that is
   ;; bad with the detailed space matching I do!
   '(org-indent ((t :inherit org-hide)))
   '(org-column ((t :inherit fixed-pitch)))
   '(org-column-title ((t :inherit fixed-pitch)))
   '(org-cite ((t :inherit modus-themes-prompt)))
   ;; only visible when ‘org-cite-csl-activate’ isn’t active
   '(org-cite-key ((t :inherit modus-themes-prompt :underline "light grey")))
   '(org-document-title ((t :height 1.5 :weight bold)))
   `(org-block-begin-line nil ((t :overline ,bg-active :height 0.9)))
   `(org-block-end-line ((t :underline (:color ,bg-active) :overline nil :height 0.9)))
   '(org-block ((t :background "#f8f8f8")))
   '(org-quote ((t :height 0.95 :background unspecified)))

   ;; ;; Use fixed-pitch for checkbox, compressed font for table
   '(org-checkbox ((t :inherit fixed-pitch)))
   '(org-table ((t :family "Input Mono Compressed")))


   '(org-habit-alert-face ((t :inherit fixed-pitch)))
   '(org-habit-alert-future-face ((t :inherit fixed-pitch)))
   '(org-habit-clear-face ((t :inherit fixed-pitch)))
   '(org-habit-clear-future-face ((t :inherit fixed-pitch)))
   '(org-habit-overdue-face ((t :inherit fixed-pitch)))
   '(org-habit-overdue-future-face ((t :inherit fixed-pitch)))
   '(org-habit-ready-face ((t :inherit fixed-pitch)))
   '(org-habit-ready-future-face ((t :inherit fixed-pitch)))

   ;; '(org-agenda-date-today ((t :background 'unspecified)))
   ;;(set-face-attribute 'org-block nil :background "#f8f8f8")

   `(org-modern-tag ((t :box (:color ,bg-inactive :line-width (0 . -3)) :background ,bg-inactive :inherit org-modern-label)))

   '(idle-highlight ((t :inherit nil :underline t)))

   '(mode-line-buffer-id ((t :inherit variable-pitch :weight regular)))

   `(mini-modeline-mode-line-inactive ((t :background ,bg-active :height 0.14)))
   `(mini-modeline-mode-line-active ((t :background ,bg-cyan :height 0.14)))

   ;; modus let’s it inherit `shadow’ for no good reason
   '(mu4e-header-face ((t :inherit nil)))
   '(lin-blue ((t :background "#def3ff"))) ; a bit lighter than default

   `(diff-hl-insert ((t :inverse-video t :foreground ,(face-background 'fringe))))
   `(diff-hl-delete ((t :inverse-video t :foreground ,(face-background 'fringe))))
   `(diff-hl-change ((t :inverse-video t :foreground ,(face-background 'fringe))))
   ))

(provide-theme 'aj)
;;; aj-theme.el ends here


