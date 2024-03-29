;;; doc-font.el --- Use a special font for documents  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Keywords: wp

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

;;; Commentary:

;;

;;; Code:

(require 'face-remap)
(require 'org nil t)
(require 'org-modern nil t)
(require 'org-indent nil t)


(defgroup doc-font nil "Customizations of ‘doc-font-mode’."
  :group 'faces)

(defcustom doc-font-attributes
  '((default . (:family "Crimson Pro" :height 1.25))
    (org-document-title . (:height 1.2))
    (org-level-1 . (:height 1.4 :weight normal ))
    (org-level-2 . (:height 1.3 :weight normal :slant italic))
    (org-level-3 . (:height 1.2 :weight normal :slant italic))
    (org-level-4 . (:height 1.1 :slant italic))
    (org-level-5 . (:height 1.0 :weight semibold))
    (org-level-6 . (:height 1.0 :weight semibold))
    (org-level-7 . (:height 1.0 :weight semibold))
    (org-level-8 . (:height 1.0 :weight semibold))
    ;;(org-block (:background ,base02))
    (org-link . (:underline t :weight normal))
    (org-special-keyword . (:height 0.9))
    (org-table . (:height 0.6))  ;; means most tables may fit.
    (org-drawer . (:height 0.6))
    (org-special-keyword . (:height 0.6))
    (org-property-value . (:height 0.6))
    (org-modern-tag . (:height 0.8)))
  "Faces to remap, with attributes to remap."
  :type '(alist :key-type face :value-type (plist :value-type sexp)))

(defcustom doc-font-keep-default
  '(font-latex-math-face
    font-latex-sedate-face
    font-latex-warning-face
    font-latex-sectioning-5-face
    message-header-name message-header-to
    message-header-cc
    message-header-newsgroups
    message-header-xheader
    message-header-subject
    message-header-other
    mu4e-header-key-face
    mu4e-header-value-face mu4e-link-face
    mu4e-contact-face
    mu4e-compose-separator-face
    mu4e-compose-header-face
    org-block
    org-block-begin-line
    org-block-end-line
    org-document-info-keyword
    org-code
    org-latex-and-related
    org-checkbox
    org-meta-line
    org-verbatim)
  "Faces to avoid remapping.

Height and family is kept from the “default” ‘default’ face."
  :type '(repeat face))

(defcustom doc-font-extra-line-spacing 0.3
  "Extra ‘line-spacing’ used in ‘doc-font-mode’."
  :type '(choice (const :tag "No extra space" nil)
                 (float :tag "Scale to default spacing")))

(defcustom doc-font-modify-parspace 'vertical
  "Replace double line breaks via font-lock.
When ‘vertical’, use small vertical space between paragraphs, when
‘indent’, use paragraph indents."
  :type '(choice (const :tag "No modification" nil)
                 (const :tag "Small vertical space" vertical)
                 (const :tag "Indent" indent)))

(defvar-local doc-font-cookies nil)
(defvar-local doc-font-cookies-def nil)
(defvar-local doc-font-variable-cookies nil)
(defvar-local doc-font-variable-cookies-nokill nil)

(defface doc-font-small-parspace '((t (:height 0.1 :inherit org-hide)))
  "Face for small vertical paragraph spaces.")

(defvar doc-font-parspace-vertical-keyword
  '(("\\(\n\\(\n\\)\\)[^*]" 2 'doc-font-small-parspace)))
(defvar doc-font-parspace-indent-keyword
  '(("\\(\n\n\\)[^*#]" 1
     (list 'face 'default
           'display (concat "\n\t\t")))))

(defmacro doc-font--store-old-val (var)
  "Store value of VAR in ‘doc-font-variable-cookies-nokill’."
  `(push (cons ,var (symbol-value ,var)) doc-font-variable-cookies-nokill))

(defmacro doc-font--set-and-store-old-val (var val)
  "Set buffer-local value of VAR to VAL and store old value.
Old value is stored in ‘doc-font-variable-cookies’."
  `(progn
     (push (cons ,var (symbol-value ,var)) doc-font-variable-cookies)
     (make-local-variable ,var)
     (set ,var ,val)))

;;;###autoload
(define-minor-mode doc-font-mode
  "Change the default face of the current buffer to a “nice” serif font.
Along with some other changes."
  :lighter ""
  (if doc-font-mode
      (let ((default-family (face-attribute 'default :family))
            (default-height (face-attribute 'default :height)))
        (setq doc-font-cookies nil
              doc-font-cookies-def nil
              doc-font-variable-cookies nil
              doc-font-variable-cookies-nokill nil)
        (cl-loop for (face . attr) in doc-font-attributes do
                 (push (apply #'face-remap-add-relative face attr) doc-font-cookies))
        (cl-loop for face in doc-font-keep-default do
                 (push (face-remap-add-relative face :family default-family
                                                :height default-height)
                       doc-font-cookies-def))

        ;; ‘line-spacing’ is buffer-local by default and could be set to
        ;; someting else
        (doc-font--store-old-val 'line-spacing)
        (setq line-spacing doc-font-extra-line-spacing)

        (when (and (derived-mode-p 'org-mode)
                   (not (buffer-base-buffer)))
          (doc-font--store-old-val 'org-indent-mode)
          (org-indent-mode -1)
          (when (bound-and-true-p global-org-modern-mode)
            (doc-font--set-and-store-old-val 'org-modern-hide-stars t)
            (org-modern-mode -1)
            (org-modern-mode)))

        (cond ((eq 'indent doc-font-modify-parspace)
               (make-local-variable 'font-lock-extra-managed-props)
               (add-to-list 'font-lock-extra-managed-props 'display)
               (font-lock-add-keywords nil doc-font-parspace-indent-keyword))
              ((eq 'vertical doc-font-modify-parspace)
               (font-lock-add-keywords nil doc-font-parspace-vertical-keyword)))
        )

    ;; DISABLE:
    (cl-loop for c in (append doc-font-cookies doc-font-cookies-def) do
             (face-remap-remove-relative c))

    ;; restore ‘line-spacing’ to possible previous value
    (setq line-spacing (alist-get 'line-spacing doc-font-variable-cookies-nokill))
    (when
        (alist-get 'org-indent-mode doc-font-variable-cookies-nokill)
      (org-indent-mode))

    (cl-loop for (c . v) in doc-font-variable-cookies do
             (kill-local-variable c))
    (when (bound-and-true-p org-modern-mode)
      (org-modern-mode -1)
      (org-modern-mode))
    (font-lock-remove-keywords nil doc-font-parspace-indent-keyword)
    (font-lock-remove-keywords nil doc-font-parspace-vertical-keyword)
    (when (eq 'indent doc-font-modify-parspace)
      (setq font-lock-extra-managed-props (delete 'display font-lock-extra-managed-props)))
    ))


(provide 'doc-font)

;;; doc-font.el ends here
