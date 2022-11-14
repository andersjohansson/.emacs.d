;;; org-capture-new-frame.el --- from doom-emacs     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Anders Johansson

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

;; Taken from
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/autoload/org-capture.el

;;; Code:

(require 'org-capture)

;;;###autoload
(defvar org-capture-new-frame-parameters
  `((name . "org-capture")
    (width . 100)
    (height . 40)
    (transient . t))
  "TODO")

;;;###autoload
(defun org-capture-new-frame-cleanup ()
  "Closes the org-capture frame once done adding an entry."
  (when (and (org-capture-new-frame-p)
             (not org-capture-is-refiling))
    (delete-frame nil t)))

;;;###autoload
(defun org-capture-new-frame-p (&rest _)
  "Return non-nil if current frame is an org capture new frame."
  (and (equal (alist-get 'name org-capture-new-frame-parameters)
              (frame-parameter nil 'name))
       (frame-parameter nil 'transient)))

(defmacro org-capture-new-frame--open (&rest body)
  "Execute BODY in new capture frame."
  (declare (indent 0))
  `(let* ((frame-title-format "")
          (frame (if (org-capture-new-frame-p)
                     (selected-frame)
                   (make-frame org-capture-new-frame-parameters))))
     ;; (select-frame-set-input-focus frame)
     ;; fix MacOS not focusing new frames
     (with-selected-frame frame
       (condition-case ex
           (cl-letf (((symbol-function #'pop-to-buffer) #'switch-to-buffer))
             ,@body)
         ('error
          (message "org-capture: %s" (error-message-string ex))
          (delete-frame frame))))))

;;;###autoload
(advice-add 'org-capture :around #'org-capture-new-frame-override)
;;;###autoload
(defun org-capture-new-frame-override (fun &optional goto keys)
  "Open org capture in new frame.
FUN (‘org-capture’ callled with GOTO and KEYS."
  (interactive "P")
  (org-capture-new-frame--open (funcall fun goto keys)))


;; (defun org-capture-new-frame-open (&optional goto keys)
;;   "Open org capture in new frame.
;; GOTO and KEYS as in ‘org-capture’."
;;   (interactive "P")
;;   (org-capture-new-frame--open (org-capture goto keys)))


;; (defun org-capture-new-frame--open (function &rest args)
;;   "Call FUNCTION with ARGS in new capture frame."
;;   ;; (when (and initial-input (string-empty-p initial-input))
;;   ;;   (setq initial-input nil))
;;   ;; (when (and key (string-empty-p key))
;;   ;;   (setq key nil))
;;   (let* ((frame-title-format "")
;;          (frame (if (org-capture-new-frame-p)
;;                     (selected-frame)
;;                   (make-frame org-capture-new-frame-parameters))))
;;     ;; (select-frame-set-input-focus frame)
;;     ;; fix MacOS not focusing new frames
;;     (with-selected-frame frame
;;       (condition-case ex
;;           (cl-letf (((symbol-function #'pop-to-buffer) #'switch-to-buffer))
;;             ;; (switch-to-buffer "*scratch*")
;;             (apply function args)
;;             ;; (let ((org-capture-initial initial-input)
;;             ;;       ;; org-capture-entry
;;             ;;       )
;;             ;;   ;; (when (and key (not (string-empty-p key)))
;;             ;;   ;;   (setq org-capture-entry (org-capture-select-template key)))
;;             ;;   (funcall function nil key))
;;             )
;;         ('error
;;          (message "org-capture: %s" (error-message-string ex))
;;          (delete-frame frame))))))

(add-hook 'org-capture-after-finalize-hook #'org-capture-new-frame-cleanup)
(advice-add 'org-capture-refile :after #'org-capture-new-frame-cleanup)

(provide 'org-capture-new-frame)
;;; org-capture-new-frame.el ends here
