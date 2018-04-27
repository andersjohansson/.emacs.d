;; -*- lexical-binding: t -*-

;;; Ta bort buffer och fil
;; från http://whattheemacsd.com/file-defuns.el-02.html
;;;###autoload
(defun aj/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;; http://whattheemacsd.com/file-defuns.el-01.html
;;;###autoload
(defun aj/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))


;;; beginning-or-indentation
;;Från http://www.emacswiki.org/emacs/misc-cmds.el
;;;###autoload
(defun aj/beginning-or-indentation ()
  "Move cursor to beginning of this line or to its indentation.
If at indentation position of this line, move to beginning of line.
If at beginning of line, move to beginning of previous line.
Else, move to indentation position of this line.

With arg N, move backward to the beginning of the Nth previous line.
Interactively, N is the prefix arg."
  (interactive)
  (cond ((bolp) (back-to-indentation))
        ((save-excursion (skip-chars-backward " \t") (bolp)) ; At indentation.
         (forward-line 0))
        (t (back-to-indentation))))

;;; Insert filename
;; From
;; http://stackoverflow.com/questions/16764502/insert-filename-using-ido
;;;###autoload
(defun aj/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

;;; write-string-to-file
;; Från http://www.emacswiki.org/emacs/ElispCookbook
;;;###autoload
(defun aj/write-string-to-file (string file)
  (interactive "sEnter the string: \nFFile to save to: ")
  (with-temp-buffer
	(insert string)
	(when (file-writable-p file)
	  (write-region (point-min)
					(point-max)
					file))))

;;;###autoload
(defun aj/read-string-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


;;; lägg senast modifierad till filnamn
;;;###autoload
(defun aj/last-mod-rename (files)
  "Lägger till mtime som Y-m-d i filnamn hos en fil eller flera."
  (interactive "FFil: ")
  (unless (listp files) (setq files (list files)))
  (dolist (file files)
	(when (file-writable-p file)
	  (let ((mtime (format-time-string "%Y-%m-%d-" (nth 5 (file-attributes file)))))
		(rename-file file (concat (file-name-directory file) mtime (file-name-nondirectory file)))))))

;; lägg till den till dired actions.
;;;###autoload
(defun aj/dired-do-last-mod-rename ()
  (interactive)
  (let ((files (dired-get-marked-files)))
	(when (y-or-n-p "Lägg till mtime till filnamnen? ")
	  (aj/last-mod-rename files))))


;;; aj/org-commit-template
;;;###autoload
(defun aj/org-commit-template (&optional prepend dir level)
  "Antar att DIR är ett git-rep. Om level ges så bara headlines av den leveln, annars alla nya. Prepend läggs längst fram i filen."
  (with-temp-buffer
	(when dir (cd dir))
	(message "defdir %s" default-directory)
	(aj/write-string-to-file
	 (let ((tmp (or prepend ""))
		   (hregx (format
                   "^+\\*\\{%d,%d\\}\\(?: +\\)?\\(?: +\\(?:\\[#.\\]\\)\\)?\\(?: +\\(.*?\\)\\)??\\(?:[ 	]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?[ 	]*$"
                   (or level 1)
                   (or level (1- org-inlinetask-min-level)))))
	   (insert (shell-command-to-string "git diff --cached"))
	   (goto-char (point-min))
	   (while (re-search-forward hregx nil t)
		 (setq tmp (concat tmp (match-string-no-properties 1) "\n")))
	   tmp)
	 ".git/info/commit-template.txt")))

;;;;Ido-completing-read-multiple, mkt bra.

;; (defun ido-completing-read-multiple (prompt choices &optional predicate require-match initial-input hist def sentinel)
;;   "Read multiple items with ido-completing-read. Reading stops
;; when the user enters SENTINEL. By default, SENTINEL is
;; \"*done*\". SENTINEL is disambiguated with clashing completions
;; by appending _ to SENTINEL until it becomes unique. So if there
;; are multiple values that look like SENTINEL, the one with the
;; most _ at the end is the actual sentinel value. See
;; documentation for `ido-completing-read' for details on the
;; other parameters."
;;   (let
;; 	  (this-choice
;;        (sentinel (if sentinel sentinel "*done*"))
;; 	   (done-reading nil)
;; 	   (res ()))

;; 	;; uniquify the SENTINEL value
;; 	(while (cl-find sentinel choices)
;; 	  (setq sentinel (concat sentinel "_")))
;; 	(setq choices (cons sentinel choices))

;; 	;; read some choices
;; 	(while (not done-reading)
;; 	  (setq this-choice (ido-completing-read prompt choices predicate require-match initial-input hist def))
;; 	  (if (equal this-choice sentinel)
;; 		  (setq done-reading t)
;; 		(setq res (cons this-choice res))))

;; 	;; return the result
;; 	res))

;;;###autoload
(defun aj/helm-completing-read-multiple (prompt choices &optional predicate require-match initial-input hist def inherit-input-method name buffer sentinel)
  "Read multiple items with `helm-completing-read-default-1'. Reading stops
when the user enters SENTINEL. By default, SENTINEL is
\"*done*\". SENTINEL is disambiguated with clashing completions
by appending _ to SENTINEL until it becomes unique. So if there
are multiple values that look like SENTINEL, the one with the
most _ at the end is the actual sentinel value. See
documentation for `ido-completing-read' for details on the
other parameters."
  (let ((sentinel (if sentinel sentinel "*done*"))
        this-choice res done-reading)
	;; uniquify the SENTINEL value
	(while (cl-find sentinel choices)
	  (setq sentinel (concat sentinel "_")))
	(setq choices (cons sentinel choices))
	;; read some choices
	(while (not done-reading)
	  (setq this-choice
            (helm-completing-read-default-1
             prompt choices predicate require-match initial-input hist def inherit-input-method
             name buffer nil t))
	  (if (equal this-choice sentinel)
		  (setq done-reading t)
		(setq res (cons this-choice res))))
    ;; return the result
	res))


;;; sudo-edit

;; (defvar find-file-root-history nil
;;   "History list for files found using `find-file-root'.")

;;;###autoload
(defvar aj/sudo-edit-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

;;;###autoload
(defun aj/sudo-edit (&optional arg)
  (interactive "P")
  (let* ((name (if (equal major-mode 'dired-mode)
                   (if (y-or-n-p "Sudo in selected file (else sudo dired)")
                       (dired-get-file-for-visit)
                     default-directory)
                 (or (buffer-file-name) default-directory)))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         (file (if tramp
                   (concat "/" (tramp-file-name-hop tramp)
                           (tramp-file-name-method tramp) ":"
                           (when (tramp-file-name-user tramp)
                             (concat (tramp-file-name-user tramp) "@"))
                           (tramp-file-name-host tramp)
                           "|sudo:" (tramp-file-name-host tramp) ":"
                           (tramp-file-name-localname tramp))
                 (concat "/sudo::" name))))
    (if arg ;;choose file
        (find-file (concat "/sudo::" (read-file-name "Sudo edit: ")))
      (if (buffer-file-name)
          (let ((pos (point)))
            (find-alternate-file file)
            ;;(find-file file)
            (goto-char pos))
        (find-file file)))
    ;; (setq find-file-root-history file-name-history)
    (run-hooks 'aj/sudo-edit-hook)))



;;; Rensa upp tex-kataloger
;;;###autoload
(defun aj/clean-tex-dirs (dirs)
  (interactive (list (list (read-directory-name "Directory: "))))
  (save-window-excursion
    (let (to-relink to-delete buf)
      (dolist (dir dirs)
        (when (file-directory-p dir)
          (let ((linkfiles (directory-files dir t "pdf\\(pc\\)?$")))
            (dolist (f linkfiles)
              (let ((ft (file-truename f)))
                (when (and (not (string= f ft)) (string-match "\\.tex-aux" ft))
                  (push (cons f ft) to-relink)))))
          (when (file-directory-p (expand-file-name ".tex-aux" dir))
            (push (expand-file-name ".tex-aux" dir) to-delete))
          (when (file-directory-p (expand-file-name ".auctex-auto" dir))
            (push (expand-file-name ".auctex-auto" dir) to-delete))))

      (when (or to-relink to-delete)
        (setq buf (switch-to-buffer (generate-new-buffer "*clean-tex-dirs*")))
        (insert "---- Länka om ----\n")
        (dolist (tr to-relink)
          (insert (car tr) " ⮕ " (cdr tr) "\n"))
        (insert "\n---- Ta bort ----\n")
        (dolist (d to-delete)
          (insert d "\n"))
        (if (y-or-n-p "Länka om och ta bort? ")
            (progn
              (dolist (tr to-relink)
                (delete-file (car tr))
                (when (file-exists-p (cdr tr))
                  (copy-file (cdr tr) (car tr) t t)))
              (dolist (d to-delete)
                (delete-directory d t t))))
        (kill-buffer buf)))))

;;;###autoload
(defun aj/clean-all-tex-dirs ()
  (interactive)
  (aj/clean-tex-dirs
   (cl-delete-duplicates
    (split-string
     (shell-command-to-string "find ~/ -name '*.tex' -printf '%h\n'")
     "\n") :test 'string=)))


;;;###autoload
(defun aj/annotations-from-läsplattan ()
  (interactive)
  (let* ((dbfile "/run/media/aj/READER/Sony_Reader/database/books.db")
         (dbfile (if (file-readable-p dbfile)
                     dbfile
                   (read-file-name "Database file (books.db)" nil nil t)))
         (callstring (format
                      "sqlite3 -csv \"%s\" \"%%s\""
                      (url-encode-url (concat "file://" dbfile "?mode=ro"))))
         books bookid annotations)
    (with-temp-buffer
      (call-process-shell-command
       (format callstring "select _id, title, file_name from books")
       nil t)
      (setq books (pcsv-parse-buffer)))
    (unless books
      (user-error "No books?"))
    (setq bookid
          (helm-comp-read "Choose book: "
                          (cl-loop for b in books
                                   collect
                                   (cons (concat
                                          (nth 1 b)
                                          "	"
                                          (propertize (nth 2 b) 'face 'shadow))
                                         (car b)))
                          :must-match t :fuzzy t))
    (with-temp-buffer
      (call-process-shell-command
       (format callstring
               (format
                "select page, markup_type, name, marked_text from annotation where content_id = %s order by page"
                bookid))
       nil t)
      (setq annotations (pcsv-parse-buffer)))
    (if annotations
        (progn (switch-to-buffer (generate-new-buffer "*PRS annotations*"))
               (dolist (a annotations)
                 (when (equal "11" (nth 1 a))
                   (insert (nth 2 a) ":\n"))
                 (insert "#+BEGIN_QUOTE\n"
                         (nth 3 a)
                         "\n"
                         "(p. " (car a) ")\n"
                         "#+END_QUOTE\n\n"))
               (goto-char (point-min))
               (org-mode))
      (message "No annotations found for that book"))))

;;;###autoload
(defun ergoemacs-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app."
  (interactive)
  (let* ((myFileList
          (cond
           (file (list file))
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
                                        ;((ido-active) (prog1 (list (ido-name (car ido-matches))) (exit-minibuffer)))
           ((ffap-guesser) (list (ffap-guesser)))
           (t (list (buffer-file-name)))))
         (doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?"))))
    (when doIt
      (cond
       ;; ((string-equal system-type "windows-nt")
       ;;  (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
       ;;  )
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) myFileList))))) )


;;; Texdoc loading and retrieval. (don’t have complete texdoc installed)
(require 'helm)
(require 'request)

(defvar aj/texdoc-history nil)

;;;###autoload
(defun aj/texdoc ()
  (interactive)
  (let* ((helm-boring-file-regexp-list
          (append '("/\\.$" "/\\.\\.$")
                  helm-boring-file-regexp-list))
         (doc
          (helm :sources
                (list (helm-build-sync-source "Texdoc"
                        :candidates #'aj/texdoc-cands
                        :action 'identity
                        :fuzzy-match t
                        :candidate-transformer #'helm-skip-boring-files
                        :filtered-candidate-transformer #'helm-highlight-files
                        :pattern-transformer #'helm-recentf-pattern-transformer)
                      (helm-build-dummy-source "Search and download"
                        :action 'aj/texdoc-search-and-download))
                :buffer "*helm-texdoc*"
                :history 'aj/texdoc-history
                :ff-transformer-show-only-basename t))
         (mtime (nth 5 (file-attributes doc)))
         (too-old (< 200 (time-to-number-of-days (time-subtract (current-time) mtime)))))

    (when too-old
      (aj/texdoc-search-and-download (file-name-base doc)))
    (ergoemacs-open-in-external-app doc)))

(defun aj/texdoc-search-and-download (doc)
  (let ((fn (format "~/texdoc/%s.pdf" doc))
        retval)
    (request
     (format "http://texdoc.net/pkg/%s" doc)
     :timeout 5
     :parser 'buffer-string
     :sync t
     :error (cl-function (lambda (&key _data &allow-other-keys)
                           (message "Failed contacting texdoc.net")))
     :success (cl-function
               (lambda (&key data response &allow-other-keys)
                 (if (string= "application/pdf"
                              (request-response-header response "content-type"))
                     (progn
                       (with-temp-buffer
                         (setq buffer-file-coding-system 'raw-text)
                         (insert data)
                         (write-file fn))
                       (setq retval fn))
                   (user-error "Not found")))))
    retval))

(defun aj/texdoc-cands ()
  (when (file-accessible-directory-p "~/texdoc")
    (directory-files "~/texdoc" t)))



(provide 'aj-custom-commands)
