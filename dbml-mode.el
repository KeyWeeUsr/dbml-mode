;;; dbml-mode.el --- Major mode for DBML -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, dbml, language, markup, highlight
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))
;; Homepage: https://github.com/KeyWeeUsr/dbml-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This major mode attempts to port all of the syntax highlighting from
;; https://dbdiagram.io and build upon it by providing helpers such as
;; duplicate checking and rendering SVGs directly in Emacs.

;;; Code:

;; TODO: pull constant patterns away, out, to defconst
(require 'subr-x)

(defgroup dbml
  nil
  "Customization group for `dbml-mode'."
  :group 'convenience
  :group 'external
  :group 'programming)

(defcustom dbml-mode-render-dockerized
  t
  "Use dockerized renderer."
  :group 'dbml
  :type 'boolean)

(defcustom dbml-mode-render-bin
  "dbml-renderer"
  "Path to `dbml-renderer' or other compatible binary."
  :group 'dbml
  :type 'string)

(defcustom dbml-mode-build-image-name
  "dbml-mode-render"
  "Image name to use for =docker build --tag=."
  :group 'dbml
  :type 'string)

(defcustom dbml-mode-use-image-name
  nil
  "Image to use for =docker run=, overrides `dbml-mode-build-image-name'."
  :group 'dbml
  :type 'string)

(defmacro dbml-mode--with-sentinel (args success &optional fail always verbose)
  "Decorator for running process with sentinels.
Argument ARGS passed into `start-process'.
Arguments SUCCESS, FAIL, ALWAYS are either a form, lambda or function symbol.
Functions are passed the current process and its status (processp, string).
Optional argument VERBOSE shows debugging messages if non-nil.  A string will
be used as a prefix for the message."
  (declare (indent 1) (debug t))
  (list
   'set-process-sentinel
   (list 'apply (quote 'start-process) args)
   (list
    'lambda '(proc msg)
    (backquote
     (unwind-protect
         (progn
           (when ,verbose
             (message "%s time: %s, status: %s, message: %s"
                      (if (booleanp ,verbose) "dbml-mode" ,verbose)
                      (current-time-string) (process-status proc) msg))
           (if (and (eq (process-status proc) 'exit)
                    (string= (string-trim msg) "finished"))
               (cond
                ((and (symbolp ,success) ,success) (funcall ,success proc msg))
                ((consp ,success)
                 (if (eq (car ,success) 'lambda) (funcall ,success proc msg)
                   (apply (car ,success) (cdr ,success)))))
             (cond ((and (symbolp ,fail) ,fail) (funcall ,fail proc msg))
                   ((consp ,fail)
                    (if (eq (car ,fail) 'lambda) (funcall ,fail proc msg)
                      (apply (car ,fail) (cdr ,fail)))))))
       (cond ((and (symbolp ,always) ,always) (funcall ,always proc msg))
             ((consp ,always)
              (if (eq (car ,always) 'lambda) (funcall ,always proc msg)
                (apply (car ,always) (cdr ,always))))))))))

(defun dbml-mode--validate-table-names (num)
  "Validate whether table declaration exists.
Argument NUM `match-data' group containing table name."
  (let* ((begin (match-beginning num))
         (end (match-end num))
         (text (match-string num))
         (pattern (rx line-start (*? whitespace)
                      "table" (+? whitespace)
                      (literal text)
                      (*? whitespace) (literal "{"))))
    (unless (string-match-p pattern (buffer-string))
      (put-text-property begin end 'face '(underline error)))))

(defun dbml-mode--validate-column-names (table-match-num column-match-num)
  "Validate whether column name declaration exists.
Argument TABLE-MATCH-NUM `match-data' group containing table name.
Argument COLUMN-MATCH-NUM `match-data' group containing column name."
  ;; TODO: fix me for matching columns names when [] is split
  ;; over multiple lines (fix regex or make it anchored)
  (let* ((begin (match-beginning column-match-num))
         (end (match-end column-match-num))
         (column-name (match-string column-match-num))
         (table-body-pattern (rx line-start (*? whitespace)
                                 "table" (+? whitespace)
                                 (literal (match-string table-match-num))
                                 (*? whitespace) (literal "{")
                                 (group (*? anychar)) "}"))
         (column-pattern (rx line-start (+? blank)
                             (group (+? (or word "_"))) (+? blank)
                             (+ (or word "_")) (*? print) line-end)))
    (save-match-data
      (string-match table-body-pattern (buffer-string))
      (let* ((table-body-begin (match-beginning 1))
             (text (buffer-string))
             (pos table-body-begin)
             found-columns)
        (while (string-match column-pattern text pos)
          (push (match-string 1 text) found-columns)
          (setq pos (match-end 1)))
        (unless (member column-name found-columns)
          (put-text-property begin end 'face '(underline error)))))))

(defun dbml-mode--validate-unique-column (num)
  "Validate whether table is declared only once.
Argument NUM `match-data' group containing column name."
  ;; TODO: fix me for matching columns names when [] is split
  ;; over multiple lines (fix regex or make it anchored)
  (let* ((begin (match-beginning num))
         (end (match-end num))
         (column-name (match-string num))
         (table-body-pattern (rx line-start (*? whitespace)
                                 "table" (+? whitespace)
                                 (literal column-name) (*? whitespace)
                                 (literal "{") (*? whitespace)
                                 (group (*? anychar))
                                 (*? whitespace) (literal "}")))
         (column-pattern (rx line-start (+? blank)
                             (group (+? (or word "_"))) (+? blank)
                             (+ (or word "_")) (*? print) line-end)))
    (save-match-data
      (string-match table-body-pattern (buffer-string))
      (let* ((table-body-begin (match-beginning 1))
             (text (buffer-string))
             (pos table-body-begin)
             found-columns)
        (while (string-match column-pattern text pos)
          (push (match-string 1 text) found-columns)
          (setq pos (match-end 1)))
        (when (member column-name found-columns)
          (put-text-property begin end 'face '(underline error)))))))

(defun dbml-mode--validate-unique-table (num)
  "Validate whether table is declared only once.
Argument NUM `match-data' group containing table name."
  (let* ((begin (match-beginning num))
         (end (match-end num))
         (table-name (match-string num))
         (pattern (rx (or line-start (+? whitespace))
                      "table" (+? whitespace)
                      (group (+? (or word "_")))
                      (*? whitespace) (literal "{"))))
    (save-match-data
      (let* (;; check only previous occurrences, highlight current
             (text (buffer-substring 1 begin))
             (pos 0)
             found-tables)
        (while (string-match pattern text pos)
          (push (match-string 1 text) found-tables)
          (setq pos (match-end 1)))
        (when (member table-name found-tables)
          (put-text-property begin end 'face '(underline error)))))))

(defun dbml-mode--render ()
  "Render current buffer."
  (interactive)
  (if dbml-mode-render-dockerized (dbml-mode--render-docker)
    (dbml-mode--render-raw)))

(defsubst dbml-mode--render-raw-cb (proc &rest _)
  "Handler for `dbml-renderer' PROC."
  (kill-buffer (get-buffer-create (process-name proc)))
  (find-file-other-window
   (format "%s.svg" (file-name-nondirectory
                     (expand-file-name buffer-file-truename)))))

(defun dbml-mode--render-raw ()
  "Render current buffer with `dbml-renderer' installed in the system."
  (interactive)
  (let* ((temp-name (make-temp-name ""))
         (proc-name (format "dbml-mode-render-%s" temp-name))
         (buff (get-buffer-create proc-name))
         (in-file (file-name-nondirectory
                   (expand-file-name buffer-file-truename))))
    (dbml-mode--with-sentinel
        (list proc-name buff dbml-mode-render-bin
              "--input" in-file "--output" (format "%s.svg" in-file))
      'dbml-mode--render-raw-cb
      (switch-to-buffer-other-window buff))))

(defun dbml-mode--render-docker-run-cb (proc &rest _)
  "Handler for \"docker run\" PROC."
  (kill-buffer (get-buffer-create (process-name proc)))
  (find-file-other-window
   (format "%s.svg" (file-name-nondirectory
                     (expand-file-name buffer-file-truename)))))

(defun dbml-mode--render-docker-build-cb (proc &rest _)
  "Handler for \"docker build\" PROC."
  (let* ((proc-name (process-name proc))
         (buff (get-buffer-create proc-name))
         (full-path (expand-file-name buffer-file-truename))
         (in-file (file-name-nondirectory full-path)))
    (kill-buffer buff)
    (dbml-mode--with-sentinel
        (list proc-name buff "docker" "run"
              "--user" "1000:1000"
              "--volume" (format "%s:/mnt" (file-name-directory full-path))
              "--workdir" "/mnt"
              (or dbml-mode-use-image-name dbml-mode-build-image-name)
              dbml-mode-render-bin
              "--input" in-file
              "--output" (format "%s.svg" (file-name-nondirectory full-path)))
      'dbml-mode--render-docker-run-cb
      (switch-to-buffer-other-window (get-buffer-create proc-name))
      nil "dbml-mode-run")))

(defun dbml-mode--render-docker-build (proc &rest _)
  "Build Docker image for `dbml-renderer'.
Argument PROC is a handle from previous process checking for image presence."
  (let* ((dockerfile (string-join
                      '("FROM node:alpine"
                        "RUN npm install -g @softwaretechnik/dbml-renderer")
                      "\n"))
         (proc-name (process-name proc))
         (buff (get-buffer-create proc-name))
         (temp-name (car (last (split-string proc-name "-")))))
    (with-temp-file temp-name
      (insert dockerfile)
      (dbml-mode--with-sentinel
          (list proc-name buff "docker" "build"
                "--tag" dbml-mode-build-image-name "--file" temp-name ".")
        'dbml-mode--render-docker-build-cb
        (switch-to-buffer-other-window buff)
        (delete-file temp-name)
        "dbml-mode-build"))))

(defun dbml-mode--render-docker ()
  "Render current buffer with dockerized `dbml-renderer'."
  (interactive)
  (let* ((temp-name (make-temp-name ""))
         (proc-name (format "dbml-mode-render-%s" temp-name))
         (buff (get-buffer-create proc-name)))
    (dbml-mode--with-sentinel
        (list proc-name buff "sh" "-c"
              (format "docker images %s|grep %s"
                      (format "--filter=reference=%s"
                              (shell-quote-argument dbml-mode-use-image-name))
                      (shell-quote-argument dbml-mode-use-image-name)))
      'dbml-mode--render-docker-build-cb
      'dbml-mode--render-docker-build
      nil "dbml-mode-check-image")))

(defvar-local dbml-mode-keymap
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") #'dbml-mode--render)
    map)
  "`dbml-mode-mode' key map.")

;;;###autoload
(define-derived-mode dbml-mode prog-mode "DBML"
  "Major mode for editing DBML diagram files.

\\{dbml-mode-keymap}"
  :group 'dbml
  (use-local-map dbml-mode-keymap)

  ;; dummy indentation without broken newline indent
  (setq indent-line-function #'tab-to-tab-stop)
  (setq electric-indent-mode nil)

  (font-lock-set-defaults)
  (setq-local
   font-lock-extend-after-change-region-function
   (lambda (&rest _)
     ;; TODO: make me context-aware. Anchor matchers' forms are not useful for
     ;;       that because those aren't evaluated during before/after
     ;;       `after-change-function'.
     ;;
     ;; Naive whole-buffer re-highlight because of missing *direct* context
     ;; from anchors. So either indirect by looking back for delimiters or
     ;; there's some magical way to always eval anchor forms.
     ;;
     ;; This can be done in two ways. Super-silly one by searching back just
     ;; for `}' and highlighting until `point-max' or smarter one, though more
     ;; expensive, via regex search for anchored patterns. The interesting and
     ;; problematic part there is: which of the patterns to try; or will it
     ;; always be all of them and then some logic for guessing?
     (cons (point-min) (point-max))))

  ;; case-insensitive matching
  (setq-local font-lock-keywords-case-fold-search t)

  (font-lock-add-keywords
   ;; 'dbml-mode ;; TODO: keep to mode only!
   nil
   `(;; inline keywords
     (,(rx (or line-start (+? whitespace))
           (group (or "note" "ref" "indexes" "as")))
      1 'font-lock-keyword-face)
     ;; block keywords
     (,(rx line-start (*? whitespace)
           (group (or "project" "table" "tablegroup" "enum")))
      1 'font-lock-keyword-face)

     ;; names/types
     (,(rx line-start (*? whitespace)
           (or "project" "table" "tablegroup" "enum")
           (*? whitespace)
           (group (+ (or word "_"))) (*? whitespace) (*? anychar) line-end)
      (1 'font-lock-type-face)
      ((lambda (&rest _)) nil (dbml-mode--validate-unique-table 1) nil))

     ;; column names/variables; must not split over lines
     (,(rx line-start (*? whitespace)
           "table" (*? anychar) (literal "{")
           (group (*? anychar)) (literal "}"))
      (,(rx
         ;; col name
         (group (or
                 ;; normal word
                 (+ (or word "_"))
                 ;; quoted with spaces
                 (and (literal "\"") (+ not-newline) (literal "\""))))
         (+? blank)
         ;; col type
         (group (+ (or word "_")))
         ;; trailing trash and delimiter
         (*? anychar) line-end)
       ;; pre-match form
       (progn
         ;; start matching in the braces
         (goto-char (match-beginning 1))
         ;; then keep the anchored match loop within the block
         (match-end 0))
       ;; post-match form
       (progn (dbml-mode--validate-unique-column 1))
       (1 'font-lock-variable-name-face t)
       (2 'font-lock-type-face)))

     ;; TODO: prefix with braces, anchored as a block?
     ;; individual column settings (non-value keywords in angle brackets)
     (,(rx (group (literal "[")) (group (*? anychar)) (group (literal "]")))
      (1 'bold) (3 'bold)
      (,(rx (+? (or "pk" "primary key" "null"
                    (and "not" (+? whitespace) "null")
                    "unique" "increment" "note" "default")))
       (progn (goto-char (match-beginning 2))
              (match-end 3))
       nil ;; post
       (0 'font-lock-builtin-face prepend)))

     ;; individual relationship settings (key-value keywords in angle brackets)
     ;; TODO: highlight tables, check if they are defined
     (,(rx "ref" (*? print)
           (group (+? (or word "_"))) (literal ".") (group (+ (or word "_")))
           (*? print) (group (or ">" "<" "-" "<>")) (*? print)
           (group (+? (or word "_"))) (literal ".") (group (+ (or word "_")))
           (*? print)
           (group "[") (*? print) (group "]"))
      (1 'italic)
      (2 'bold-italic)
      (3 'bold)
      (3 'font-lock-builtin-face append)
      (4 'italic)
      (5 'bold-italic)
      (6 'bold t)
      (7 'bold t)
      (,(rx (+? (group (or "delete" "update"))
                (group ":") (*? blank)
                (group (or "cascade" "restrict"
                           (and "set" (+? blank) (or "null" "default"))
                           (and "no" (+? blank) "action")))))
       ;; pre-match in anchor, go before "]"
       (progn (goto-char (match-end 6))
              ;; TODO return correct pos back
              )
       ;; post-match form; table name validation
       (progn
         (dbml-mode--validate-table-names 1)
         (dbml-mode--validate-column-names 1 2))
       (0 'font-lock-builtin-face t)
       (1 'bold-italic prepend)
       (2 'default t)))))

  ;; NOTE: These MUST NOT set a face directly because it's weirdly
  ;; removed after post-self-insert-hook (or wherever...)
  ;; Use `font-lock-add-keywords' because it's keywords based on regex
  ;; i.e. "color-me-by-pattern" instead of it's original name
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ;; multi-line string
               ;; TODO: Ensure syntax highlighting works inside on patterns
               ;; check js-mode-syntax-table in 25.1+
               ((rx (group "`")) (1 "\""))

               ;; single-line strings
               ((rx (or whitespace (literal ":"))
                    (group "'" (*? print) "'")
                    (not "'")) (1 "\""))

               ;; multi-line strings
               ((rx (or whitespace (literal ":"))
                    (group "'''" (*? (not "'")) "'''")
                    (not "'")) (1 "\""))))

  (let ((table (make-syntax-table)))
    ;; As per `Syntax-Flags' section
    ;; / as a comment: opener, opener second char, closer second char
    (modify-syntax-entry ?/ ". 124" table)
    ;; * as a comment: opener second char (alternative), closer first char
    (modify-syntax-entry ?* ". 23b" table)
    ;; newline as a comment ender (primary)
    (modify-syntax-entry ?\n ">" table)
    (set-syntax-table table)))

(provide 'dbml-mode)
;;; dbml-mode.el ends here
