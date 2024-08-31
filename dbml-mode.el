;;; dbml-mode.el --- Major mode for DBML -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, dbml, language, markup, highlight
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
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

;; -

;;; Code:

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

(defun dbml-mode--validate-table-names (num)
  (let* ((begin (match-beginning num))
         (end (match-end num))
         (text (match-string num))
         (pattern (rx (or line-start (+? whitespace))
                      "table" (+? whitespace)
                      (literal text)
                      (*? whitespace) (literal "{"))))
    (unless (string-match-p pattern (buffer-string))
      (put-text-property begin end 'face '(underline error)))))

(defun dbml-mode--validate-column-names (table-match-num column-match-num)
  ;; TODO: fix me for matching columns names when [] is split
  ;; over multiple lines (fix regex or make it anchored)
  (let* ((begin (match-beginning column-match-num))
         (end (match-end column-match-num))
         (column-name (match-string column-match-num))
         (table-body-pattern (rx (or line-start (+? whitespace))
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
             (table-body-end (match-end 1))
             (text (buffer-string))
             (pos table-body-begin)
             found-columns)
        (while (string-match column-pattern text pos)
          (push (match-string 1 text) found-columns)
          (setq pos (match-end 1)))
        (unless (member column-name found-columns)
          (put-text-property begin end 'face '(underline error)))))))

(defun dbml-mode--validate-unique-column (num)
  ;; TODO: fix me for matching columns names when [] is split
  ;; over multiple lines (fix regex or make it anchored)
  (let* ((begin (match-beginning num))
         (end (match-end num))
         (column-name (match-string num))
         (table-body-pattern (rx (or line-start (+? whitespace))
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
             (table-body-end (match-end 1))
             (text (buffer-string))
             (pos table-body-begin)
             found-columns)
        (while (string-match column-pattern text pos)
          (push (match-string 1 text) found-columns)
          (setq pos (match-end 1)))
        (when (member column-name found-columns)
          (put-text-property begin end 'face '(underline error)))))))

(defun dbml-mode--validate-unique-table (num)
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
  (if dbml-mode-render-dockerized
      (dbml-mode--render-docker)
    (dbml-mode--render-raw)))

(defun dbml-mode--render-raw ()
  "Render current buffer with `dbml-renderer' installed in the system."
  (interactive)
  (let* ((temp-name (make-temp-name ""))
         (proc-name (format "dbml-mode-render-%s" temp-name)))
    (message "meow")
    (set-process-sentinel
     (start-process
      proc-name (get-buffer-create proc-name)
      dbml-mode-render-bin
      "--input" (file-name-nondirectory
                 buffer-file-truename)
      "--output" (format "%s.svg" (file-name-nondirectory
                                   buffer-file-truename)))
     `(lambda (proc msg)
        (message "%s time: %s, status: %s, message: %s"
                 "dbml-mode-run" (current-time-string)
                 (process-status proc) msg)
        (when (and (eq (process-status proc) 'exit)
                   (string= (string-trim msg) "finished"))
          (kill-buffer (get-buffer-create ,proc-name))
          (find-file-other-window
           (format "%s.svg" (file-name-nondirectory
                             buffer-file-truename))))))))

(defun dbml-mode--render-docker ()
  "Render current buffer with dockerized `dbml-renderer'."
  (interactive)
  (let* ((dockerfile (string-join
                     '("FROM node:alpine"
                       "RUN npm install -g @softwaretechnik/dbml-renderer")
                     "\n"))
        (temp-name (make-temp-name ""))
        (proc-name (format "dbml-mode-render-%s" temp-name))
        (image-name "dbml-mode-render"))
    (with-temp-file temp-name
      (insert dockerfile)
      (set-process-sentinel
       (start-process
        proc-name (get-buffer-create proc-name)
        "docker" "build"
        "--tag" image-name "--file" temp-name ".")
       `(lambda (proc msg)
          (message "%s time: %s, status: %s, message: %s"
                   "dbml-mode-build" (current-time-string)
                   (process-status proc) msg)
          (when (and (eq (process-status proc) 'exit)
                     (string= (string-trim msg) "finished"))
            (kill-buffer (get-buffer-create ,proc-name))
            (set-process-sentinel
             (start-process
              ,proc-name (get-buffer-create ,proc-name)
              "docker" "run"
              "--user" "1000:1000"
              "--volume" (format "%s:/mnt"
                                 (file-name-directory
                                  (expand-file-name buffer-file-truename)))
              "--workdir" "/mnt" ,image-name
              ,dbml-mode-render-bin
              "--input" (file-name-nondirectory
                         buffer-file-truename)
              "--output" (format "%s.svg" (file-name-nondirectory
                                           buffer-file-truename)))
             `(lambda (proc msg)
                (message "%s time: %s, status: %s, message: %s"
                         "dbml-mode-run" (current-time-string)
                         (process-status proc) msg)
                (when (and (eq (process-status proc) 'exit)
                           (string= (string-trim msg) "finished"))
                  (kill-buffer (get-buffer-create ,,proc-name))
                  (find-file-other-window
                   (format "%s.svg" (file-name-nondirectory
                                     buffer-file-truename))))))))))))

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

  ;; dummy indentation
  (setq indent-line-function 'tab-to-tab-stop)

  (font-lock-set-defaults)

  ;; case-insensitive matching
  (setq-local font-lock-keywords-case-fold-search t)

  (font-lock-add-keywords
   ;; 'dbml-mode ;; TODO: keep to mode only!
   nil
   `(;; keywords
     (,(rx (or "project" "table" "tablegroup" "note" "ref" "enum"))
      0 'font-lock-keyword-face)

     ;; names/types
     (,(rx (or "project" "table" "tablegroup" "enum")
           (*? whitespace)
           (group (+? graph)) (*? whitespace) (literal "{"))
      (1 'font-lock-type-face)
      ((lambda (&rest _)) nil (dbml-mode--validate-unique-table 1) nil))

     ;; column names/variables; must not split over lines
     (,(rx (or line-start (+? whitespace))
           "table" (*? anychar) (literal "{")
           (group (*? anychar)) (literal "}"))
      (,(rx (or line-start (*? blank))
            (group (+? (or word "_"))) (+? blank)
            (group (+ (or word "_"))) (*? print) line-end)
       (progn (goto-char (1+ (match-beginning 1)))
              (match-end 1))
       (progn
         (dbml-mode--validate-unique-column 1))
       (1 'font-lock-variable-name-face)
       (2 'font-lock-type-face)))

     ;; individual column settings (non-value keywords in angle brackets)
     (,(rx (group (literal "[")) (group (*? anychar)) (group (literal "]")))
      (1 'bold) (3 'bold)
      (,(rx (+? (or "pk" "primary key" "null"
                    (and "not" (+? whitespace) "null")
                    "unique" "increment" "note" "default")))
       (progn (goto-char (match-beginning 2))
              (match-end 2))
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
