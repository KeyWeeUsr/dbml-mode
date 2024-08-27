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

;;;###autoload
(define-derived-mode dbml-mode prog-mode "DBML"
  "Major mode for editing DBML diagram files."
  :group 'dbml

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
       nil ;; post
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
       nil ;; post
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
