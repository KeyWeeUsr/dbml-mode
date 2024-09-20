;;; dbml-mode-tests.el -- tests for dbml-mode

;;; Code:

(require 'ert)
(require 'dbml-mode)
(require 'font-lock)
(setq ert-quiet t)

(defsubst dbml-mode-in-ert ()
  "Because something is turned off in ERT."
  (dbml-mode)
  (font-lock-mode 1)
  (font-lock-ensure))

(defsubst dbml-mode-test-file (path)
  "Check for font properties based on marks in file located in PATH."
  (let (to-highlight highlighted work-buff properties inside)
    (with-temp-buffer
      (setq work-buff (current-buffer))

      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8)) (insert-file-contents path))
        (should (eq (point) (point-min)))
        (while (not (eobp))
          (let ((line (buffer-substring
                       (point) (progn (forward-line 1) (point)))))
            (cond ((string-prefix-p ">" line)
                   (with-current-buffer work-buff
                     (insert (string-trim-right
                              (substring-no-properties line 1) "\n"))))
                  ((string-prefix-p "#" line)
                   (let* ((text (substring-no-properties line 1))
                          (start (string-match "\\^" text))
                          (end (string-match "\\$" text)))
                     (when start
                       (setq inside t)
                       (push start properties))
                     (if (not end)
                         (progn
                           (unless (numberp inside) (setq inside 0))
                           (setq inside (+ inside (string-match "?" text))))
                       (when (numberp inside)
                         (setq end (+ end inside)))
                       (setq inside nil)
                       (push end properties)
                       (push (intern
                              (string-trim-right
                               (substring-no-properties
                                line (+ 2 (string-match "|" text))) "\n"))
                             properties))))))))
      (setq to-highlight
            (buffer-substring-no-properties (point-min) (point-max)))
      (goto-char (point-min))
      (dbml-mode-in-ert)
      (setq highlighted (format "%S" (buffer-string))))
    (should
     (string=
      (substring highlighted 1)
      (format "%s" `(,(format "%S" to-highlight) ,@(reverse properties)))))))

(ert-deftest dbml-mode-comment-single-no-newline ()
  (dbml-mode-test-file "test-files/comment-single-no-newline.txt"))

(ert-deftest dbml-mode-comment-single-with-newline ()
  (dbml-mode-test-file "test-files/comment-single-with-newline.txt"))

(ert-deftest dbml-mode-comment-multi-no-newline ()
  (dbml-mode-test-file "test-files/comment-multi-no-newline.txt"))

(ert-deftest dbml-mode-keyword-line-start-no-newline ()
  "One of the keywords with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #("table" 0 5 (face font-lock-keyword-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

(ert-deftest dbml-mode-keyword-line-leading-no-newline ()
  "One of the keywords with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #(" table" 1 6 (face font-lock-keyword-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

(ert-deftest dbml-mode-keyword-line-leading-trailing-no-newline ()
  "One of the keywords with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table ")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #(" table " 1 6 (face font-lock-keyword-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

(ert-deftest dbml-mode-keyword-line-trailing-no-newline ()
  "One of the keywords with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table ")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #("table " 0 5 (face font-lock-keyword-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

(ert-deftest dbml-mode-keyword-line-start-with-newline ()
  "One of the keywords with with newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table\n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #("table\n" 0 5 (face font-lock-keyword-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

(ert-deftest dbml-mode-keyword-line-leading-with-newline ()
  "One of the keywords with with newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table\n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #(" table\n" 1 6 (face font-lock-keyword-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

(ert-deftest dbml-mode-keyword-line-leading-trailing-with-newline ()
  "One of the keywords with with newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table \n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #(" table \n" 1 6 (face font-lock-keyword-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

(ert-deftest dbml-mode-keyword-line-trailing-with-newline ()
  "One of the keywords with with newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table \n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #("table \n" 0 5 (face font-lock-keyword-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

(ert-deftest dbml-mode-table-name-line-start-no-newline ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table abc")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #("table abc"
                                    0 5 (face font-lock-keyword-face)
                                    6 9 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-leading-no-newline ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table abc")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #(" table abc"
                                    1 6 (face font-lock-keyword-face)
                                    7 10 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-leading-trailing-no-newline ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table abc ")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #(" table abc "
                                    1 6 (face font-lock-keyword-face)
                                    7 10 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-trailing-no-newline ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table abc ")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #("table abc "
                                    0 5 (face font-lock-keyword-face)
                                    6 9 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-start-with-newline ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table abc\n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #("table abc\n"
                                    0 5 (face font-lock-keyword-face)
                                    6 9 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-leading-with-newline ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table abc\n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #(" table abc\n"
                                    1 6 (face font-lock-keyword-face)
                                    7 10 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-leading-trailing-with-newline ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table abc \n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #(" table abc \n"
                                    1 6 (face font-lock-keyword-face)
                                    7 10 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-trailing-with-newline ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table abc \n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #("table abc \n"
                                    0 5 (face font-lock-keyword-face)
                                    6 9 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-start-no-newline-spaced ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table    abc")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #("table    abc"
                                    0 5 (face font-lock-keyword-face)
                                    9 12 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-leading-no-newline-spaced ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table    abc")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #(" table    abc"
                                    1 6 (face font-lock-keyword-face)
                                    10 13 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-leading-trailing-no-newline-spaced ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table    abc ")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #(" table    abc "
                                    1 6 (face font-lock-keyword-face)
                                    10 13 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-trailing-no-newline-spaced ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table    abc ")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #("table    abc "
                                    0 5 (face font-lock-keyword-face)
                                    9 12 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-start-with-newline-spaced ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table    abc\n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #("table    abc\n"
                                    0 5 (face font-lock-keyword-face)
                                    9 12 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-leading-with-newline-spaced ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table    abc\n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #(" table    abc\n"
                                    1 6 (face font-lock-keyword-face)
                                    10 13 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-leading-trailing-with-newline-spaced ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert " table    abc \n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #(" table    abc \n"
                                    1 6 (face font-lock-keyword-face)
                                    10 13 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-table-name-line-trailing-with-newline-spaced ()
  "Table name with no newline is highlighted as a keyword."
  (with-temp-buffer
    (insert "table    abc \n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" #("table    abc \n"
                                    0 5 (face font-lock-keyword-face)
                                    9 12 (face font-lock-type-face)))))))

(ert-deftest dbml-mode-keyword-in-word ()
  "Do not highlight keyword as a part of other word (possibly mangled)."
  (with-temp-buffer
    (insert "notatable\nnot-a-table")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (should (string= (format "%S" (buffer-string))
                     (format "%S" "notatable\nnot-a-table")))))

(ert-deftest dbml-mode-table-name-in-mangled ()
  "Do not highlight block name for mangled keyword."
  (let ((lines '("table t"
                 "table name"
                 "table name{}"
                 "table name2 {}"
                 "xtable t"
                 "xtable name"
                 "xtable name{}"
                 "xtable name2 {}")))
    (with-temp-buffer
      (insert (string-join lines "\n"))
      (should-not (text-properties-at (point-min)))
      (dbml-mode-in-ert)
      (should (string= (format "%S" (buffer-string))
                       (replace-regexp-in-string
                        "placeholderxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                        (string-join lines "\n")
                        (format
                         "%S" #("placeholderxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                                0 5 (face font-lock-keyword-face)
                                6 7 (face font-lock-type-face)
                                8 13 (face font-lock-keyword-face)
                                14 18 (face font-lock-type-face)
                                19 24 (face font-lock-keyword-face)
                                25 29 (face font-lock-type-face)
                                32 37 (face font-lock-keyword-face)
                                38 43 (face font-lock-type-face)))))))))

(ert-deftest dbml-mode-column-name-rehighlight-in-anchored ()
  "Re-highlight columns (anchored block pattern matching region)."
  (let* ((noninteractive nil)
         (lines '("table name {"
                  "one type"
                  "two type"
                  "}"))
         (expected '("table name {"
                     "one type"
                     "two type2"
                     "three type"
                     "}")))
    (with-temp-buffer
      (dolist (char (string-to-list (string-join lines "\n")))
        (when char
          (execute-kbd-macro (kbd (cond ((= char 10) "RET")
                                        ((= char 32) "SPC")
                                        (t (char-to-string char)))))))
      (should-not (text-properties-at (point-min)))
      (dbml-mode-in-ert)

      (should
       (string= (format "%S" (buffer-string))
                (replace-regexp-in-string
                 "placeholderxxxxxxxxxxxxxxxxxxxxx"
                 (string-join lines "\n")
                 (format
                  "%S" #("placeholderxxxxxxxxxxxxxxxxxxxxx"
                         0 5   (face font-lock-keyword-face fontified t)
                         5 6   (fontified t)
                         6 10  (face font-lock-type-face fontified t)
                         10 13 (fontified t)
                         13 16 (face font-lock-variable-name-face fontified t)
                         16 17 (fontified t)
                         17 21 (face font-lock-type-face fontified t)
                         21 22 (fontified t)
                         22 25 (face font-lock-variable-name-face fontified t)
                         25 26 (fontified t)
                         26 30 (face font-lock-type-face fontified t)
                         30 32 (fontified t))))))

      ;; Go to last col's type, type something to trigger re-highlighting
      (goto-char (- (point-max) 2))
      (execute-kbd-macro (kbd "2"))
      (dolist (char (string-to-list "\nthree type"))
        (when char (execute-kbd-macro (kbd (cond ((= char 10) "RET")
                                                 ((= char 32) "SPC")
                                                 (t (char-to-string char)))))))

      ;; TODO: fontification-functions are NOT called in ERT. Why?
      (jit-lock-fontify-now)

      (should
       (string= (format "%S" (buffer-string))
                (replace-regexp-in-string
                 "placeholderxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                 (string-join expected "\n")
                 (format
                  ;; DO NOT TOUCH THESE!!!!
                  "%S" #("placeholderxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                         0 5   (face font-lock-keyword-face fontified t)
                         5 6   (fontified t)
                         6 10  (face font-lock-type-face fontified t)
                         10 13 (fontified t)
                         13 16 (face font-lock-variable-name-face fontified t)
                         16 17 (fontified t)
                         17 21 (face font-lock-type-face fontified t)
                         21 22 (fontified t)
                         22 25 (face font-lock-variable-name-face fontified t)
                         25 26 (fontified t)
                         26 31 (face font-lock-type-face fontified t)
                         31 32 (fontified t)
                         32 37 (face font-lock-variable-name-face fontified t)
                         37 38 (fontified t)
                         38 42 (face font-lock-type-face fontified t)
                         42 44 (fontified t)))))))))

(ert-deftest dbml-mode-table-name-duplicate ()
  "Highlight table name if duplicated before occurrence."
  (let ((lines '("table name{}"
                 "table name{}")))
    (with-temp-buffer
      (insert (string-join lines "\n"))
      (should-not (text-properties-at (point-min)))
      (dbml-mode-in-ert)
      (should (string= (format "%S" (buffer-string))
                       (replace-regexp-in-string
                        "placeholderxxxxxxxxxxxx"
                        (string-join lines "\n")
                        (format
                         "%S" #("placeholderxxxxxxxxxxxx"
                                0 5 (face font-lock-keyword-face)
                                6 10 (face font-lock-type-face)
                                13 18 (face font-lock-keyword-face)
                                19 23 (face (underline error))))))))))

(ert-deftest dbml-mode-keyword-multi-occurrence ()
  "Highlight keywords even if duplicated."
  (let ((lines '("table table" "table")))
    (with-temp-buffer
      (insert (string-join lines "\n"))
      (should-not (text-properties-at (point-min)))
      (dbml-mode-in-ert)
      (should (string= (format "%S" (buffer-string))
                       (replace-regexp-in-string
                        "placeholderxxxxxx"
                        (string-join lines "\n")
                        (format
                         "%S" #("placeholderxxxxxx"
                                0 5 (face font-lock-keyword-face)
                                6 11 (face font-lock-type-face)
                                12 17 (face font-lock-keyword-face)))))))))

(ert-deftest dbml-mode-not-a-column ()
  "Columns are anchored. No random highlighting out of braces."
  (let ((lines '("Table name {}"
                 " don't highlight this line with column font!")))
    (with-temp-buffer
      (insert (string-join lines "\n"))
      (should-not (text-properties-at (point-min)))
      (dbml-mode-in-ert)
      (should (string= (format "%S" (buffer-string))
                       (replace-regexp-in-string
                        "placeholder"
                        (string-join lines "\n")
                        (format
                         "%S" #("placeholder"
                                0 5 (face font-lock-keyword-face)
                                6 10 (face font-lock-type-face)))))))))

(ert-deftest dbml-mode-column-settings ()
  "Columns settings are anchored to brackets and belong to a known list."
  ;; TODO: pull to defcustom var
  (dolist (word-prop '("pk" "primary key" "null" "not null" "unique"
                       "increment" ("note" font-lock-keyword-face) "default"))
    (let* ((word (if (stringp word-prop) word-prop (car word-prop)))
           (prop (if (stringp word-prop) '(font-lock-builtin-face)
                   (backquote (font-lock-builtin-face ,@(cdr word-prop)))))
           (blank ", not, ")
           (text (format "table test{col type [%s%s%s]}" word blank word)))
      (with-temp-buffer
        (insert text)
        (should-not (text-properties-at (point-min)))
        (dbml-mode-in-ert)
        (should
         (string=
          (replace-regexp-in-string "#" "" (format "%S" (buffer-string)))
          (replace-regexp-in-string
           "placeholderxxxxxxxxxxxxxxxxxxxxxx" text
           (format
            "%S" (list
                  "placeholderxxxxxxxxxxxxxxxxxxxxxx"
                  0 5 '(face font-lock-keyword-face)
                  6 10 '(face font-lock-type-face)
                  11 14 '(face font-lock-variable-name-face)
                  15 19 '(face font-lock-type-face)
                  20 21 '(face bold)
                  ;; append/prepend -> ()
                  21 (+ 21 (length word)) '(face (font-lock-builtin-face))
                  (+ 21 (length word) (length blank))
                  (+ 21 (length word) (length blank) (length word))
                  (backquote (face ,prop))
                  (+ 21 (length word) (length blank) (length word))
                  (+ 21 (length word) (length blank) (length word) 1)
                  '(face bold))))))))))

(provide 'dbml-mode-tests)

;;; dbml-mode-tests.el ends here
