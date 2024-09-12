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

(ert-deftest dbml-mode-comment-single-no-newline ()
  "'// comment' with no newline is highlighted as a comment."
  (with-temp-buffer
    (insert "// comment")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #("// comment" 0 10 (face font-lock-comment-face))))
      (should (string= (format "%S" (buffer-string))
                       (format "%S" expected))))))

(ert-deftest dbml-mode-comment-single-with-newline ()
  "'// comment' with newline is highlighted as a comment."
  (with-temp-buffer
    (insert "// comment\n")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #("// comment\n" 0 11 (face font-lock-comment-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

(ert-deftest dbml-mode-comment-multi-no-newline ()
  "'/* comment */' with no newline is highlighted as a comment."
  (with-temp-buffer
    (insert "/*\nsome\ncomment\n*/")
    (should-not (text-properties-at (point-min)))
    (dbml-mode-in-ert)
    (let ((expected #("/*\nsome\ncomment\n*/" 0 18
                      (face font-lock-comment-face))))
      (should
       (string= (format "%S" (buffer-string)) (format "%S" expected))))))

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

(provide 'dbml-mode-tests)

;;; dbml-mode-tests.el ends here
