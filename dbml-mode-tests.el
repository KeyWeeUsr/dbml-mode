;;; dbml-mode-tests.el -- tests for dbml-mode

;;; Code:

(require 'ert)
(require 'dbml-mode)

(defsubst dbml-mode-in-ert ()
  "Because something is turned off in ERT."
  (dbml-mode)
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

(provide 'dbml-mode-tests)

;;; dbml-mode-tests.el ends here
