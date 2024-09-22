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
  (let (to-highlight highlighted work-buff properties inside (last-pos 0))
    (with-temp-buffer
      (setq work-buff (current-buffer))

      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents (format "test-files/%s" path)))
        (should (eq (point) (point-min)))

        (let (collect track copy line start end)
          (while (not (eobp))
            (let ((char (char-to-string (char-after))))
              (delete-char 1)
              (cond ((and (not line) (string= char ">")) (setq collect t))
                    (collect
                     (cond
                      ((string= char "\\") (push "\n" line))
                      ((string= char "\n")
                       (setq collect nil)
                       (with-current-buffer work-buff
                         (insert (string-join (reverse line) "")))
                       (setq line nil))
                      (t (push char line))))
                    ((and (not collect) (string= char "#")) (setq track t))
                    (track
                     (cond ((string= char "^")
                            (setq start last-pos)
                            (setq inside t)
                            (push start properties)
                            (setq last-pos (1+ last-pos)))
                           ((string= char "-")
                            ;; inside
                            (setq last-pos (1+ last-pos)))
                           ((string= char "~")
                            ;; outside, whitespace or noise between highlights
                            (setq last-pos (1+ last-pos)))
                           ((string= char "$")
                            (setq end last-pos)
                            (setq inside nil)
                            (push end properties))
                           ((string= char "|")
                            (setq track nil)
                            (setq copy t))
                           ((string= char "\n")
                            (setq track nil))))
                    (copy
                     (if (not (string= char "\n"))
                         (push char line)
                       (setq copy nil)
                       (push (intern (string-join (reverse line) ""))
                             properties)
                       (setq line nil)))
                    (t (setq last-pos (1+ last-pos))))))))
      (setq to-highlight
            (buffer-substring-no-properties (point-min) (point-max)))
      (goto-char (point-min))
      (dbml-mode-in-ert)
      (setq highlighted (format "%S" (buffer-string))))
    (let ((real (if properties (substring highlighted 1) highlighted))
          (expected
           (if properties
               (format
                "%s" `(,(format "%S" to-highlight) ,@(reverse properties)))
             (format "%S" to-highlight))))
      (should (string= expected real)))))

(ert-deftest dbml-mode-run-test-files ()
  "Run `dbml-mode' against test-files and verify its highlighting."
  (let ((files (directory-files "test-files" nil ".txt$"))
        (ran 0) failed)
    (dolist (name files)
      (let* ((stats ert--current-run-stats)
             (test (make-ert-test
                    :name (string-trim-right name ".txt")
                    :body (lambda () (dbml-mode-test-file name))))
             (result (ert-run-test test)))
        (setq ran (1+ ran))
        (if (ert-test-passed-p result)
            (progn
              (if (ert-test-result-expected-p test result)
                  (setf (ert--stats-passed-expected stats)
                        (1+ (ert--stats-passed-expected stats)))
                (setf (ert--stats-passed-unexpected stats)
                      (1+ (ert--stats-passed-unexpected stats)))))
          (if (ert-test-result-expected-p test result)
              (setf (ert--stats-failed-expected stats)
                    (1+ (ert--stats-failed-expected stats)))
            ;; failed and unexpected
            (setf (ert--stats-failed-unexpected stats)
                  (1+ (ert--stats-failed-unexpected stats)))
            (let ((should-forms (ert-test-failed-should-forms result)))
              (push `(,(ert-test-name test)
                      ,(with-temp-buffer (insert (format "%s" should-forms))
                                         (pp-buffer) (buffer-string)))
                    failed))))))
    (advice-add 'ert-stats-total
                :filter-return `(lambda (result) (+ result ,ran)))
    (when failed
      (ert-fail failed))))

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
