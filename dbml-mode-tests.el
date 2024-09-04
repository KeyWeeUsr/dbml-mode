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

(provide 'dbml-mode-tests)

;;; dbml-mode-tests.el ends here
