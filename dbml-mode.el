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

;;;###autoload
(define-derived-mode dbml-mode prog-mode "DBML"
  "Major mode for editing DBML diagram files."
  :group 'dbml

  (font-lock-set-defaults))

(provide 'dbml-mode)
;;; dbml-mode.el ends here
