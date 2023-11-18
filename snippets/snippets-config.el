;;; snippets-config.el ---

;; Author: Christian Birk Sørensen <chrbirks+emacs@gmail.com>
;; Created: 17 July 2019
;; Keywords: snippets

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup snippets-config nil
  "Different helper functions for (ya)snippet templates"
  )

;; VHDL settings
(setq vhdl-company-name company-name
      vhdl-date-format "%Y-%m-%d"
      )

;; \sum_{}^{}
;; \sum_{$1}^{$2} ${3:$$(yas-delete-if-empty)}
(defun yas-delete-if-empty ()
  (save-excursion
    (when (re-search-backward "\\\\sum\\(_{}\\)^{.+}" (line-beginning-position) t)
      (replace-match "" t t nil 1))))

(provide 'snippets-config)
;;; snippets-config.el ends here
