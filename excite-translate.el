;;; excite-translate.el --- translate english using excite's service.

;; Copyright (C) 2010  

;; Author:  <lieutar@TREEFROG>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(require 'kael-http)

(defun excite-translate-* (before type)
  (let ((buf (kael-http-post
              "http://www.excite.co.jp/world/english/"
              `(("before" . ,before)
                ("wb_lp"  . ,type))))
        (result nil))
    (set-buffer buf)
    (goto-char (point-min))
    (if (re-search-forward "<textarea[^>]*? name=\"after\"[^>]*>\\([^<]+\\)"
                           nil t)
        (setq result (match-string-no-properties 1))
      )
    (kill-buffer buf)
    (decode-coding-string result 'utf-8)))

(defun excite-translate-enja (before) (excite-translate-* before "ENJA"))
(defun excite-translate-jaen (before) (excite-translate-* before "JAEN"))

(defvar excite-translate-guess-type-threshold 0.25)
(defun excite-translate-guess-type (str)
  (let ((ascii     0)
        (non-ascii 0)
        (len (length str))
        (i   0))
    (while (< i len)
      (let ((char (aref str i)))
        (if (< char 127)
            (setq ascii (1+ ascii))
            (setq non-ascii (1+ non-ascii)))
        (setq i (1+ i))))
    (if (< excite-translate-guess-type-threshold
           (/ (+ 0.0 non-ascii) len))
        "JAEN" "ENJA"
        )))

(defun excite-translate-region (beg end &optional type)
  (interactive "r")
  (let* ((before (buffer-substring-no-properties beg end))
         (type   (or type
                     (excite-translate-guess-type before)
                     ))
         (result (excite-translate-* before type)))
    (when (interactive-p)
      (let ((buf (get-buffer-create "*excite-translate*")))
        (set-buffer buf)
        (setq buffer-read-only t)
        (let ((buffer-read-only nil))
          (delete-region (point-min) (point-max))
          (insert before)
          (insert "\n----\n")
          (insert result)
          (goto-char (point-min)))
        (pop-to-buffer buf)))
    result))


;;(excite-translate-enja "This is a pen.")
;;(excite-translate-jaen "これはペンです。")


(provide 'excite-translate)
;;; excite-translate.el ends here
