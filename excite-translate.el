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

(defvar excite-translate-previous-window-configuration nil)
(defvar excite-translate-end-of-before nil)
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

(defconst excite-translate-result-mode-back-to nil)

(defconst excite-translate-result-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-k") 'excite-translate-result-mode-quit)
    (define-key km (kbd "C-c C-c") 'excite-translate-result-mode-re-translate)
    km))

(define-derived-mode excite-translate-result-mode
  text-mode
  "excite-translate-result"
  "The Major mode for displaying result of \\[excite-translate].

    \\[excite-translate-result-mode-map]
"
  (make-variable-buffer-local
   'excite-translate-result-mode-back-to)
  (make-variable-buffer-local
   'excite-translate-previous-window-configuration)
  (make-variable-buffer-local
   'excite-translate-end-of-before))

(defun excite-translate-result-mode-quit ()
  (interactive)
  (let ((winconf excite-translate-previous-window-configuration)
        (backto (and (windowp excite-translate-result-mode-back-to)
                     excite-translate-result-mode-back-to)))
    (kill-buffer (current-buffer))
    (when winconf (set-window-configuration winconf))
    (when backto (select-window backto))))

(defun excite-translate-result-mode-re-translate ()
  (interactive)
  (excite-translate
   (replace-regexp-in-string
    "\\`\\s +\\|\\s +\\'" ""
    (buffer-substring-no-properties
     (point-min) excite-translate-end-of-before))
    nil
    t))

(defun excite-translate-popup (before result)
  (let ((buf (get-buffer-create "*excite-translate*")))
    (set-buffer buf)
    (excite-translate-result-mode)
    (let ((inhibit-read-only t)
          buffer-read-only)
      (delete-region (point-min) (point-max))
      (insert "\n" before "\n")
      (setq excite-translate-end-of-before (point-marker))
      (setq excite-translate-previous-window-configuration
            (current-window-configuration (selected-frame)))
      (insert (propertize "--------\n"
                          'face '((:foreground "#CCCCCC"
                                   :background "#CCCCCC"))
                          'read-only t
                          'rear-nonsticky t))
      (insert (propertize (concat "\n"
                                  (propertize result
                                              'face
                                              '((:foreground "#000099"))) "\n")
                          'read-only t))
      (goto-char (point-min)))
    (setq excite-translate-result-mode-back-to cwin)
    (pop-to-buffer buf)))

(defun excite-translate (before &optional type with-popup)
  "Translates string by Excite translation service.

`BEFORE' is the string for translation. `TYPE' is translation type.
When the `TYPE' is \"ENJA\" , the translation is English to Japanese, 
when the `TYPE' is \"JAEN\" , the translation is Japanese to English.
If the `TYPE' is nil, this function chooses value of `TYPE' automatically.

When `WITH-POPUP' is not nil this function popups buffer that contains
result of the translation. The mode of the buffer is
`excite-translate-result-mode'.


Under the interactive calling:

  If the region is active, this command uses string at content of the region.
  Otherwise this command asks string with prompt at the minibuffer.
"
  (interactive 
   (list (if mark-active
             (buffer-substring-no-properties
              (region-beginning) (region-end))
           (read-string "from: "))))

  (let* ((cwin   (selected-window))
         (type   (or type
                     (excite-translate-guess-type before)))
         (result (excite-translate-* before type)))
    (when (or with-popup
              (interactive-p))
      (excite-translate-popup before result))
    result))


(provide 'excite-translate)
;;; excite-translate.el ends here
