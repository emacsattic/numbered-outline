;;; numbered-outline.el --- Numbered Outline mode

;; Author: Gareth Rees <gareth.rees@pobox.com>
;; Version: 0.1
;; Keywords: outlines
;; Time-stamp: <2000-02-04T17:39:55>
;; Copyright (c) 2000 Gareth Rees

;;; Licence:

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; Numbered Outline mode is a minor mode providing extensions to both
;; Outline mode and Outline minor mode for dealing with hierarchical
;; numbered outlines, for example
;;
;; 1 Top-level heading
;; 1.1 Subheading
;; 1.2 Another subheading
;; 1.2.1 Subsubheading
;;
;; When turned on for a buffer, Numbered Outline mode sets
;; `outline-regexp' to `numbered-outline-regexp' which matches numbered
;; headings and sets `outline-level' to `numbered-outline-level' which
;; calculates the heading level appropriately.  Functions are provided
;; for promoting and demoting heading levels in the region, and for
;; canonically renumbering headings in the buffer.
;;
;; You can customize `numbered-outline-regexp' (for example, to match
;; heading numbers preceded by a comment indicator or a piece of
;; markup), as long as you make sure that your new regexp has the right
;; set of capturing parentheses -- see the documentation for that
;; variable for details.

;;; Usage:

;; Put the following in your .emacs:
;;
;;   (autoload 'numbered-outline "numbered-outline" nil t)
;;
;; and put the following at the top of files that you want to edit using
;; Outline major mode:
;;
;;   -*- mode: outline; mode: numbered-outline -*-
;;
;; or the following for files that you want to edit using Outline minor
;; mode:
;;
;;   -*- mode: numbered-outline -*-
;;
;; or use the function `numbered-outline-mode' to toggle the mode.

;;; Code:

(eval-when-compile (require 'cl))
(require 'outline)

(define-key outline-mode-prefix-map "\C-r" 'numbered-outline-renumber)
(define-key outline-mode-prefix-map "\C-z" 'numbered-outline-promote)
(define-key outline-mode-prefix-map "\C-x" 'numbered-outline-demote)

(define-key outline-mode-menu-bar-map [headings separator]
  '(menu-item "--" :visible numbered-outline-mode))
(define-key outline-mode-menu-bar-map [headings numbered-outline-renumber]
  '(menu-item "Renumber headings" numbered-outline-renumber
              :visible numbered-outline-mode))
(define-key outline-mode-menu-bar-map [headings numbered-outline-demote]
  '(menu-item "Demote region" numbered-outline-demote
              :visible numbered-outline-mode))
(define-key outline-mode-menu-bar-map [headings numbered-outline-promote]
  '(menu-item "Promote region" numbered-outline-promote
              :visible numbered-outline-mode))

(defcustom numbered-outline-renumber-on-save t
  "*Non-nil if buffer should be renumbered before saving.
Applies only to buffers where Numbered Outline mode is on."
  :type 'boolean
  :group 'outlines)

(defcustom numbered-outline-regexp "^\\([0-9]+\\(\\.[0-9]+\\)*\\)"
  "*Regular expression to match the beginning of a numbered heading.
Any line which matches this regexp is considered to start a heading.
Used to set the value of `outline-regexp' when Numbered Outline mode
is turned on.

The regexp should contain one pair of capturing parentheses (indicated
by `numbered-outline-regexp-match-whole-number') that capture the whole
heading number, and a second pair of capturing parentheses (indicated by
`numbered-outline-regexp-match-last-number') that capture the last
component of the heading number, if any.

The recommended way to set this (and its two auxiliaries) is with a
Local Variables: list in the file it applies to."
  :type '(choice regexp (const nil))
  :group 'outlines)

(defcustom numbered-outline-regexp-match-whole-number 1
  "*The capturing parentheses in `numbered-outline-regexp' that capture
the whole heading number."
  :type 'integer
  :group 'outlines)

(defcustom numbered-outline-regexp-match-last-number 2
  "*The capturing parentheses in `numbered-outline-regexp' that capture
the last component of the heading number, if any."
  :type 'integer
  :group 'outlines)

(defcustom numbered-outline-mode nil
  "Non-nil if using Numbered Outline mode with Outline mode."
  :type 'boolean
  :group 'outlines)

(make-variable-buffer-local 'numbered-outline-mode)

(unless (assq 'numbered-outline-mode minor-mode-alist)
  (setq minor-mode-alist
        (append minor-mode-alist
                (list '(numbered-outline-mode "")))))

(defun numbered-outline-level ()
  "Return the level of the heading on the current line.
Point must be at the beginning of the heading."
  (save-excursion
    (looking-at numbered-outline-regexp)
    (numbered-outline-depth (match-string 0))))

(defun numbered-outline-depth (heading)
  "Returns the heading level of the string HEADING."
  ;; This is just a count of the periods in the heading, plus 1.
  (1+ (loop for c across heading count (eq c ?.))))

(defun numbered-outline-renumber ()
  "Renumber headings in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((stack nil)
          (whole numbered-outline-regexp-match-whole-number))
      (while (re-search-forward numbered-outline-regexp nil t)
        (let ((level (numbered-outline-depth (match-string whole))))
          (while (< level (length stack)) (pop stack))
          (while (> level (length stack)) (push 0 stack))
          (incf (car stack)))
        (delete-region (match-beginning whole) (match-end whole))
        (apply #'insert (nreverse (butlast (mapcan #'(lambda (x) (list (number-to-string x) ".")) stack))))))))

(defun numbered-outline-promote (start end)
  "Promote each numbered heading in the region by one level"
  (interactive "r")
  (save-restriction
    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((last numbered-outline-regexp-match-last-number)
            (whole numbered-outline-regexp-match-whole-number))
        (while (re-search-forward numbered-outline-regexp nil t)
          (when (< 1 (numbered-outline-depth (match-string whole)))
            (delete-region (match-beginning last) (match-end last)))))))
  (numbered-outline-renumber))

(defun numbered-outline-demote (start end)
  "Demote each numbered heading in the region by one level"
  (interactive "r")
  (save-restriction
    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward numbered-outline-regexp nil t)
        (goto-char (match-end numbered-outline-regexp-match-whole-number))
        (insert ".1"))))
  (numbered-outline-renumber))

;;;###autoload
(defun numbered-outline-mode (&optional arg)
  "Toggle Numbered Outline mode.
With arg, turn Numbered Outline mode on if arg is positive, off otherwise.

Numbered Outline mode modifies Outline mode and Outline minor mode to
deal with numbered headings of the form 1, 2.3, 4.5.6 and so on.

Commands:
\\[numbered-outline-promote]   promote region by one heading level
\\[numbered-outline-demote]   demote region by one heading level
\\[numbered-outline-renumber]   renumber headings in buffer

When the variable `numbered-outline-renumber-on-save' is non-nil,
the headings in the buffer are renumbered whenever the buffer is
saved.

The regexp `numbered-outline-regexp' identifies heading lines.

When Numbered Outline mode is turned on, it runs the hooks in
`numbered-outline-hook'."
  (interactive "P")
  (setq numbered-outline-mode
        (if (null arg) (not numbered-outline-mode)
          (> (prefix-numeric-value arg) 0)))
  (when numbered-outline-mode
    ;; Turn on Outline minor mode unless it's already on.
    (unless (or (eq major-mode 'outline-mode) outline-minor-mode)
      (outline-minor-mode 1))
    (setq outline-regexp numbered-outline-regexp)
    (setq outline-level #'numbered-outline-level)
    (add-hook 'local-write-file-hooks #'numbered-outline-save)
    (run-hooks 'numbered-outline-mode-hook)))

(defun numbered-outline-save ()
  "Renumber before saving, if appropriate."
  (and numbered-outline-renumber-on-save
       numbered-outline-mode
       (or (eq major-mode 'outline-mode) outline-minor-mode)
       (numbered-outline-renumber)))

(provide 'numbered-outline)

;;; numbered-outline.el ends here
