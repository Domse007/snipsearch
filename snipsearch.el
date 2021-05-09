;;; snipsearch.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Dominik Keller

;; Author: Dominik Keller <user@user.com>
;; Keywords: lisp, macros
;; Version: 0.0.1

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

;; This package provides a simple interface to insert custom macros, that
;; can be defined by the user.

;;; Code:

(defgroup snipsearch nil
  "Provide a simple interface to search for snippets."
  :prefix "snipsearch-"
  :group 'snipsearch)

(defcustom snipsearch-author ""
  "Set the default author of org files."
  :type 'string
  :group 'snipsearch)

(defcustom snipsearch-list '()
  "List with all macros"
  :type 'list
  :group 'snipsearch)

(defcustom snipsearch-comp-interface 'default
  "Symbol that indicates prefered completion frontend"
  :type 'symbol
  :options '(default helm ivy)
  :group 'snipsearch)

(define-minor-mode snipsearch-mode
  "Easy access to snippets by short keywords."
  :keymap '(([?\C-c ?\m] . snipsearch))
  :lighter " snipsearch")

(defun snipsearch-insert (snipsearch--insert-start-list)
  (let ((snipsearch--insert-list snipsearch--insert-start-list)
	(snipsearch--insert-title (concat (upcase (substring (buffer-name) 0 1))
					  (substring (buffer-name) 1 -4))))
    (progn (insert (format (nth 1 snipsearch--insert-list)
		   snipsearch--insert-title
		   snipsearch-author
		   ))
	   (forward-char (nth 2 snipsearch--insert-list)))))

(defun snipsearch--get-names ()
  (let (snipseach--loop-result)
    (dolist (snipsearch--loop-var snipsearch-list snipseach--loop-result)
      (setq snipseach--loop-result (concat snipseach--loop-result "[" (car snipsearch--loop-var) "], ")))
    snipseach--loop-result))

(defun snipsearch-interface ()
  (let* ((snipsearch--macro-names (snipsearch--get-names))
	 (snipsearch--user-input (read-string snipsearch--macro-names))
	 (snipsearch--interface-result ""))
    (dolist (snipsearch--interface-loop snipsearch-list snipsearch--interface-result)
      (when (string-equal (car snipsearch--interface-loop) snipsearch--user-input)
	(snipsearch-insert snipsearch--interface-loop)))))

(defun snipsearch ()
  "Provide different options for interfacing with the user."
  (interactive)
  (let ((test 1))
    (progn
      (cond
       ((equal snipsearch-comp-interface 'default)
	(snipsearch-interface))
       ((equal snipsearch-comp-interface 'ivy)
	(ivy-read "Select snippet: "
		  snipsearch-list
		  :preselect (ivy-thing-at-point)
		  :require-match t
		  :action (lambda (result)
			    (snipsearch-insert result))))
       ((equal snipseach-comp-interface 'helm)
	(helm :sources (helm-build-sync-source "snipsearch"
			 :candidates snipsearch-list
			 :fuzzy-match t)
	      :buffer "*snipsearch*"))))))

(provide 'snipsearch)
;;; snipsearch.el ends here
