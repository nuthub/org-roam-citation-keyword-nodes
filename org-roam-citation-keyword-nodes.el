;;; org-roam-citation-keyword-nodes.el --- Synchronizing keywords in a citar bibliography with org-roam  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2023  Julian Flake

;; Author: Julian Flake <flake@uni-koblenz.de>
;; Created: 10 Dec 2023
;; URL: https://github.com/nuthub/org-roam-citation-keyword-nodes
;; Version: 0.1
;; Keywords: bib, convenience, files, hypermedia
;; Package-Requires: ((citar "1.4") (org-roam "2.2"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; TODO: make pull request to org-roam for including the nocase &optional

;;; Code:

(require 'org-roam)
(require 'citar)

;; Variables
(defvar jf/org-roam-references-keyword-field
  "keywords"
  "The key (as in key/value) of the bibtex field that contains keywords.
Is \"keywords\" in the typical use case, but may also be e.g. \"groups\"
if you want to create roam nodes for JabRef groups.
Set jf/org-roam-references-keyword-field to the delimiter, the different
keywords are separated by.  The keywords are trimmed after separation.")

(defvar jf/org-roam-references-keyword-separator
  ","
  "The character, the bibtex keyword entries are separated by.
The delimiter of the entries in the keyword field.  This is the string
separating the values in the jf/org-roam-references-keyword-field of bibtex
entries.  An alternative could be \";\".")

(defvar jf/org-roam-references-capture-template-key
  "d"
  "The key (as in keyboard) of the template to use for new org-roam nodes.
The template key of the templates in in org-roam-capture-templates to use for
creating new nodes.  If the value is nil, the (first) template in
jf/org-roam-references-capture-fallback-template is used.")

(defvar jf/org-roam-references-capture-fallback-template
  '("d" "default" plain "%?"
    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)
  "A fallback template that is used, if the capture template key is nil.
The capture template key is set in jf/org-roam-references-capture-template-key.")

(defvar jf/org-roam-references-heading
  "References"
  "The org heading that should contain the references added to a keyword node.")

(defvar jf/org-roam-references-heading-filter "LEVEL=1"
  "The tags/property/todo match expression for searching the reference heading.
This is the MATCH string applied to org-map-entries, while scanning for
existence of the heading, the references should be added to.  Any kind of
tags/property/todo match expression is allowed here.")


;; Functions

(defun jf/org-roam-references--add-reference-to-roam-node-if-not-exists (node citekey)
  "Add to the org-roam node NODE a reference to CITEKEY."
  (let ((citation (citar-get-entry citekey)))
    (save-excursion
      ;; open roam node file
      (org-roam-node-open node)
      ;; Go to the beginning of the buffer.
      (goto-char (point-min))
      ;; check, if reference is already contained
      (when (eq nil (search-forward (concat "@" citekey) nil t))
	;; make sure that the References heading exists
	(when (not (member jf/org-roam-references-heading
			   (org-map-entries (lambda () (nth 4 (org-heading-components)))
					    jf/org-roam-references-heading-filter)))
	  (goto-char (point-max))
	  (insert (concat "\n* " jf/org-roam-references-heading "\n")))
	;; search for and go to the references heading
	(search-forward (concat "\n* " jf/org-roam-references-heading "\n") nil t)
	;; Jump to the end of the content (if there is content)
	(goto-char (or
		    (org-element-property :contents-end (org-element-at-point))
		    (point)))
	;; insert a newline, if we aren't on a blank line
	(when (not (lambda () (beginning-of-line) (looking-at-p "[[:blank:]]*$")))
	  (newline))
	;; Point is now at the end of the content in a blank line. Insert the requested text.
	(insert (concat "- [cite:@" citekey "] "
			(cdr (assoc "author" citation))
			" :: "
			(cdr (assoc "title" citation))
			", " (cdr (assoc "year" citation)))))

      ;; save & close
      (save-buffer)
      ;; TODO kill buffer only, if not visited
      (kill-buffer))
    ))

(defun jf/org-roam-references--get-all-keywords-of-citation (citation)
  "Return a list of strings with all keywords of the CITATION.
If no keywords were found, return the empty string."
  (let ((keywords-string (cdr (assoc jf/org-roam-references-keyword-field citation))))
    (split-string
     (or keywords-string "")
     jf/org-roam-references-keyword-separator
     t " ")))


(defun jf/org-roam-references--create-get-roam-node (title &optional force)
  "Create or get existing roam node with title TITLE.
The user is asked for each new node, unless FORCE is t."
  (save-excursion
    (or
     (org-roam-node-from-title-or-alias title t)
     (progn
       (let* ((templatekey jf/org-roam-references-capture-template-key))
	 (when (or force
		   (y-or-n-p (concat "Create a org-roam node \"" title "\"?")))
	   (apply 'org-roam-capture-
		  :info (list :title title)
		  :node (org-roam-node-create :title title)
		  :props '(:immediate-finish t :kill-buffer t)
		  (if templatekey
		      (list :keys templatekey)
		    (list
		     :templates
		     (list jf/org-roam-references-capture-fallback-template))))))
       (org-roam-node-from-title-or-alias title t)))))

;;
;; The command to start the synchronization.
;;
(defun jf/org-roam-references-sync-keywords-to-roam-db (&optional force)
  "Synchronize the citations' keywords with org-roam nodes.
Only the direction bibliography -> org-roam is supported.
If the optional FORCE is t, nodes are created without asking the user."
  (interactive)
  
  (maphash
   (lambda (citekey citation)
     (message (concat "Now processing " citekey))
     ;; get all keywords of citation
     (let ((keywords (jf/org-roam-references--get-all-keywords-of-citation citation)))
       ;; 0. with every keyword
       (mapc (lambda (keyword)
	       ;; 1. create (or get) roam node for the keyword
	       (let ((node
		      (jf/org-roam-references--create-get-roam-node keyword force)))
		 ;; 2. add all reference to the keyword's node
		 (jf/org-roam-references--add-reference-to-roam-node-if-not-exists node citekey)))
	     keywords)))
   (citar-get-entries))
  )

(provide 'org-roam-citation-keyword-nodes)

;;; org-roam-citation-keyword-nodes.el ends here
