;;; org-roam-citation-keyword-nodes.el --- Synchronizing keywords in a citar bibliography with org-roam  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2023  Julian Flake

;; Author: Julian Flake <julian@flake.de>
;; Created: 10 Dec 2023
;; URL: https://github.com/nuthub/org-roam-citation-keyword-nodes
;; Version: 0.1
;; Keywords: bib, convenience, files, hypermedia
;; Package-Requires: ((citar "1.4") (org-roam "2.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; TODO: make it interactive (let user decide, whether a new node should be created)
;; TODO: make pull request to org-roam for including the nocase &optional

;;; Code:

(require 'org-roam)
(require 'citar)

;; Variables
(defvar jf/org-roam-references-keyword-field
  "keywords"
  "A string.  The name of the bibtex field that contains keywords.
Is \"keywords\" in the typical use case, but may also be e.g. \"groups\"
if you want to create roam nodes for JabRef groups.
Set jf/org-roam-references-keyword-field to the delimiter, the different
keywords are separated by.  The keywords are trimmed after separation.")

(defvar jf/org-roam-references-keyword-separator
  ","
  "The delimiter of the entries in the keyword field.
This is a string separating the values in jf/org-roam-references-keyword-field.")

(defvar jf/org-roam-references-capture-template-key
  "d"
  "The template key in org-roam-capture-templates to use for creating new nodes.
If the value is nil, the template in
jf/org-roam-references-capture-fallback-template is used.")

(defvar jf/org-roam-references-capture-fallback-template
  '("d" "default" plain "%?"
    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)
  "A fallback template, if jf/org-roam-references-capture-template-key is nil.")

(defvar jf/org-roam-references-heading
  "References"
  "The heading that should contain the references added to a keyword node.")

(defvar jf/org-roam-references-heading-filter "LEVEL=1"
  "Filter string for searching reference heading.
This is MATCH string applied to org-map-entries, while scanning for existence of
the heading, the references should be added to.")


;; Functions

(defun jf/org-roam-references--get-node-from-title-or-alias (s &optional nocase)
  "Retrieves the node that has S as title or alias.
If NOCASE is t, the query is case insensitive.  It is case sensitive otherwise."
  ;; Search for nodes in the roam db that have the provided S as title or alias. There should be only one such node.
  (let ((matches
	 (seq-uniq
	  (append
	   (org-roam-db-query (vconcat [:select [id] :from nodes
						:where (= title $s1)]
				       (if nocase [ :collate NOCASE ]))
			      s)
	   (org-roam-db-query (vconcat [:select [node-id] :from aliases
						:where (= alias $s1)]
				       (if nocase [ :collate NOCASE ]))
			      s)))))
    (cond
     ((seq-empty-p matches)
      nil)
     ((= 1 (length matches))
      (org-roam-populate (org-roam-node-create :id (caar matches))))
     (t
      (user-error "Multiple nodes exist with title or alias \"%s\"" s)))))

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
     (jf/org-roam-references--get-node-from-title-or-alias title t)
     (progn (let* ((templatekey jf/org-roam-references-capture-template-key))
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
	    (jf/org-roam-references--get-node-from-title-or-alias title t)))))

;;
;; The command to start the synchronization.
;;
(defun jf/org-roam-references-sync-keywords-to-roam-db (&optional force)
  "Synchronize the citations' keywords with org-roam nodes.
Only the direction bibliography -> org-roam is supported.
If the optional FORCE is t, nodes are created without asking the user."
  (interactive)
  
  (maphash (lambda (citekey citation)
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

;;; org-roam-citation-keyword-nodes.el ends here
