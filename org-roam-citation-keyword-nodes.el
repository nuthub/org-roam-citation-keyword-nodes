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
(require 'citar-org)

;; Variables
(defvar jf/org-roam-references-keyword-field "keywords"
  "The key (as in key/value) of the bibtex field that contains keywords.
Is \"keywords\" in the typical use case, but may also be e.g. \"groups\"
if you want to create roam nodes for JabRef groups.
Set jf/org-roam-references-keyword-field to the delimiter, the different
keywords are separated by.  The keywords are trimmed after separation.")

(defvar jf/org-roam-references-keyword-separator ","
  "The character, the bibtex keyword entries are separated by.
The delimiter of the entries in the keyword field.  This is the string
separating the values in the jf/org-roam-references-keyword-field of bibtex
entries.  An alternative could be \";\".")

(defvar jf/org-roam-references-capture-template-key "d"
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

(defvar jf/org-roam-references-heading "References"
  "The org heading that should contain the references added to a keyword node.
The heading needs to be at level 1 and will be created as level 1 heading,
if it does not exist.")

(defvar jf/confirmation-function 'y-or-n-p
  "The function used to ask the user for confirmation.
This is used when references are about to be removed or new nodes are about
to be created.")

;; Functions

(defun jf/org-roam-references--create-get-roam-node (title &optional force)
  "Create or get existing roam node with title TITLE.
The user is asked for each new node, unless FORCE is t.
Parts of the code are from citar-org-roam."
  (save-excursion
    (or
     (jf/org-roam-references--get-node-from-title-or-alias title t)
     (progn
       (let ((templatekey jf/org-roam-references-capture-template-key))
	 (when (or force
		   (funcall jf/confirmation-function (concat "Create a org-roam node \"" title "\"?")))
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

(defun jf/org-roam-references--get-node-from-title-or-alias (s &optional nocase)
  "Retrieves the node that has S as title or alias.
If NOCASE is t, the query is case insensitive.  It is case sensitive otherwise.
This is an adoption of org-roam-node-from-title-or-alias.  PR pending."
  ;; Search for nodes in the roam db that have the provided S as title or alias.
  ;; There should be only one such node.
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
(make-obsolete 'jf/org-roam-references--get-node-from-title-or-alias nil "2023-12-11")

(defun jf/org-roam-references--goto-references-heading-end (node)
  "Go to end of content of NODE's references heading.
The heading is created, if it does not exist."
  ;; open roam node file
  (org-roam-node-open node)
  ;; if there is no references heading, create one.
  ;; TODO can be achieved with a capture? What's better?
  (when (not (member jf/org-roam-references-heading
		     (org-map-entries
		      (lambda () (nth 4 (org-heading-components)))
		      "LEVEL=1")))
    (goto-char (point-max))
    (insert (concat "\n* " jf/org-roam-references-heading)))
  ;; search for and go to the (first) references heading
  ;; it should exist now
  (goto-char (point-min))
  (re-search-forward (concat "\n* " jf/org-roam-references-heading "$"))
  ;; Jump to the end of the content (if there is content)
  (goto-char (1- (or
		  (org-element-property :contents-end (org-element-at-point))
		  (point)))))

(defun jf/org-roam-references--add-reference-to-roam-node (node citekey)
  "Add to the org-roam node NODE a reference to CITEKEY.
The reference is only added, if it is not present in the node, already.
If the reference is added, it is added to the references heading.
The references heading is defined in jf/org-roam-references-heading."
  ;; Go to the beginning of the buffer.
  (org-roam-node-open node)
  (save-excursion
    (goto-char (point-min))
    ;; check, if reference is already contained
    (when (not (jf/org-roam-references--search-forward-citekey citekey))
      ;; make sure that the References heading exists
      (jf/org-roam-references--goto-references-heading-end node)
      ;; insert a newline, if we aren't on a blank line
      (goto-char (line-end-position))
      (ensure-empty-lines 1)
      (delete-blank-lines)
      (insert (format "- %s[cite:@%s] %s :: %s, %s\n"
		      (if (funcall (citar-has-notes) citekey) "(n) " "")
		      citekey
		      (citar-get-value "author" citekey)
		      (citar-get-value "title" citekey)
		      (or (citar-get-value "year" citekey)
			  (citar-get-value "date" citekey)))))))


(defun jf/org-roam-references--get-keywords-of-citation (citation)
  "Return a list of strings with all keywords of the CITATION.
If no keywords were found, return the empty string."
  (let ((keywords-string (cdr (assoc jf/org-roam-references-keyword-field citation))))
    (split-string
     (or keywords-string "")
     jf/org-roam-references-keyword-separator
     t " ")))

(defun jf/org-roam-references--get-citekeys-by-keyword (keyword)
  "Gives a list of citation keys related to KEYWORD."
  (let ((all-citations (citar-get-entries))
	(ref-citations ()))
    (maphash
     (lambda (citekey citation)
       (let ((keywords-of-citation (jf/org-roam-references--get-keywords-of-citation citation)))
	 (mapcar
	  (lambda (current-keyword)
	    (when (string= (upcase current-keyword) (upcase keyword))
	      (setq ref-citations (cons citekey ref-citations))))
	  keywords-of-citation)))
     all-citations)
    ref-citations))

(defun jf/org-roam-references--get-citations-for-roam-node (node)
  "Return all citations that have keywords matching NODE.
A \"keyword\" matches NODE, when the keyword is either the title or an alias.
The comparison is case insensitive."
  (let ((keywords (cons (org-roam-node-title node)
    			(org-roam-node-aliases node)))
    	(all-refs ()))
    (mapc (lambda (keyword)
    	    (setq all-refs
		  (append
    		   (jf/org-roam-references--get-citekeys-by-keyword keyword)
    		   all-refs)))
    	  keywords)
    all-refs))

(defun jf/org-roam-references--search-forward-citekey (citekey)
  "Move point to a matching CITEKEY in org cite links."
  (let ((regexp (concat "\\[cite:.*@" citekey ".*]"))
	(found nil)
	(beg 0)
	(save-point (point)))
    (while (and (not found)
		(re-search-forward regexp nil t)
		(setq beg (match-beginning 0)))
      (while (and (not (string-equal citekey (citar-key-at-point)))
      		  (search-backward citekey beg t)))
      (setq found (string-equal citekey (citar-key-at-point))))
    ;; move cursor to good position and return meaningful value
    (cond
     (found (goto-char (+ (point) (length citekey)))
	    (citar-key-at-point))
     ((not found) (goto-char save-point)
      nil))))

(defun jf/org-roam-references--ask-for-citation-removal (&optional force)
  "Delete from text the citation key at point.
User gets asked for confirmation, unless FORCE is non-nil."
  (when (or force
	    (funcall jf/confirmation-function
		     (concat "Remove \"@"
			     (citar-key-at-point)
			     "\" (from) citation? ")))
    (citar-org-delete-citation)))

(defun jf/org-roam-references--remove-citation (citekey &optional force)
  "Remove all citations of CITEKEY in current buffer.
If FORCE is t, the citation reference is removed without asking the user."
  (goto-char (point-min))
  ;; search-forward-regexp moves point after citation
  (while (jf/org-roam-references--search-forward-citekey citekey)
    ;; if a citation contains a reference multiple times,
    ;; only the last occurence is considered for removal
    (jf/org-roam-references--ask-for-citation-removal force)))



(defun jf/org-roam-references--get-all-bibliography-keywords ()
  "Return a list of strings with all keywords in bibliography."
  (let ((all-keywords))
    (maphash (lambda (citekey citation)
	       (setq all-keywords
		     (append
		      all-keywords
		      (jf/org-roam-references--get-keywords-of-citation citation))))
	     (citar-get-entries))
    (delq nil (delete-dups all-keywords))))

;;
;; interactive commands
;;

(defun jf/org-roam-references-remove-unresolvable-citations (&optional force)
  "Remove all citation references, that are not found in bibliography.
If FORCE is t, the citation reference is removed without asking the user."
  (interactive)
  ;; Alternative: (if (not (citar-org--reference-at-point)) /delete/)
  (let ((all-citations (citar-get-entries)))
    (dolist (citekey (citar-org-list-keys))
      (when (not (gethash citekey all-citations))
	(jf/org-roam-references--remove-citation citekey force)))))

(defun jf/org-roam-references-add-related-references (&optional node)
  "Add the related references to org-roam NODE.
Acts on (org-roam-node-at-point), unless NODE is non-nil.
Return t, if there were no errors."
  (interactive)
  (let ((node (or node (org-roam-node-at-point))))
    (dolist (keyword (cons (org-roam-node-title node)
			   (org-roam-node-aliases node)))
      (dolist (citekey (jf/org-roam-references--get-citekeys-by-keyword keyword))
	(jf/org-roam-references--add-reference-to-roam-node node citekey)))))

(defun jf/org-roam-references-remove-unrelated-references (&optional force node)
  "Remove all unrelated references from NODE's references heading.
Acts on (org-roam-node-at-point), unless NODE is non-nil.
Asks the user before removal, unless FORCE is non-nil.
This command also works on non org-roam org files.
Return t, if there were no errors."
  (interactive)
  (let* ((node (or node (org-roam-node-at-point)))
	 (related-cites (jf/org-roam-references--get-citations-for-roam-node node)))
    (org-map-entries
     (lambda ()
       ;; act on the relevant heading only
       (when (string= jf/org-roam-references-heading
		      (nth 4 (org-heading-components)))
         (save-excursion
  	   (org-narrow-to-subtree)
	   ;; iterate over all citations
  	   (while (re-search-forward "\\[cite:.*]" nil t)
	     ;; save begin of citations
	     (let ((beg (match-beginning 0)))
  	       (dolist (citekey (citar-citation-at-point))
  		 (when (not (member citekey related-cites))
		   ;; move cursor to begin of citation only, if we want to modify it
		   (goto-char beg)
                   (search-forward (concat "@" citekey))
                   (jf/org-roam-references--ask-for-citation-removal force))))))))
     "LEVEL=1"))t)


(defun jf/org-roam-references-remove-citations-from-roam-db (&optional force)
  "Synchronize the citations' keywords with org-roam nodes.
Only the direction bibliography -> org-roam is supported.
If the optional FORCE is t, nodes are created without asking the user."
  (interactive)
  (save-excursion
    (dolist (keyword (jf/org-roam-references--get-all-bibliography-keywords))
      (let ((node (jf/org-roam-references--create-get-roam-node keyword force)))
	(jf/org-roam-references-remove-unrelated-references force node)
	(save-buffer node)))))


(provide 'org-roam-citation-keyword-nodes)

;;; org-roam-citation-keyword-nodes.el ends here
