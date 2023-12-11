;;; org-roam-citation-keyword-nodes.el --- This package synchronizes keywords found in a bibliography (a bibtex file) with org-roam nodes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Julian Flake

;; Author: Julian Flake <julian@flake.de>
;; Keywords: citar, org-roam


(defvar jf/org-roam-references-keyword-field
  "keywords"
  "A string. The name of the bibtex field that contains keywords. Is \"keywords\" in the typical use case, but may also be e.g. \"groups\", if you want to create roam nodes for JabRef groups.
Set jf/org-roam-references-keyword-field to the delimiter, the different keywords are separated by. The keywords are trimmed after separation.")

(defvar jf/org-roam-references-keyword-separator
  ","
  "A string. The delimiter of the entries in the jf/org-roam-references-keyword-field.")

(defvar jf/org-roam-references-capture-template-key
  "d"
  "The key of the template in org-roam-capture-templates to use for creating new nodes. If the value is nil, the template in jf/org-roam-references-capture-fallback-template is used.")

(defvar jf/org-roam-references-capture-fallback-template
  '("d" "default" plain "%?"
    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)
  "A fallback template, if jf/org-roam-references-capture-template-key is nil.")

(defvar jf/org-roam-references-heading
  "References"
  "The heading that should contain the references added to a keyword node.")

(defvar jf/org-roam-references-heading-filter "LEVEL=1"
  "The MATCH string applied to org-map-entries, while scanning for exitence of the heading, the references should be added to.")


(defun jf/org-roam-references--get-node-from-title-or-alias (s &optional nocase)
  "Retrieves the node that has S as title or alias.
If NOCASE is t, the query is case insensitive. It is case sensitive otherwise.
TODO: make pull request to org-roam"
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

(defun jf/org-roam-references--add-reference-to-roam-node-if-not-exists (nodetitle citekey)
  "Add to the org-roam node NODETITLE a reference to CITEKEY."
  (let ((citation (citar-get-entry citekey))
	(roam-node (jf/org-roam-references--get-node-from-title-or-alias nodetitle t)))
    ;; open roam node file
    (org-roam-node-open roam-node)
    (save-excursion
      ;; Go to the beginning of the buffer.
      (beginning-of-buffer)
      ;; check, if reference is already contained
      (when (eq nil (search-forward (concat "@" citekey) nil t))
	;; make sure that the References heading exists
	(when (not (member jf/org-roam-references-heading
			   (org-map-entries (lambda () (nth 4 (org-heading-components)))
					    jf/org-roam-references-heading-filter)))
	  (end-of-buffer)
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
			", " (cdr (assoc "year" citation))))))

    ;; save & close
    (save-buffer)
    ;; TODO kill buffer only, if not visited
    (kill-buffer)
    ))

(defun jf/org-roam-references--get-all-keywords-of-citation (citation)
  "Return a list of strings with all keywords of the CITATION. If no keywords were found, return the empty string"
  (let ((keywords-string (cdr (assoc jf/org-roam-references-keyword-field citation))))
    (split-string
     (or keywords-string "")
     jf/org-roam-references-keyword-separator
     t " ")))


(defun jf/org-roam-references--create-get-roam-node (title)
  "Create or get existing roam node with title TITLE."
  (or
   (jf/org-roam-references--get-node-from-title-or-alias title t)
   (progn (let* ((templatekey jf/org-roam-references-capture-template-key))
	    (apply 'org-roam-capture-
		   :info (list :title title)
		   :node (org-roam-node-create :title title)
		   :props '(:finalize find-file)
		   (if templatekey
		       (list :keys templatekey)
		     (list
		      :templates
		      (list jf/org-roam-references-capture-fallback-template)))))
	  (jf/org-roam-references--get-node-from-title-or-alias title t))))




;; The command to start the synchronization.
;;
(defun jf/org-roam-references-sync-keywords-to-roam-db ()
  "Synchronize the citations's keywords with org-roam nodes."
  (interactive)
  
  (org-roam-db-sync)

  (maphash (lambda (CITEKEY CITATION)
	     "Process on citation"
	     ;; get all keywords
	     (let ((keywords (jf/org-roam-references--get-all-keywords-of-citation CITATION)))
	       ;; 0. do something with all keywords
	       (mapc (lambda (KEYWORD)
		       "Process a keyword for a citation."
		       ;; 1. create roam nodes for the keyword
		       (jf/org-roam-references--create-get-roam-node KEYWORD)
		       ;; 2. add all references to the keyword's nodes
		       (jf/org-roam-references--add-reference-to-roam-node-if-not-exists KEYWORD CITEKEY))
		     keywords)
	       ;; (jf/add-reference-to-roam-node-if-not-exists)
	       ))
	   (citar-get-entries))
  )
