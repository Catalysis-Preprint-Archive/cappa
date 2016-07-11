;;; cappa.el --- Catalysis Preprint Archive          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Version: 0.0.1
;; Keywords:

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

;; This package provides functions to interact with the Catalysis Preprint
;; Archive.

;; When you install a preprint, it will be registered so you can easily find it
;; and open it.


;;; Code:

(defvar cappa-repo
  "http://catalysis-preprint-archive.github.io/preprints/"
  "URL to the preprint archive.")

(add-to-list 'package-archives (cons "CaPPA" cappa-repo) t)

(defvar cappa-preprints '()
  "Contains a list of installed preprints.")

(defun cappa-register (preprint-label &rest args)
  "Register the symbol PREPRINT-LABEL in `cappa-preprints'."
  (add-to-list 'cappa-preprints (cons preprint-label args)))


;;* Helm interface
(defun cappa-available-preprints ()
  "Return list of available preprints.
Returns a list of (cons label properties)."
  (let ((data (with-current-buffer
		  (url-retrieve-synchronously
		   (concat cappa-repo "archive-contents"))
		(goto-char url-http-end-of-headers)
		(read (current-buffer)))))
    (cdr data)))



(defun helm-cappa-candidates ()
  "List of CaPPA preprints that are not installed."
  (loop for (label . properties) in (cappa-available-preprints)
	if (not (package-installed-p label))
	collect
	(cons
	 (format "%s | %s" label (elt properties 0))
	 (cons label properties))))

(defun helm-cappa-installed-candidates ()
  (loop for (label . properties) in (cappa-available-preprints)
	if (package-installed-p label)
	collect
	(cons
	 (format "%s | %s" label (elt properties 0))
	 (cons label properties))))


(defvar helm-cappa-installed-preprints
  (helm-build-sync-source "CaPPA installed preprints"
    :candidates 'helm-cappa-installed-candidates
    :action (lambda (entry)
	      (find-file
	       (expand-file-name
		"README.org"
		(file-name-directory
		 (locate-library (symbol-name 'kitchingroup-9)))))))
  "Helm source for installed preprints.")


(defvar helm-cappa-available-preprints
  (helm-build-sync-source "CaPPA available preprints"
    :candidates 'helm-cappa-candidates
    :action (lambda (candidate)
	      (package-install (car candidate))))
  "Helm source for preprints in CaPPA.
TODO: remove installed packages from the list.
Make updates apparent.")

(defvar helm-cappa-submit-preprint
  (helm-build-dummy-source "CaPPA submit preprint"
    :filtered-candidate-transformer
    (lambda (_candidates _source)
      (list (or (and (not (string= helm-pattern ""))
		     helm-pattern)
		"Submit preprint")))
    :action '(("Submit preprint" . (lambda (candidate)
				     (cappa-submit-preprint)))))
   "Helm source for submitting a preprint.")

(defun cappa ()
  "Helm interface to CaPPA."
  (interactive)
  (helm :sources '(helm-cappa-installed-preprints
		   helm-cappa-available-preprints
		   helm-cappa-submit-preprint)))


;; * Generate a recipe from a bibtex file
(defun cappa-clean-bibtex-string (s)
  "Clean a bibtex string S."
  (setq s (replace-regexp-in-string "^{\\|}$" "" s))
  (setq s (replace-regexp-in-string "[\n\\|\t\\|\s]+" " " s))
  s)


(defun cappa-bibtex-recipe ()
  "Generate a package from the bibtex entry at point.
This uses the key as the label for the preprint, and constructs
the label.el file."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((bibtex-data (bibtex-parse-entry t))
	 (bibtex-text )
	 (label (cdr (assoc "=key=" bibtex-data)))
	 (title (cappa-clean-bibtex-string (cdr (assoc "title" bibtex-data))))
	 (doi (cdr (assoc "doi" bibtex-data)))
	 (year (cdr (assoc "year" bibtex-data)))
	 (journal (cdr (assoc "journal" bibtex-data)))
	 (el-file (format "%s.el" label))
	 bibtex authors)

    ;; Get the actual bibtex text, and format it for the el file
    (bibtex-copy-entry-as-kill)
    (setq bibtex (with-temp-buffer
		   (bibtex-yank)
		   (goto-char (point-min))
		   (while (not (eobp))
		     (forward-line)
		     (beginning-of-line)
		     (insert ";;  ")
		     (end-of-line))
		   (buffer-string)))

    ;; format the authors in the multiline form for the el file
    (setq authors
	  (with-temp-buffer
	    (cl-loop for au in (split-string
				(cappa-clean-bibtex-string
				 (cdr (assoc "author" bibtex-data)))
				" and ")
		     do
		     (insert (format "%s\n" au)))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (forward-line)
	      (beginning-of-line)
	      (insert ";;  ")
	      (end-of-line))
	    (buffer-string)))

    ;; generate the el-file
    (with-temp-file el-file
      (insert
       (s-format ";;; ${label}.el --- ${title}

;; Copyright (C) ${current-year} ${submitter} ${email}

;; Version: 0.0.1
;; Author: ${authors}
;; DOI: ${doi}
;; Year: ${year}
;; Journal: ${journal}
;; Bibtex: ${bibtex}

;;; Comentary:
;; Generated on ${generation-date}
;;; Code:
\(require 'cappa)

\(cappa-register '${label})

\(provide '${label})
;;; ${label}.el ends here"
		 'aget
		 `(("label" . ,(or label "no-label"))
		   ("submitter" . ,(user-full-name))
		   ("email" . ,(format "<%s>" user-mail-address))
		   ("generation-date" . ,(current-time-string))
		   ("title" . ,(or title "no title"))
		   ("authors" . ,(or authors "no authors"))
		   ("journal" . ,(or journal "no journal"))
		   ("year" . ,year)
		   ("doi" . ,(or doi "nodoi"))
		   ("bibtex" . ,(or bibtex "no bibtex"))
		   ("current-year" . ,(format "%s"
					      (calendar-extract-year
					       (calendar-current-date))))))))

    ;; Now generating the recipe file is trickier. We could probe around for the
    ;; info, in .git, .hg etc... or go interactive and prompt the user if we
    ;; can't figure it out.
    ))


;; * Submit a recipe
(defun cappa-submit-preprint ()
  "Submit preprint to CaPPA.
Works for github only right now."
  (interactive)
  (let (label repo)
    ;; get the label, assume it is the directory name.
    (setq label (file-name-base
		 (directory-file-name
		  (file-name-directory (buffer-file-name)))))

    ;; check for label.el
    (unless (file-exists-p (concat label ".el"))
      (message "You must create %s. Doing that now." (concat label ".el"))
      (find-file (concat label ".el"))
      (insert (s-format
	       ";;; ${label}.el --- ${description}

;; Copyright (C) ${year} ${name}

;; Version: 0.0.1
;; Author: ${name} <${email}>
;; Keywords:
;; DOI:
;; Journal:
;; Bibtex:

;; ${license}

;;; Commentary:

;; Put some descriptive text here.

;;; Code:
\(require 'cappa)

\(cappa-register '${label})


\(provide '${label})
;;; ${label}.el ends here
"
	       'aget
	       `(("label" . ,label)
		 ("description" . "CaPPA preprint for doi:")
		 ("year" . ,(format "%s" (calendar-extract-year (calendar-current-date))))
		 ("license" . "TBD")
		 ("name" . ,user-full-name)
		 ("email" . ,user-mail-address))))
      (message-box "You need to complete %s.el before submitting." label))

    ;; check for README.org
    (unless (file-exists-p "README.org")
      (find-file "README.org")
      (message-box "Please complete the README.org file before submitting."))

    ;; check for the repo
    (setq repo
	  (replace-regexp-in-string
	   "\\.git" ""
	   (s-trim
	    (nth 1
		 (s-split
		  ":"
		  (shell-command-to-string
		   "git config --get remote.origin.url"))))))

    ;; generate the recipe file
    (with-temp-file label
      (insert (format "(%s
 :fetcher github
 :repo \"%s\"
 :files (\"*\"))" label repo)))
    (message "Send the contents of the %s file to jkitchin@andrew.cmu.edu" label)))

(provide 'cappa)
;;; cappa.el ends here
