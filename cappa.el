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
  "http://catalysis-preprint-archive.github.io/cappa/preprints/"
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
		   "http://catalysis-preprint-archive.github.io/cappa/preprints/archive-contents")
		(goto-char url-http-end-of-headers)
		(read (current-buffer)))))
    (cdr data)))


;; This is not working. You have to do this (require 'kitchingroup-57) to get it
;; in cappa-preprints.
(defvar helm-cappa-installed-preprints
  (helm-build-sync-source "CaPPA installed preprints"
    :candidates (loop for entry in  cappa-preprints
		      collect
		      (cons (car entry) entry))
    :action (lambda (entry)
	      (find-file
	       (file-name-directory
		(find-library (symbol-name (car entry)))))))
  "Helm source for installed preprints.")


(defvar helm-cappa-available-preprints
  (helm-build-sync-source "CaPPA available preprints"
    :candidates (loop for (label . properties) in (cappa-available-preprints)
		      if (not (memq label (mapcar 'car cappa-preprints)))
		      collect
		      (cons label (cons label properties)))
    :action (lambda (candidate)
	      (package-install (car candidate))))
  "Helm source for preprints in CaPPA.
TODO: remove installed packages from the list.
Make updates apparent.")

;; (defvar helm-cappa-submit-preprint
;;   ()
;;   "Helm source for submitting a preprint.")

(defun cappa ()
  "Helm interface to CaPPA."
  (helm :sources '(helm-cappa-installed-preprints
		   helm-cappa-available-preprints
		   ;; helm-cappa-submit-preprint
		   ))
  )


;; * Submit a recipe
(defun cappa-submit-preprint ()
  "Submit preprint to CaPPA."
  (interactive)
  (let (label repo)
    ;; get the label, assume it is the directory name.
    (setq label (file-name-base
		 (directory-file-name
		  (file-name-directory (buffer-file-name)))))

    ;; check for label.el
    (unless (file-exists-p (concat label ".el"))
      (message "You must create %s. Doing that now.")
      (find-file (concat label ".el"))
      (auto-insert)
      (error "Please complete this file and try again."))

    ;; check for README.org
    (unless (file-exists-p "README.org")
      (find-file "README.org")
      (error "Please complete the README.org file and try again."))

    ;; check for the repo
    (setq repo
	  (s-trim
	   (nth 1
		(s-split
		 ":"
		 (shell-command-to-string
		  "git config --get remote.origin.url"))))))

  ;; generate the recipe file
  (with-temp-file "recipe"
    (insert (format "(%s
 :fetcher github
 :repo \"%s\"
 :files (\"*\"))" label repo)))
  (message "Send the contents of the recipe file to jkitchin@andrew.cmu.edu"))

(provide 'cappa)
;;; cappa.el ends here
