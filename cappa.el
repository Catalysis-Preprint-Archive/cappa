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

;; This package provides the Catalysis Preprint Archive.

;;; Code:

(defvar cappa-preprints '()
  "Contains a list of installed preprints.")

(defun cappa-register (preprint-label &rest args)
  "Register the symbol PREPRINT-LABEL in `cappa-preprints'."
  (add-to-list 'cappa-preprints (cons preprint-label args)))


;; TODO cappa-helm function

(provide 'cappa)
;;; cappa.el ends here
