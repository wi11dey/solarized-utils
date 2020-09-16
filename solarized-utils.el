;;; solarized-utils.el --- Improved `create-face-spec' macro for `color-theme-solarized' -*- lexical-binding: t -*-

;; Author: Will  Dey
;; Maintainer: Will Dey
;; Version: 1.0.0
;; Package-Requires: ((color-theme-solarized ""))
;; Homepage: https://github.com/wi11dey/solarized-utils
;; Keywords: keywords

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; Generate README:
;;; Commentary:

;; commentary

;;; Code:

(require 'solarized-definitions)

(defun solarized-utils--get-color (color index &optional light)
  (when (eq color 'back)
    (setq color 'base03))
  (when light
    (setq color (pcase color
		  ('base03 'base3)
		  ('base02 'base2)
		  ('base01 'base1)
		  ('base00 'base0)
		  ('base0 'base00)
		  ('base1 'base01)
		  ('base2 'base02)
		  ('base3 'base03)
		  (_ color))))
  (nth index (assoc color solarized-colors)))

(defun solarized-utils--recursive-replace-colors (sexp index &optional light)
  (cond ((consp sexp)
	 (cons (solarized-utils--recursive-replace-colors (car sexp) index light)
	       (solarized-utils--recursive-replace-colors (cdr sexp) index light)))
	((null sexp)
	 nil)
	((symbolp sexp)
	 (or (solarized-utils--get-color sexp index light)
	     sexp))
	(t
	 sexp)))

(defun solarized-utils--for-index (facespec index &optional light)
  "Creates a face from facespec where the colors use the names from
  `solarized-colors`."
  (when (= index 5)
    (setq facespec (copy-sequence facespec))
    (when (eq (plist-get facespec :background) 'back)
      (setq facespec (plist-put facespec :background nil)))
    (when (memq (plist-get facespec :foreground) '(base0 base1))
      (setq facespec (plist-put facespec :foreground nil))))
  (solarized-utils--recursive-replace-colors facespec index light))

;;;###autoload
(defun solarized-create-face-spec-form (name &rest facespec)
  `(,name ((((background dark) (type graphic))
            ,(solarized-utils--for-index facespec
					 (cond (solarized-degrade     3)
					       (solarized-broken-srgb 2)
					       (t                     1))))
           (((background dark) (type tty) (min-colors 256))
            ,(solarized-utils--for-index facespec
					 (if (= solarized-termcolors 16) 4 3)))
           (((background dark) (type tty) (min-colors  16))
            ,(solarized-utils--for-index facespec 4))
           (((background dark) (type tty) (min-colors   8))
            ,(solarized-utils--for-index facespec 5))
           (((background light) (type graphic))
            ,(solarized-utils--for-index facespec
					 (cond (solarized-degrade     3)
					       (solarized-broken-srgb 2)
					       (t                     1))
					 t))
           (((background light) (type tty) (min-colors 256))
            ,(solarized-utils--for-index facespec
					 (if (= solarized-termcolors 16) 4 3)
					 t))
           (((background light) (type tty) (min-colors  16))
            ,(solarized-utils--for-index facespec 4 t))
           (((background light) (type tty) (min-colors   8))
            ,(solarized-utils--for-index facespec 5 t)))
	  :now))

;;;###autoload
(defmacro solarized-create-face-spec (name &rest facespec)
  `',(apply #'solarized-create-face-spec-form name facespec))

;;;###autoload
(defmacro solarized-set-faces (&rest args)
  `(progn
     (put 'solarized 'theme-immediate t)
     (custom-theme-set-faces 'user
			     ,@(mapcar (lambda (arg)
					 `',(apply #'solarized-create-face-spec-form arg))
				       args))))

(provide 'solarized-utils)
