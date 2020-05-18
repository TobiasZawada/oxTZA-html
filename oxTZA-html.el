;;; oxTZA-html.el --- export image in orgmode buffers as base64 encoded data to html pages  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Tobias Zawada

;; Author: i@tn-home.de
;; Keywords: outlines

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

;; This package was not intended for publication.
;; Even if it has already been used for a long time its publication is experimental.
;; So use it with some degree of suspicion.
;; The part for base64 encoding of inline images in HTML export
;; and the adaption of texfrag to HTML export should work quite reasonable.

;;; Code:

(require 'org)
(require 'ox-html)
(require 'base64)
(eval-when-compile
  (require 'subr-x))

(defcustom orgTZA-html-image-base64-max-size #x40000
  "Export embedded base64 encoded images up to this size."
  :type 'number
  :group 'org-export-html)

(defcustom orgTZA-html-latex-prefix "org-eq:"
  "Prefix used for the alt-text of images generated from LaTeX formulas."
  :type 'string
  :group 'org-export-html)

(defun orgTZA-file-to-base64-string (file &optional image prefix postfix)
  "Transform binary file FILE into a base64-string prepending PREFIX and appending POSTFIX.
Puts \"data:image/%s;base64,\" with %s replaced by the image type before the actual image data if IMAGE is non-nil."
  (concat prefix
	  (with-temp-buffer
	    (set-buffer-multibyte nil)
	    (insert-file-contents file nil nil nil t)
	    (base64-encode-region (point-min) (point-max) 'no-line-break)
	    (when image
	      (goto-char (point-min))
	      (insert (format "data:image/%s;base64," (image-type-from-file-name file))))
	    (buffer-string))
	  postfix))

(defun orgTZA-html-base64-encode-p (file)
  "Check whether FILE should be exported base64-encoded.
The return value is actually FILE with \"file://\" removed if it is a prefix of FILE."
  (when (and (stringp file)
             (string-match "\\`file://" file))
    (setq file (substring file (match-end 0))))
  (and
   (file-readable-p file)
   (let ((size (nth 7 (file-attributes file))))
     (<= size orgTZA-html-image-base64-max-size))
   file))

(defun orgTZA-html-add-to-list (list element &optional append compare-fn)
  "Supplement LIST with ELEMENT if it is not there yet.
If APPEND is non-nil append ELEMENT to LIST.
Comparison is done with COMPARE-FN which defaults to `equal'.
Returns the modified list.

This function does not modify the original list.
It works on a copy if necessary."
  (if list
      (progn
        (unless compare-fn
          (setq compare-fn #'equal))
        (if (cl-member element list :test compare-fn)
	    list
          (if append
              (let ((new-list (cl-copy-list list)))
                (setcdr (last new-list) (list element))
                new-list)
            (cons element list))))
    (list element)))

(defun orgTZA-html-preview-image-get (preview-image prop)
  "Query PREVIEW-IMAGE for property PROP."
  (let* ((preview-icon (car-safe preview-image))
         (plist (cdr preview-icon)))
    (plist-get plist prop)))

(defvar texfrag-mode)
(defvar texfrag-subdir)

(defun orgTZA-html--format-image (source attributes info)
  "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
  (require 'htmlize)
  (require 'texfrag)
  (if (string= "svg" (file-name-extension source))
      (org-html--svg-image source attributes info)
    (let* ((orgTZA-html-image-base64-max-size (or (plist-get info :embedded-image-size)
                                                  orgTZA-html-image-base64-max-size))
           (file (orgTZA-html-base64-encode-p source))
           (data (if file (orgTZA-file-to-base64-string file t)
		   source))
           (latex-src (get-char-property 0 'org-latex-src source))
           (alt (or latex-src
                    (and (string-match-p "^ltxpng/" source)
                         (org-html-encode-plain-text
                          (org-find-text-property-in-string 'org-latex-src source)))
                    (file-name-nondirectory source)))
           (preview-image (org-find-text-property-in-string 'preview-image source))
           (image-vertical-align (orgTZA-html-preview-image-get preview-image :ascent))
           (org-html-protect-char-alist (orgTZA-html-add-to-list org-html-protect-char-alist '("\n" . "&#10;") t)))
      (when (and (null file)
		 latex-src
		 texfrag-mode)
        (when-let ((input-buffer (plist-get info :input-buffer))
                   (input-preview-dir (texfrag-preview-dir input-buffer)))
          (when (file-directory-p input-preview-dir)
            (setq data (file-relative-name
                        (expand-file-name (file-name-nondirectory source) input-preview-dir)))
            (copy-file source data t))))
      (org-html-close-tag
       "img"
       (org-html--make-attribute-string
        (org-combine-plists
         (list :src data
               :alt alt
               :style (when (numberp image-vertical-align)
                        (format "vertical-align:%gpt" image-vertical-align)))
         attributes))
       info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; texfrag support

(defun orgTZA-html-propertize-latex (oldfun latex-fragment &rest args)
  "Propertize arg LATEX-FRAGMENT of `org-html-latex-fragment' with the org-element."
  (let* ((plist (nth 1 latex-fragment))
	 (pvalue (cdr (member :value plist))))
    (setcar pvalue (propertize (car pvalue) 'org-element latex-fragment)))
  (apply oldfun latex-fragment args))

(defun orgTZA-html-preview-overlay (pos &optional end)
  "Get preview overlay at POS or in the range POS END if END is non-nil.
Return nil if there is no preview overlay at POS."
  (cl-find-if (lambda (ol) (and (overlay-get ol 'preview-image) ol))
	      (or (and (number-or-marker-p end)
		       (overlays-in pos end))
		  (overlays-at pos))))

(declare-function file-to-base64-string "filesTZA")

(defun orgTZA-html-texfrag-with-depth-as-ascent (&optional b e)
  "Run `texfrag-region-synchronously' on region from B to E.
Thereby, replace `preview-ascent-from-bb' by the calculation of the depth."
  (unless b
    (setq b (point-min)))
  (unless e
    (setq e (point-max)))
  (cl-letf (((symbol-function 'preview-ascent-from-bb)
             (lambda (bb)
               ;; BB: [left bottom right top]
               ;; baseline is at 1in from the top of letter paper (11in), so it is
               ;; at 10in from the bottom precisely, which is 720 in PostScript
               ;; coordinates.
               (let ((bottom (aref bb 1)))
                 (- bottom 720.0)))))
      (texfrag-region-synchronously b e)))


(defun orgTZA-html-format-texfrag (latex-frag)
  "Format LATEX-FRAG with `texfrag'."
  (or
   (when-let ((el (get-text-property 0 'org-element latex-frag))
	      (b (org-element-property :begin el))
	      (e (org-element-property :end el))
	      (ol (and
		   (>= b (point-min))
		   (<= e (point-max))
		   ;; \begin{align}...\end{align} may have leading whitespace.
		   ;; Therefore, (orgTZA-html-preview-overlay b) does not work.
		   (or (orgTZA-html-preview-overlay b e)
		      (progn
			(texfrag-mode)
			(orgTZA-html-texfrag-with-depth-as-ascent)
			(orgTZA-html-preview-overlay b e)))))
	      (preview-image (progn
			       (cl-assert ol nil "Generation of texfrag preview overlays failed.")
			       (overlay-get ol 'preview-image)))
	      (file (orgTZA-html-preview-image-get preview-image :file)))
     (cl-assert (file-readable-p file) nil
                "TeXfrag image %S cannot be read." file)
     ;; The image file is embedded as base64 encoded string
     ;; by `orgTZA-html--format-image'.
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; That only works with
     ;; #+OPTIONS: tex:dvipng
     ;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ;; Note, the following file:FILE is org-protocol syntax, not HTML syntax!
     (propertize (concat "file:" file) 'preview-image preview-image 'org-latex-src latex-frag))
   "TeXfrag failed"))

(defun orgTZA-html-format-latex (oldfun latex-frag processing-type info)
  "TeXfrag for OLDFUN with args LATEX-FRAG, PROCESSING-TYPE, and INFO.
Thereby, OLDFUN is `org-html-format-latex'.
Use texfrag images if `texfrag-mode' is non-nil in the original org buffer
and `org-html-format-latex' otherwise."
  (require 'texfrag)
  (let ((input-buffer (plist-get info :input-buffer)))
    (if (with-current-buffer input-buffer texfrag-mode)
        (orgTZA-html-format-texfrag latex-frag)
      (funcall oldfun latex-frag processing-type info))))

(defun orgTZA-html-copy-texfrag-vars (new-buffer)
  "Copy local texfrag vars from current buffer to NEW-BUFFER."
  (let ((pairs (buffer-local-variables)))
    (with-current-buffer new-buffer
      (dolist (pair pairs)
	(when (and (consp pair)
		   (string-match-p "\\`texfrag-" (symbol-name (car pair))))
	  (make-local-variable (car pair))
	  (set (car pair) (cdr pair))
	  ))))
  new-buffer)

(advice-add 'org-export-copy-buffer :filter-return #'orgTZA-html-copy-texfrag-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; advices
(advice-add 'org-html-latex-fragment :around #'orgTZA-html-propertize-latex)
(advice-add 'org-html-latex-environment :around #'orgTZA-html-propertize-latex)

(advice-add 'org-html--format-image :override #'orgTZA-html--format-image)
(advice-add 'org-html-format-latex :around #'orgTZA-html-format-latex)

(defun orgTZA-html-org-hook-fun ()
  "Add stuff for orgTZA-html export in `org-export-options-alist'."
  (setq org-export-options-alist
      (orgTZA-html-add-to-list
       org-export-options-alist
       '(:embedded-image-size "EMBEDDED_IMAGE_SIZE" "embedded-image-size" orgTZA-html-image-base64-max-size)
       t
       (lambda (o1 o2) (eq (car o1) (car o2))))))

(eval-after-load 'ox #'orgTZA-html-org-hook-fun)

(provide 'oxTZA-html)
;;; oxTZA-html.el ends here
