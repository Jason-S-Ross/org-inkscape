;;; org-inkscape.el --- Provides inkscape handling for org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jason Ross
;;
;; Author: Jason Ross <https://github.com/jason>
;; Maintainer: Jason Ross <jasonross1024@gmail.com>
;; Created: January 14, 2022
;; Modified: January 14, 2022
;; Version: 0.0.1
;; Homepage: https://github.com/Jason-S-Ross/org-inkscape
;; Package-Requires: ((emacs "24.4") (org "9.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;;  Description
;; Adds inkscape support to Org Mode
;;
;;; Code:

(require 'org)
(require 'f)
(require 'uuidgen)
(require 'org-element)

;;; Customization

(defgroup org-inkscape nil
  "Org-inkscape customization."
  :group 'org
  :package-version '(org-inkscape . "0.1.0"))

(defcustom org-inkscape-ask-for-file-name t
  "If org-inkscape should ask for a file name when inserting a file."
  :group 'org-inkscape
  :type 'boolean
  :package-version '(org-inkscape . "0.1.0"))

(defcustom org-inkscape-filepath-absolute-p t
  "If org-inkscape should insert absolute file paths or relative file paths."
  :group 'org-inkscape
  :type 'boolean
  :package-version '(org-inkscape . "0.1.0"))

(defcustom org-inkscape-image-directory ".org-inkscape/"
  "Default directory in which to generate images."
  :group 'org-inkscape
  :type 'string
  :package-version '(org-inkscape . "0.1.0"))

(defcustom org-inkscape-template-path "~/.config/inkscape/templates/org-inkscape.svg"
  "Default template for new images."
  :group 'org-inkscape
  :type 'string
  :package-version '(org-inkscape . "0.1.0"))

(defcustom org-inkscape-create-image-command
"inkscape --actions=\"file-new:%t; export-area-page; export-filename:%p; export-do;\""
  "Command to create new inkscape images.

Format string with the following keys:
%p : File path for the file
%t : File path for the template"
  :group 'org-inkscape
  :type 'string
  :package-version '(org-inkscape . "0.1.0"))

(defcustom org-inkscape-open-image-command "inkscape \"%s\" "
  "Command to open existing inkscape images. Format string with PATH as a key."
  :group 'org-inkscape
  :type 'string
  :package-version '(org-inkscape . "0.1.0"))

(defcustom org-inkscape-generate-file-function #'org-inkscape--generate-filename
  "Function to generate a filename.
Takes 1 argument, the path of the current buffer."
  :group 'org-inkscape
  :type 'function
  :package-version '(org-inkscape . "0.1.0"))

;;; Functions

(defun org-inkscape--generate-filename (_filename)
  "Generate a unique filename."
  (expand-file-name (concat org-inkscape-image-directory (uuidgen-1) ".svg")))

(defun org-inkscape--make-new-image (path)
  "Create an inkscape image at PATH."
  (unless (file-exists-p (f-dirname path))
    (mkdir (f-dirname path)))
  (let* ((default-directory (expand-file-name (f-dirname path)))
         (fname (f-filename path))
         (cmd (format-spec
               org-inkscape-create-image-command
               `((?t . ,(expand-file-name org-inkscape-template-path))
                 (?p . ,fname)))))
    (message "Command: %s" cmd)
    (with-temp-buffer
      (save-window-excursion
        (let ((async-shell-command-buffer 'new-buffer)
              (async-shell-command-display-buffer t))
          (async-shell-command cmd))))))

(defun org-inkscape--open-image (path)
  "Open an inkscape image at PATH."
  (let* ((default-directory (expand-file-name (f-dirname path)))
         (fname (f-filename path))
         (cmd (format org-inkscape-open-image-command fname)))
    (with-temp-buffer
      (save-window-excursion
        (let ((async-shell-command-buffer 'new-buffer)
              (async-shell-command-display-buffer t))
          (async-shell-command cmd))))))

(defun org-inkscape-open-or-make-image (path)
  "Open an image at PATH if it exists otherwise create it."
  (unless (file-exists-p path)
    (org-inkscape--make-new-image path))
  (org-inkscape--open-image path))

(defun org-inkscape-redraw-images (&rest _args)
  "Use font-lock to redraw the links."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (org-restart-font-lock)))

(defun org-inkscape-overlay (begin end path _bracketp)
  "Display overlays for inkscape images.
BEGIN is the start point, END is the end point, and PATH is the image path."
  (let (img ov)
    (when (and
           path
           (file-exists-p path))
      (setq img
            (create-image
             (expand-file-name path)))
      (setq ov (make-overlay begin end))
      (overlay-put ov 'display img)
      (overlay-put ov 'face 'default)
      (overlay-put ov 'org-image-overlay t)
      (overlay-put
       ov
       'modification-hooks
       (list
        `(lambda (&rest args)
           (org-display-inline-remove-overlay ,ov t ,begin ,end))))
      (push ov org-inline-image-overlays))))

(defun org-inkscape-preprocess (_backend)
  "Preprocessing function to run in `org-export-before-processing-hook'."
  (let ((links (reverse (org-element-map (org-element-parse-buffer) 'link
			  (lambda (link)
			    (when (string= (org-element-property :type link) "inkscape")
			      link))))))
    (cl-loop for link in links
	     do
	     (goto-char (org-element-property :begin link))
	     (re-search-forward "inkscape:" (org-element-property :end link))
	     (replace-match "file:"))))

;;;###autoload
(defun org-inkscape-insert-new-image (path desc)
  "Insert new image with description DESC and path PATH in current buffer.

TEMPLATE is the path of the template to use."
  (interactive
   (let ((path
          (if (xor current-prefix-arg org-inkscape-ask-for-file-name)
              (read-file-name "New inkscape file: ")
            (funcall org-inkscape-generate-file-function (buffer-file-name))))
         (desc (read-string "Description: ")))
     (list path desc)))
   (org-inkscape-open-or-make-image path)
   (org-insert-link
    nil
    (concat
     "inkscape:"
     (if org-inkscape-filepath-absolute-p
         path
       (f-relative path)))
    desc)
   (org-inkscape-redraw-images))

(defun org-inkscape-link-finder ()
  "Return the link at point."
  ;;; Copied from org.el
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (when (eq type 'link)
      (let* ((link context)
             (type (org-element-property :type link)))
        (when (string= type "inkscape")
          (cons
           'org-inkscape-key
           (expand-file-name (org-element-property :path link))))))))

;;; Embark Config

;;;; Declare embark variables in case they are not defined

(defvar embark-target-finders)
(defvar embark-general-map)
(defvar embark-keymap-alist)

;;;; Declare Keymap
(defvar org-inkscape-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "k") #'org-inkscape-open-or-make-image)
    map)
  "Keymap for Embark minibuffer actions.")

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders 'org-inkscape-link-finder)
  (set-keymap-parent org-inkscape-map embark-general-map)
  (add-to-list 'embark-keymap-alist '(org-inkscape-key . org-inkscape-map)))


;;; Hooks and Advice

(org-link-set-parameters
 "inkscape"
 :help-echo "Click to open in inkscape."
 :activate-func 'org-inkscape-overlay
 :follow 'org-inkscape-open-or-make-image)

(advice-add 'org-display-inline-images :after 'org-inkscape-redraw-images)

(add-hook 'org-export-before-parsing-hook 'org-inkscape-preprocess)

(provide 'org-inkscape)
;;; org-inkscape.el ends here
