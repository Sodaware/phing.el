;;; phing.el --- Emacs interface for Phing

;; Copyright (C) 2014 Phil Newton <phil@sodaware.net>

;; Author: Phil Newton <phil@sodaware.net>
;; Keywords: phing, php
;; URL: http://www.philnewton.net/code/phing-el/
;; Created: September 23rd, 2014
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;; Free Software Foundation
;; 51 Franklin Street, Fifth Floor
;; Boston, MA 02110-1301
;; USA

;;; Commentary:

;; phing.el provides way to interact with Phing, a PHP build tool based
;; on Apache Ant.

;;; Code:

;; Dependencies

(require 'xml)

;; Configuration

(defgroup phing nil
  "Emacs interface for Phing"
  :group 'processes
  :prefix "phing-")

(defvar phing-last-target nil)
(defvar phing-build-file-name "build.xml")
(defvar phing-command "phing -logger phing.listener.DefaultLogger")


;; Functions

;;;###autoload
(defun phing-execute-target (&optional target)
  "Run TARGET in the project's root directory."
  (interactive)
  (let ((build-file (phing-find-build-file-for-current-buffer)))
    (setq phing-last-target build-file)
    (message (concat phing-command " // " target))
    (compile (concat phing-command " " target))))

(defun phing-list-buffer-targets ()
  "List targets for current buffer."
  (interactive)
  (message (phing-get-targets (phing-find-build-file-for-current-buffer))))

(defun phing-format-target (target)
  "Make TARGET pretty."
  (let ((target-name (xml-get-attribute target 'name))
        (target-desc (xml-get-attribute target 'description)))
    (format "%s -- %s"
            target-name target-desc)))

(defun phing-get-target-nodes (file)
  "Get target nodes from FILE."
  (xml-get-children (car (xml-parse-file file)) 'target))

(defun phing-find-build-file-for-current-buffer ()
  "Find the build.xml file for the current directory."
  (phing-find-build-file (file-name-directory (buffer-file-name))))

(defun phing-find-build-file (directory)
  "Find a build.xml file in DIRECTORY."
  (phing--find-file-upwards directory "build.xml"))

(defun phing--find-file-upwards (directory file)
  "Search DIRECTORY (or parent directories) for FILE."
  (let* ((updir (file-truename (concat (file-name-directory directory) "../")))
         (curfd (if (not (string= (substring directory (- (length directory) 1)) "/"))
                    (concat directory "/" file)
                  (concat directory file))))
    (if (file-exists-p curfd)
        curfd
      (if (and (not (string= (file-truename directory) updir))
               (< (length updir) (length (file-truename directory))))
          (phing--find-file-upwards updir file)
        nil))))


(provide 'phing)
;;; phing.el ends here
