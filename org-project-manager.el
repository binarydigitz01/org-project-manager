;;; org-project-manager.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Arnav Vijaywargiya
;;
;; Author: Arnav Vijaywargiya <binarydigitz01@protonmail.com>
;; Maintainer: Arnav Vijaywargiya <binarydigitz01@protonmail.com>
;; Created: March 06, 2022
;; Modified: March 06, 2022
;; Version: 1.0
;; Homepage: https://github.com/Ice-Cube69/org-project-manager
;; Package-Requires: ((emacs "28.1") (org-roam "2.2.2"))
;;

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

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This module provides `org-project-manager-open-node'.
;;`org-project-manager-delete' and `org-project-manager-capture-current', for use.

;;; Code:

(require 'org-roam)

(defvar org-project-manager-known-project-org-nodes '() "List of Projects with known-nodes.")
(defvar org-project-manager-save-path (concat user-emacs-directory "org-project-manager") "Path of File which contains save data.")
(defgroup org-project-manager nil "The group for org-project-manager."
  :group 'emacs)
(defcustom org-project-manager-default-project-library 'projectile "Default project app to use.  Possible values are 'project, 'projectile."
  :type 'symbol
  :options (list 'project 'projectile)
  :group 'org-project-manager)
;; (defvar org-project-manager-default-project-library 'projectile "Default project app to use. Possible values are 'project, 'projectile")
(require org-project-manager-default-project-library)

(when (null (file-exists-p org-project-manager-save-path))
  (make-directory org-project-manager-save-path))

(when (null (file-exists-p (concat org-project-manager-save-path "/data.txt")))
  (progn
    (with-temp-buffer
      (print '() (current-buffer))
      (write-file (concat org-project-manager-save-path "/data.txt")))))

(defun org-project-manager-get-project-name ()
  "Get the project name using either projectile or project, depending on \'org-project-manager-default-project-library\'."
  (if (equal org-project-manager-default-project-library 'project)
      (project-root (project-current))
    (projectile-project-name)))

(defun org-project-manager-get-file-path (node-id) "Get File path from NODE-ID."
       (car
        (car
         (org-roam-db-query [:SELECT file :FROM nodes :WHERE (= id $s1)] node-id))))

(defun org-project-manager-ask-roam-node ()
  "Ask for Org roam node from user, create 1 if it does not exist."
  (let ((node (org-roam-node-read)))
    (if (org-roam-node-file node)
        (node)
      (progn
        (org-roam-capture-
         :node node
         :templates org-roam-capture-templates)
        node))))

;;;###autoload
(defun org-project-manager-open-node ()
  "Opens the node associated with current project."
  (interactive)
  (let ((node-id)
        (project-name)
        (node)
        (node-path))
    (setq project-name (org-project-manager-get-project-name))
    (setq node-id (car (cdr (assoc project-name org-project-manager-known-project-org-nodes))))
    (setq node-path (org-project-manager-get-file-path node-id))
    (if node-id
        (find-file node-path)
      (progn
        (setq node (org-project-manager-ask-roam-node))
        (setq node-id (org-roam-node-id node))
        (setq node-path (org-roam-node-file node))
        (add-to-list 'org-project-manager-known-project-org-nodes (list project-name node-id))
        (find-file node-path)))))

(defun org-project-manager-write-to-file ()
  "Save \'org-project-manager-default-project-library\' to file."
  (with-temp-buffer
    (prin1 org-project-manager-known-project-org-nodes (current-buffer))
    (write-file (concat org-project-manager-save-path "/data.txt"))))

(defun org-project-manager-read-from-file ()
  "Save \'org-project-manager-default-project-library\' to file."
  (with-temp-buffer
    (insert-file-contents (concat org-project-manager-save-path "/data.txt"))
    (setq org-project-manager-known-project-org-nodes (read (current-buffer)))))

(org-project-manager-read-from-file)

(add-hook 'kill-emacs-hook #'org-project-manager-write-to-file)

;;;###autoload
(defun org-project-manager-delete ()
  "Deletes all cache."
  (interactive)
  (setq org-project-manager-known-project-org-nodes '()))

(defun org-project-manager-get-node (project)
  "Return the org roam file corresponding to the project-name."
  (let (node-id)
    (dolist (element org-project-manager-known-project-org-nodes node-id)
      (if (string= project (car element))
          (setq node-id (nth 1 element))))
    (org-roam-node-from-id node-id)))
;;;###autoload
(defun org-project-manager-capture-current ()
  "Capture information to org roam node of current project."
  (interactive)
  (org-roam-capture-
   :node (org-project-manager-get-node (org-project-manager-get-project-name))))

(defun org-project-manager-open-agenda ()
  "Open org agenda with project todos for the current project."
  (interactive)
  (let ((org-agenda-files (list (org-project-manager-get-file-path (org-roam-node-id (org-project-manager-get-node (org-project-manager-get-project-name)))))))
    (print org-agenda-files)
    (org-agenda)))

(provide 'org-project-manager)
;;; org-project-manager.el ends here
