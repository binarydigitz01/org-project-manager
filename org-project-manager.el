;;; org-project-manager.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Arnav Vijaywargiya
;;
;; Author: Arnav Vijaywargiya <binarydigitz01@protonmail.com>
;; Maintainer: Arnav Vijaywargiya <binarydigitz01@protonmail.com>
;; Created: March 06, 2022
;; Modified: March 06, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/Ice-Cube69/org-project-manager
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org-roam)
(require 'projectile)

(defvar org-project-manager-known-project-org-nodes '() "List of Projects with known-nodes.")
(defvar org-project-manager-save-path (concat user-emacs-directory "org-project-manager") "Path of File which contains save data.")

(when (null (file-exists-p org-project-manager-save-path))
    (make-directory org-project-manager-save-path))

(when (null (file-exists-p (concat org-project-manager-save-path "/data.txt")))
  (progn
    (with-temp-buffer
      (print '() (current-buffer))
      (write-file (concat org-project-manager-save-path "/data.txt")))))

(defun org-project-manager-get-file-path (node-name) "Get File path from NODE-NAME."
       (car
        (car
         (org-roam-db-query [:SELECT file :FROM nodes :WHERE (= title $s1)] node-name))))

;;;###autoload
(defun org-project-manager-open-node ()
  "Opens the node associated with current projectile project."
  (interactive)
  (let ((node-name)
        (project-name)
        (node)
        (node-path))
    (setq project-name (projectile-project-name))
    (setq node-name (car (cdr (assoc project-name org-project-manager-known-project-org-nodes))))
    (setq node-path (org-project-manager-get-file-path node-name))
    (if node-name
        (find-file node-path)
      (progn
        (setq node (org-roam-node-read))
        (setq node-name (org-roam-node-title node))
        (setq node-path (org-roam-node-file node))
        (add-to-list 'org-project-manager-known-project-org-nodes (list project-name node-name))
        (find-file node-path)))))

(defun org-project-manager-write-to-file ()
  "Save org-project-manager-known-project-org-nodes to file."
  (with-temp-buffer
  (prin1 org-project-manager-known-project-org-nodes (current-buffer))
  (write-file (concat org-project-manager-save-path "/data.txt"))))

(defun org-project-manager-read-from-file ()
  "Save org-project-manager-known-project-org-nodes to file."
  (with-temp-buffer
    (insert-file-contents (concat org-project-manager-save-path "/data.txt"))
    (setq org-project-manager-known-project-org-nodes (read (current-buffer)))))

(org-project-manager-read-from-file)

(add-hook 'kill-emacs-hook #'org-project-manager-write-to-file)

(defun org-project-manager-delete ()
  "Deletes all cache"
  (interactive)
  (setq org-project-manager-known-project-org-nodes '()))

(provide 'org-project-manager)
;;; org-project-manager.el ends here
