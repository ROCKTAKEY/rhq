;;; rhq.el --- Client for rhq                        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools, extensions

;; Version: 0.2.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/ROCKTAKEY/rhq
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Client for rhq.

;;; Code:

(require 'shell)

(defgroup rhq nil
  "Client for rhq command."
  :prefix "rhq-"
  :group 'tools)

;; Copied from shell.el in emacs-28.0.91
(defun rhq--split-string-shell-command (string)
  "Split STRING (a shell command) into a list of strings.
General shell syntax, like single and double quoting, as well as
backslash quoting, is respected."
  (with-temp-buffer
    (insert string)
    (let ((comint-file-name-quote-list shell-file-name-quote-list))
      (car (shell--parse-pcomplete-arguments)))))

(defcustom rhq-executable "rhq"
  "Location of rhq executable."
  :group 'rhq
  :type '(choice
          (const "rhq")
          (file :must-match t)))

(defcustom rhq-async-buffer "*rhq*"
  "Output buffer name for `rhq-call-command'."
  :group 'rhq
  :type 'string)

(defconst rhq--subcommands
  '("add"
    "clone"
    "completion"
    "help"
    "import"
    "list"
    "new"
    "refresh")
  "Subcommands available on rhq.")

(defun rhq--make-shell-command-string (&rest args)
  "Join ARGS and make shell command string."
  (mapconcat
   #'shell-quote-argument
   args
   " "))

(defun rhq--read-project ()
  "Read project from user input."
  (completing-read
   "Project: "
   (rhq-get-project-list)))

(defun rhq--check-executable-availability ()
  "Confirm `rhq-executable' exist as executable."
  (unless (executable-find rhq-executable)
    (error "\"rhq\" is not available. Please run `rhq-install-executable' and install it")))

;;;###autoload
(defun rhq-install-executable (&optional noconfirm)
  "Install rhq.
If NOCONFIRM is non-nil, you are not asked confirmation."
  (interactive "P")
  (when (or noconfirm
            (y-or-n-p "\"Cargo\" is prerequisited. Install rhq? "))
    (async-shell-command "cargo install rhq")))

;;;###autoload
(defun rhq-call-command (subcommand &rest args)
  "Call `rhq-executable' with SUBCOMMAND and ARGS, asynchronously."
  (interactive
   (cons
    (completing-read
     "Subcommand: "
     rhq--subcommands)
    (rhq--split-string-shell-command
     (read-from-minibuffer "Arguments: "))))
  (rhq--check-executable-availability)
  (let ((async-shell-command-display-buffer nil))
   (async-shell-command
   (apply #'rhq--make-shell-command-string
          rhq-executable
          subcommand
          args)
   rhq-async-buffer)))

;;;###autoload
(defun rhq-call-command-to-string (subcommand &rest args)
  "Call `rhq-executable' with SUBCOMMAND and ARGS, and get output as string."
  (rhq--check-executable-availability)
  (shell-command-to-string
   (apply #'rhq--make-shell-command-string
          rhq-executable
          subcommand
          args)))

;;;###autoload
(defun rhq-get-project-list ()
  "Get list of projects managed by rhq."
  (split-string (rhq-call-command-to-string "list") "\n" t))

;;;###autoload
(defun rhq-open-project (dirname)
  "Find project directory named DIRNAME from project list by \"rhq list\"."
  (interactive
   (list (rhq--read-project)))
  (find-file dirname))

;;;###autoload
(defun rhq-find-file (filename)
  "Read project and find file named FILENAME in it."
  (interactive
   (let ((project (rhq--read-project)))
     (list (read-file-name "Find file: "
                           project project))))
  (find-file filename))

;;;###autoload
(defun rhq-clone (url)
  "Clone repository from URL by rhq."
  (interactive "sProject URL: ")
  (rhq-call-command "clone" url))

;;;###autoload
(defun rhq-refresh ()
  "Rhq executable refreshes project list."
  (interactive)
  (rhq-call-command "refresh"))

;;;###autoload
(defun rhq-import (dirname)
  "Import DIRNAME as root of rhq-managed projects.
Directories in DIRNAME are regarded as one of project."
  (interactive "DImport root of projects: ")
  (rhq-call-command "import" dirname))

;;;###autoload
(defun rhq-add (dirname)
  "Add DIRNAME as rhq-managed project."
  (interactive "DImport project: ")
  (rhq-call-command "add" dirname))

(defconst rhq--vcs-list
  '("git"
    "hg"
    "darcs"
    "pijul")
  "Possible values as --vcs argument on \"rhq new\".")

;;;###autoload
(defun rhq-new (name &optional root vcs)
  "Create new repository named NAME.
NAME can be \"github.com/username/repo\", \"username/repo\" and so on.
If ROOT is non-nil, it should be path to destination of new repository.
If VCS is non-nil, it should be version control system name:
  git(default), hg, darcs, pijul

With prefix argument, you can explicitly pass ROOT and VCS from minibuffer."
  (interactive
   `(,(read-string "New repository name (like \"username/repo\"): ")
     ,@(when prefix-arg
         (list
          (read-directory-name "Root directory name (where the repository is placed): " default-directory)
          (completing-read "Version control system: "
                           rhq--vcs-list)))))
  (apply
   #'rhq-call-command
   "new"
   name
   `(,@(when root (list "--root" root))
     ,@(when vcs (list "--vcs" vcs)))))

(provide 'rhq)
;;; rhq.el ends here
