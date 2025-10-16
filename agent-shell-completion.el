;;; agent-shell-completion.el --- Completion support for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Provides completion-at-point support for file mentions (@filename syntax)
;; in agent-shell buffers.
;;
;; Features:
;; - Auto-completion after @ symbol
;; - Project-wide file completion using project.el
;; - Fuzzy matching support (flex)
;; - Relative and absolute path completion
;; - Directory completion with annotations

;;; Code:

(require 'project)

(declare-function agent-shell-cwd "agent-shell")

(defun agent-shell-completion--make-file-completion-table (candidates)
  "Create completion table for CANDIDATES with file metadata.
Returns a completion table function suitable for fuzzy matching."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . file))
      (complete-with-action action candidates string pred))))

(defun agent-shell-completion--extract-all-directory-components (file-path)
  "Extract all directory components from FILE-PATH.
For example, `a/b/c/file.txt' returns `(\"a\" \"a/b\" \"a/b/c\")'."
  (let ((dir (file-name-directory file-path))
        (components nil))
    (when dir
      (setq dir (directory-file-name dir))
      (while (not (string-empty-p dir))
        (push dir components)
        (let ((parent (file-name-directory dir)))
          (if parent
              (setq dir (directory-file-name parent))
            (setq dir "")))))
    components))

(defun agent-shell-completion--project-completion (start end proj-root candidates)
  "Build completion for project CANDIDATES.
START/END are bounds, PROJ-ROOT is project root, CANDIDATES are project files."
  (let* ((file-list (mapcar (lambda (f) (file-relative-name f proj-root)) candidates))
         (all-dirs (delete-dups (apply #'append (mapcar #'agent-shell-completion--extract-all-directory-components file-list))))
         (all-candidates (append all-dirs file-list)))
    (list start end
          (agent-shell-completion--make-file-completion-table all-candidates)
          :annotation-function
          (lambda (cand)
            (when (file-directory-p (expand-file-name cand proj-root))
              " <dir>")))))

(defun agent-shell-completion--complete-bare-at (start end cwd)
  "Complete @ with no path - project files or CWD.
START/END are bounds, CWD is working directory."
  (if-let ((proj (project-current nil))
           (candidates (condition-case nil (project-files proj) (error nil))))
      (agent-shell-completion--project-completion start end (project-root proj) candidates)
    (agent-shell-completion--directory-completion start end cwd "" #'identity cwd)))

(defun agent-shell-completion--complete-absolute-path (start end prefix)
  "Complete absolute path PREFIX.
START/END are bounds."
  (let ((dir (or (file-name-directory prefix) "/")))
    (agent-shell-completion--directory-completion
     start end dir (or (file-name-nondirectory prefix) "")
     (lambda (file) (concat dir file))
     dir)))

(defun agent-shell-completion--complete-relative-path (start end prefix cwd)
  "Complete relative path PREFIX from CWD.
START/END are bounds."
  (let ((dir (expand-file-name (file-name-directory prefix) cwd)))
    (agent-shell-completion--directory-completion
     start end dir (or (file-name-nondirectory prefix) "")
     (lambda (file) (concat (file-name-directory prefix) file))
     cwd)))

(defun agent-shell-completion--directory-completion (start end dir file-prefix prefix-transform cwd)
  "Helper for directory-based completion.
START and END are completion bounds, DIR is directory to search,
FILE-PREFIX is the partial filename typed (used to determine if dotfiles shown),
PREFIX-TRANSFORM is function to transform results,
CWD is current working directory for annotation."
  (let* ((show-dotfiles (string-prefix-p "." file-prefix))
         (match-regex (if show-dotfiles "^\\." "^[^.]"))
         (candidates (when (file-directory-p dir)
                       (condition-case nil
                           (directory-files dir nil match-regex)
                         (error nil)))))
    (when candidates
      (list start end
            (agent-shell-completion--make-file-completion-table
             (mapcar prefix-transform candidates))
            :annotation-function
            (lambda (cand)
              (if (file-directory-p (expand-file-name cand cwd))
                  " <dir>"
                ""))))))

(defun agent-shell-completion--file-mention-completion-at-point ()
  "Provide file path completion after @ symbol.

Completion behavior:
  @filename       -> Project files or current directory
  @path/file      -> Files in relative path subdirectory
  @/abs/path      -> Absolute path completion
  @\"file space\"  -> Quoted filename completion"
  (save-excursion
    (let ((end (point)))
      (when (re-search-backward "@\\(?:\"\\([^\"]*\\)\\|\\([^[:space:]\"]*\\)\\)" (line-beginning-position) t)
        (let* ((quoted-path (match-string 1))
               (unquoted-path (match-string 2))
               (is-quoted (not (null quoted-path)))
               (prefix (or quoted-path unquoted-path ""))
               (start (match-beginning (if is-quoted 1 2))))
          (when (and (>= end start) (<= end (match-end (if is-quoted 1 2))))
            (let ((cwd (agent-shell-cwd)))
              (cond
               ((string-prefix-p "/" prefix) (agent-shell-completion--complete-absolute-path start end prefix))
               ((file-name-directory prefix) (agent-shell-completion--complete-relative-path start end prefix cwd))
               (t (agent-shell-completion--complete-bare-at start end cwd))))))))))

(defun agent-shell-completion--maybe-trigger-file-completion ()
  "Trigger native completion if @ was just typed."
  (when (eq (char-before) ?@)
    (completion-at-point)))

(defun agent-shell-completion-setup ()
  "Setup `completion-at-point' for file mentions.

Fuzzy matching works with built-in completion styles like `flex' (Emacs 27+)."
  (add-hook 'completion-at-point-functions
            #'agent-shell-completion--file-mention-completion-at-point nil t)
  (add-hook 'post-self-insert-hook
            #'agent-shell-completion--maybe-trigger-file-completion nil t))

(provide 'agent-shell-completion)

;;; agent-shell-completion.el ends here
