;;; find-file-in-git-repo.el --- Utility to find files in a git repo

;; Fork of:
;; Copyright 2011 atom smith
;; Author: atom smith
;; URL: http://github.com/re5et/find-file-in-git-repo
;; Version: 0.1.2

;;; Commentary:

;; Using default-directory searches upward for a .git repo directory,
;; then, feeds files into ido-completing-read using git ls-files.

(defgroup find-file-in-git-repo nil
  "Using default-directory searches upward for a .git repo directory,
then, feeds files into ido-completing-read using git ls-files.")

(defcustom goto-alternate-git-file-patterns
  (quote (("\\.spec\\.js$" . ".js")
          ("\\.h$" . ".c")
          ("Test\\.java$" . ".java")))
  "Alist of patterns and their replacement to get from an alternate file to the base file.

Example:

'((\"\\\\.spec\\\\.js$\" \".js\")
  (\"\\\\.h\" \".c$\")
  (\"\\\\Test.java$\" \".java\"))"
  :group 'find-file-in-git-repo
  :type '(repeat (cons (regexp :tag "Regexp matching to replace to get to alternate file name")
		       (string :tag "Replacement"))))

(defun files-in-git-repo (repo &optional filter regexp-filter)
  (let* ((relative-buffer-file-name (replace-in-string (buffer-file-name) repo ""))
         (grepString (if filter (concat " | grep -e '" filter "'") ""))
         (files (shell-command-to-string (format "cd %s && git ls-files --cached --others --exclude-standard%s | grep -ve '%s'" repo grepString relative-buffer-file-name))))
         (remove-if (lambda (x) (string= "" x))
                    (split-string files "\n"))))

(defun find-file-in-git-repo (&optional initial-input regexp-filter)
  (interactive)
  (let* ((repo (find-git-repo default-directory))
         (files (files-in-git-repo repo regexp-filter))
         (files-count (length files)))

    (when (= 1 files-count)
        (find-file (concat repo (car files))))

    (when (< 1 files-count)
      (find-file
       (concat repo
               (ido-completing-read
                "find in git repo: "
                files
                nil nil initial-input))))))

(defun find-git-repo (dir)
  (if (string= "/" dir)
      (message "not in a git repo.")
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))

(defun goto-alternate-git-file ()
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name)))
        (patterns goto-alternate-git-file-patterns)
        (found nil))


    (while (and patterns (not found))
      (let* ((regexp (caar patterns))
             (replacement (cdar patterns)))
        (when (string-match regexp file-name)
          (setq found (replace-match replacement nil nil file-name))))
      (setq patterns (cdr patterns)))

    (when (not found)
      (setq found (file-name-sans-extension file-name)))

    (let ((regexp-filter (concat "\\(^\\|/\\)[^/]*" (regexp-quote found) "[^/]*$")))
      (find-file-in-git-repo "" regexp-filter))))

;;; find-file-in-git-repo.el ends here

(provide 'find-file-in-git-repo)
