;;; github-topics.el --- Lookup PRs matching a query -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: March 29, 2025
;; Modified: March 29, 2025
;; Version: 0.0.1
;; Keywords: vc matching tools
;; Homepage: https://github.com/agzam/github-topics
;; Package-Requires: ((emacs "29.4") (ts "0.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;  For finding Pull-Requests on GitHub using 'gh' command-line tool.
;;
;;; Code:

(require 'ts)
(require 'parse-time)
(require 'thingatpt)

(defgroup github-topics nil
  "Lookup PRs matching a query."
  :group 'github-topics)

(defcustom github-topics-default-orgs nil
  "Default GH Orgs to search for PRs."
  :group 'github-topics
  :type '(repeat symbol))

(defcustom github-topics-convert-body-with-pandoc t
  "Convert body of pull-request to `org-mode'.
If nil - keeps the body in markdown - might be slightly faster.
Can be set to the explicit path to pandoc in the system."
  :group 'github-topics
  :type '(choice
          (boolean :tag "Use default pandoc if available")
          (string :tag "Explicit path to pandoc executable")))

(defcustom github-topics-prs-buffer-hook nil
  "Triggers on `github-topics-find-prs' buffer with the list of PRs.
takes a single parameter - the buffer pointer."
  :group 'github-topics
  :type 'hook)

(defun github-topics--time-ago (iso8601-time-str)
  "Convert ISO8601-TIME-STR in the past - into relative time description."
  (let* ((time-obj (parse-iso8601-time-string iso8601-time-str))
         (unix-timestamp (float-time time-obj))
         (diff (- (float-time) unix-timestamp)))
    (cond
     ((<= diff 60) "just now")
     (t (concat (car (split-string (ts-human-format-duration diff) ","))
                " ago")))))

(defun github-topics--body->org (body)
  "Converts the BODY of GitHub Pull-Request to Org-mode format.

The conversion works only if pandoc is detected in the system, otherwise
it wraps it into a source block."
  (with-temp-buffer
    (if-let* ((pandoc (unless (null github-topics-convert-body-with-pandoc)
                        (if (stringp github-topics-convert-body-with-pandoc)
                            github-topics-convert-body-with-pandoc
                          (executable-find "pandoc")))))
        (progn
          (insert body)
          (shell-command-on-region
           (point-min)
           (point-max)
           (concat pandoc " --wrap=none -f markdown -t org")
           nil t)

          ;; remove all property drawers
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*:PROPERTIES:\n\\(?:.*\n\\)*?[ \t]*:END:\n" nil t)
            (replace-match ""))

          ;; increase outline levels to match the main doc
          (let ((level-increase 2))
            (goto-char (point-min))
            (while (re-search-forward "^\\(\\*+\\)\\( \\)" nil t)
              (replace-match
               (concat
                (make-string (+ (length (match-string 1)) level-increase) ?*)
                "\\2")))))
      (progn
        (insert (replace-regexp-in-string "\r" "" body))
        (goto-char (point-max))
        (insert (format "\n#+end_src\n"))
        (goto-char (point-min))
        (insert (format "#+begin_src markdown\n"))))
    (buffer-substring (point-min) (point-max))))

(defun github-topics-find-prs (&optional query-string orgs)
  "Find PRs in ORGS, containing QUERY-STRING.

With an argument - open the query in the browser.

For searching in all of GitHub, ignoring `github-topics-default-orgs',
set ORGS - `'none'."
  (interactive)
  (let* ((query-string (or query-string
                           (read-string "Search GitHub for: "
                                        (word-at-point))))
         (orgs-user-readable (if (eq orgs 'none)
                                 "all"
                               (mapconcat
                                (lambda (x) (format "'%s'" x)) github-topics-default-orgs " and ")))
         (user-msg (format "Searching GitHub for '%s' in %s orgs" query-string orgs-user-readable))
         (orgs-str (if (eq orgs 'none) ""
                     (thread-last
                       (or orgs github-topics-default-orgs)
                       (seq-map (lambda (x) (format "org:%s" x)))
                       (funcall (lambda (s) (if (length< s 1) ""
                                              (concat (string-join s " ") " ")))))))
         (query-params (url-hexify-string (concat orgs-str query-string)))
         (search-page-url (format "https://github.com/search?q=%s&type=pullrequests" query-params)))
    (if current-prefix-arg (browse-url search-page-url)
      (let* ((gh (or (executable-find "gh") (user-error "'gh' cmd-line tool not found")))
             (fields (mapconcat
                      #'symbol-name
                      '(title url repository author number state createdAt body) ","))
             (orgs-str (if (eq orgs 'none) ""
                         (mapconcat (lambda (x) (format "--owner %s" x))
                                    (or nil github-topics-default-orgs) " ")))
             (cmd-args (format "search prs %s \"%s\" --json \"%s\""
                               orgs-str query-string fields))
             (_ (message user-msg)))
        (if-let* ((res (thread-first
                         gh (concat " " cmd-args)
                         shell-command-to-string
                         (json-parse-string :object-type 'alist))))
            (let ((buf (get-buffer-create (format "*%s*" user-msg))))
              (with-current-buffer buf
                (erase-buffer)
                (insert
                 (format "[[%s][%s]]\n\n"
                         search-page-url
                         user-msg))
                (thread-last
                  res
                  (seq-do
                   (lambda (rec)
                     (let-alist rec
                       (insert (format "* %s\n" .repository.nameWithOwner))
                       (insert (format "** [[%s][%s #%s]]\n" .url .title .number))
                       (insert (format "%s by: [[%s][%s]] %s\n\n"
                                       (upcase .state)
                                       .author.url
                                       .author.login
                                       (github-topics--time-ago .createdAt)))
                       (insert (github-topics--body->org .body))))))
                (org-mode)
                (read-only-mode +1)
                (goto-char (point-min)))
              (run-hook-with-args 'github-topics-prs-buffer-hook buf)
              (pop-to-buffer buf))
          (message (concat "No PRs for: " user-msg)))))))

(provide 'github-topics)
;;; github-topics.el ends here
