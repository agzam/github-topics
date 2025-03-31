;;; github-topics-tests.el --- Lookup PRs matching a query - tests -*- lexical-binding: t; -*-
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

(require 'buttercup)
(require 'github-topics)

(describe "github-topics--time-ago"
  (dolist (test-case '(;; (seconds-ago expected-result)
                       ((* 4 24 60 60) "4 days ago")
                       ((* 1 60 60)    "1 hours ago")
                       ((* 5 60)       "5 minutes ago")
                       (60             "1 minutes ago")
                       (15             "just now")))
    (let ((seconds-ago (eval (car test-case)))
          (expected (cadr test-case)))
      (it (format "shows '%s' when %d seconds ago" expected seconds-ago)
        (let* ((time-in-past (- (float-time) seconds-ago))
               (time-str (format-time-string "%Y-%m-%dT%H:%M:%S%z" (seconds-to-time time-in-past))))
          (expect (github-topics--time-ago time-str)
                  :to-equal expected))))))

(describe "if no query string"
  :var (read-string)
  (it "prompts for one"
    (spy-on 'word-at-point :and-return-value "tayster-shmeister")
    (spy-on 'read-string :and-return-value "tayster-shmeister")
    (spy-on 'shell-command-to-string :and-return-value "[]")
    (github-topics-find-prs)
    (expect 'read-string :to-have-been-called-with "Search GitHub for: " "tayster-shmeister")))

(describe "when no gh found"
  (it "throws an error"
    (spy-on 'read-string :and-return-value "foo")
    (spy-on 'executable-find :and-return-value nil)
    (expect (github-topics-find-prs) :to-throw 'error)))

(describe "when called with an argument"
  (it "browse-url with correct url-string"
    (spy-on 'browse-url)
    (let ((current-prefix-arg '(4))
          (github-topics-default-orgs '("my-org")))
      (funcall-interactively 'github-topics-find-prs "foo"))
    (expect 'browse-url :to-have-been-called-with
            (format "https://github.com/search?q=%s&type=pullrequests"
                    (url-hexify-string "org: my-org foo")))))

(describe "when called orgs=none"
  (it "should ignore github-topics-default-orgs and search in all of GitHub"
    (spy-on 'url-hexify-string)
    (spy-on 'browse-url)
    (spy-on 'executable-find :and-return-value "gh")
    (let ((current-prefix-arg '(4))
          (github-topics-default-orgs '("my-org")))
      (funcall-interactively 'github-topics-find-prs "foo" 'none))
    (expect 'url-hexify-string :to-have-been-called-with "foo")))
