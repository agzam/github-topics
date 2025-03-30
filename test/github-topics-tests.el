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
  (dolist (test-case '(
                       ;; (seconds-ago expected-result)
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

;; (describe "if no query string"
;;   (it "prompts for one"
;;     (spy-on 'word-at-point :and-return-value "tayster-shmeister")
;;     ;; (spy-on 'read-string :to-have-been-called-with "Search GitHub for: "  "tayster-shmeister")
;;     (before-each
;;      (setf (symbol-function 'read-string)
;;            (lambda (prompt &optional initial-input history default-value inherit-input-method)
;;              (message "TIS RUNNING")
;;              ;; (user-error "boom")
;;              )))

;;     (expect (github-topics-find-prs) :to-equal nil)))
