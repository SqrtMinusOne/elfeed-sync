;;; elfeed-sync.el --- TODO -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (elfeed "3.4.1") (request "0.3.2"))
;; Homepage: https://github.com/SqrtMinusOne/avy-dired.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; TODO

;;; Code:
(require 'elfeed)
(require 'request)

(defgroup elfeed-sync ()
  "Sync elfeed with tt-rss."
  :group 'elfeed)

(defcustom elfeed-sync-tt-rss-instance "http://localhost:8280/tt-rss"
  "URL of the tt-rss instance."
  :group 'elfeed-sync
  :type 'string)

(defcustom elfeed-sync-tt-rss-login "admin"
  "Login of the tt-rss instance."
  :group 'elfeed-sync
  :type 'string)

(defcustom elfeed-sync-tt-rss-password "password"
  "Password of the tt-rss instance."
  :group 'elfeed-sync
  :type 'string)

(defvar elfeed-sync--tt-rss-sid nil
  "Session ID.")

(defun elfeed-sync--with-session (callback)
  "Run CALLBACK with the active tt-rss session.

The `elfeed-sync--tt-rss-sid' variable stores the active session ID.
If it's non-nil, CALLBACK is called outright, otherwise CALLBACK is
called only after a succesful login query."
  (if elfeed-sync--tt-rss-sid
      (funcall callback)
    (request (concat elfeed-sync-tt-rss-instance "/api/")
      :type "POST"
      :data (json-encode
             `(("op" . "login")
               ("user" . ,elfeed-sync-tt-rss-login)
               ("password" . ,elfeed-sync-tt-rss-password)))
      :headers '(("Content-Type" . "application/json"))
      :parser 'json-read
      :success (elfeed-sync--handler
                (setq elfeed-sync--tt-rss-sid
                      (alist-get 'session_id
                                 (alist-get 'content data)))
                (funcall callback))
      :error
      (cl-function (lambda (&key error-thrown &allow-other-keys)
                     (message "Error: %S" error-thrown))))))

(defmacro elfeed-sync--session (&rest body)
  "A wrapper around `elfeed-sync--with-session'."
  `(elfeed-sync--with-session
    (lambda ()
      ,@body)))

(defun elfeed-sync--get-tree-summary (&optional data)
  "Get list of feeds and categories from elfeed-summary."
  (unless data
    (setq data (elfeed-summary--get-data)))
  (cl-loop for datum in data
           for type = (car datum)
           if (eq type 'group)
           collect `(("group" . (("name" .
                                  ,(alist-get :title (alist-get 'params (cdr datum))))
                                 ("children" .
                                  ,(when-let ((children
                                               (alist-get 'children (cdr datum))))
                                     (elfeed-sync--get-tree-summary children))))))
           if (eq type 'feed)
           collect `(("feed" . (("tags" . ,(alist-get 'tags (cdr datum)))
                                ("url" . ,(elfeed-feed-url
                                           (alist-get 'feed (cdr datum))))
                                ("title" . ,(or (plist-get
                                                 (elfeed-feed-meta
                                                  (alist-get 'feed (cdr datum)))
                                                 :title)
                                                (elfeed-feed-title
                                                 (alist-get 'feed (cdr datum))))))))))

(defun elfeed-sync--get-tree-plain ()
  "Get list of feeds and categories from plain elfeed."
  (cl-loop for datum in elfeed-feeds
           collect `(("feed" . (("tags" . ,(cdr datum))
                                ("url" . ,(car datum)))))))

(defun elfeed-sync--get-tree ()
  "Get list of feeds and categories."
  (if (fboundp #'elfeed-summary--get-data)
      (elfeed-sync--get-tree-summary)
    (elfeed-sync--get-tree-plain)))

(defmacro elfeed-sync--handler (&rest body)
  "Default response handler for tt-rss."
  `(cl-function
    (lambda (&key data &allow-other-keys)
      (if (= (alist-get 'status data) 0)
          (progn
            ,@body)
        (message "Error!: %S" (alist-get 'content data))
        (when (string-equal
               "NOT_LOGGED_IN"
               (alist-get 'error (alist-get 'content data)))
          (message "Login error. Try again")
          (setq elfeed-sync--tt-rss-sid nil))))))

(defun elfeed-sync-feeds ()
  "Sync feeds with tt-rss."
  (interactive)
  (elfeed-sync--session
   (request (concat elfeed-sync-tt-rss-instance "/api/")
     :type "POST"
     :data (json-encode
            `(("op" . "setFeedsTree")
              ("sid" . ,elfeed-sync--tt-rss-sid)
              ("tree" . ,(elfeed-sync--get-tree))))
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read
     :success (elfeed-sync--handler
               (message "Success!"))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
                    (message "Error: %S" error-thrown))))))

(provide 'elfeed-sync)
;;; elfeed-sync.el ends here
