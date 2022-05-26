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
(require 'seq)
(require 'elfeed-db)
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

(defcustom elfeed-sync-look-back 15552000
  "How far back to sync."
  :group 'elfeed-sync
  :type 'number)

(defcustom elfeed-sync-unread-tag 'unread
  "Unread tag for sync."
  :group 'elfeed-sync
  :type 'symbol)

(defcustom elfeed-sync-marked-tag 'later
  "Marked tag for sync."
  :group 'elfeed-sync
  :type 'symbol)

(defcustom elfeed-sync-missing-attempts 5
  "How many attempts to sync missing entries."
  :group 'elfeed-sync
  :type 'number)

(defcustom elfeed-sync-missing-targets-keep (* 60 60 24 7)
  "How long to keep missing targets."
  :group 'elfeed-sync
  :type 'number)

(defvar elfeed-sync--tt-rss-sid nil
  "Session ID.")

(defvar elfeed-sync--state nil
  "State of the tt-rss sync.")

(cl-defstruct (elfeed-sync-datum (:constructor elfeed-sync-datum--create))
  id tags)

(defun elfeed-sync--state-empty ()
  "Create an empty elfeed-sync state."
  `((:last-sync . nil)
    (:feeds . ,(make-hash-table :test #'equal))
    (:missing . ,(make-hash-table :test #'equal))
    (:discarded . ,(make-hash-table :test #'equal))
    (:missing-target . nil)))

(defun elfeed-sync--state-file ()
  (concat elfeed-db-directory "/sync-state"))

(defun elfeed-sync--state-load ()
  "Load elfeed-sync state from the filesystem."
  (if (not (file-exists-p (elfeed-sync--state-file)))
      (setf elfeed-sync--state (elfeed-sync--state-empty))
    (with-temp-buffer
      (insert-file-contents (elfeed-sync--state-file))
      (goto-char (point-min))
      (condition-case _
          (progn
            (setf elfeed-sync--state (read (current-buffer))))
        (error (progn
                 (message "Recreating the sync state because of read error")
                 (setf elfeed-sync--state (elfeed-sync--state-empty))))))))

(defun elfeed-sync--state-ensure ()
  "Ensure that the sync state has been loaded."
  (when (null elfeed-sync--state) (elfeed-sync--state-load)))

(defun elfeed-sync-reset ()
  "Reset the elfeed sync state"
  (interactive)
  (setf elfeed-sync--state (elfeed-sync--state-empty)))

(defun elfeed-sync-state-save ()
  "Save the elfeed sync state to the filesystem."
  (interactive)
  (elfeed-sync--state-ensure)
  (mkdir (file-name-directory (elfeed-sync--state-file)) t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file (elfeed-sync--state-file)
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle nil))
        (princ ";;; Elfeed Sync State\n\n")
        (prin1 elfeed-sync--state)))))

(defun elfeed-sync-state-save-safe ()
  "Save the elfeed sync state, ignoring errors.

This can be put to `kill-emacs-hook' and not screw up anything
with exceptions."
  (ignore-errors
    (org-journal-tags-db-save)))

(defun elfeed-sync--json-read-safe ()
  (condition-case _
      (json-read)
    (with-current-buffer (get-buffer-create "*elfeed-sync-json-read-error*")
      (erase-buffer)
      (insert-buffer-substring (current-buffer))
      (error "Error reading JSON: %s" (buffer-string)))))

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
      :parser 'elfeed-sync--json-read-safe
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
     :parser 'elfeed-sync--json-read-safe
     :success (elfeed-sync--handler
               (message "Success!"))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
                    (message "Error: %S" error-thrown))))))

(defun elfeed-sync--get-bad-feeds ()
  (let ((feed-hash (make-hash-table :test #'equal))
        (bad-feeds-hash (make-hash-table :test #'equal)))
    (with-elfeed-db-visit (entry feed)
      (let ((url-hash (gethash (elfeed-feed-id feed) feed-hash)))
        (unless url-hash
          (setq url-hash (make-hash-table :test #'equal))
          (puthash (elfeed-feed-id feed) url-hash feed-hash))
        (if (gethash (elfeed-entry-link entry) url-hash)
            (puthash (elfeed-feed-id feed) t bad-feeds-hash)
          (puthash (elfeed-entry-link entry) t url-hash)))
      (when (> (- (time-convert nil 'integer)
                  elfeed-sync-look-back)
               (elfeed-entry-date entry))
        (elfeed-db-return)))
    bad-feeds-hash))


(defun elfeed-sync--datum-chaged (datum entry)
  (let* ((old-tags (elfeed-sync-datum-tags datum))
         (new-tags (elfeed-entry-tags entry))
         (common-tags (seq-intersection old-tags new-tags)))
    (or (not (= (length old-tags) (length common-tags)))
        (not (= (length new-tags) (length common-tags))))))

(defun elfeed-sync--entry-good-p (id)
  (null (gethash id (alist-get :discarded elfeed-sync--state))))

(defun elfeed-sync--get-changed ()
  (let ((feed-hash (alist-get :feeds elfeed-sync--state))
        changed)
    (with-elfeed-db-visit (entry feed)
      (when-let ((id (elfeed-ref-id (elfeed-entry-content entry))))
        (when (elfeed-sync--entry-good-p id)
          (let ((entry-hash (gethash (elfeed-feed-id feed) feed-hash)))
            (unless entry-hash
              (setq entry-hash (make-hash-table :test #'equal))
              (puthash (elfeed-feed-id feed) entry-hash feed-hash))
            (if-let ((datum (gethash id entry-hash)))
                (when (elfeed-sync--datum-chaged datum entry)
                  (push (list entry feed id) changed))
              (push (list entry feed id) changed)))))
      (when (> (- (time-convert nil 'integer)
                  elfeed-sync-look-back)
               (elfeed-entry-date entry))
        (elfeed-db-return)))
    changed))

(defun elfeed-sync--prepare-request (bad-feeds)
  (let ((changed (elfeed-sync--get-changed)))
    `((bad_feeds . ,(cl-loop for key being the hash-keys of bad-feeds
                             collect key))
      (changed .,(mapcar
                  (lambda (datum)
                    (let ((entry (nth 0 datum))
                          (feed (nth 1 datum))
                          (id (nth 2 datum)))
                      (if (gethash (or (elfeed-feed-url feed)
                                       (elfeed-feed-id feed))
                                   bad-feeds)
                          `((id . ,id)
                            (title . ,(elfeed-entry-title entry))
                            (url . ,(elfeed-entry-link entry))
                            (feed_url . ,(or (elfeed-feed-url feed)
                                             (elfeed-feed-id feed)))
                            (updated . ,(format-time-string
                                         "%Y-%m-%d %H:%M:%S"
                                         (seconds-to-time (elfeed-entry-date entry))
                                         "UTC0"))
                            (tags . ,(elfeed-entry-tags entry)))
                        `((id . ,id)
                          (feed_url . ,(or (elfeed-feed-url feed)
                                           (elfeed-feed-id feed)))
                          (url . ,(elfeed-entry-link entry))
                          (tags . ,(elfeed-entry-tags entry))))))
                  changed))
      (last_sync . ,(alist-get :last-sync elfeed-sync--state))
      (unread_tag . ,elfeed-sync-unread-tag)
      (marked_tag . ,elfeed-sync-marked-tag)
      (look_back . ,elfeed-sync-look-back))))

(defun elfeed-sync--sort-response-entries (entries bad-feeds entries-by-title-date entries-by-url is-new)
  (dolist (entry entries)
    (setf (alist-get 'is-new entry) is-new)
    (if (gethash (alist-get 'feed_url entry) bad-feeds)
        (let ((title-date (format "%s---%s"
                                  (alist-get 'title entry)
                                  (alist-get 'updated entry))))
          (puthash title-date entry entries-by-title-date))
      (puthash (alist-get 'link entry) entry entries-by-url))))

(defun elfeed-sync--get-response-entry (entry feed entries-by-title-date entries-by-url)
  (if (gethash (or (elfeed-feed-url feed)
                   (elfeed-feed-id feed))
               bad-feeds)
      (let ((title-date (format "%s---%s"
                                (elfeed-entry-title entry)
                                (format-time-string
                                 "%Y-%m-%d %H:%M:%S"
                                 (seconds-to-time (elfeed-entry-date entry))
                                 "UTC0"))))
        (prog1
            (gethash title-date entries-by-title-date)
          (remhash title-date entries-by-title-date)))
    (prog1
        (gethash (elfeed-entry-link entry) entries-by-url)
      (remhash (elfeed-entry-link entry) entries-by-url))))

(defun elfeed-sync--set-entry-unread (entry status)
  "Set the unread status of ENTRY to STATUS.

STATUS is a boolean.  If nil, the entry is marked as read. ENTRY is an instance of `elfeed-entry'."
  (let ((is-unread (member elfeed-sync-unread-tag
                           (elfeed-entry-tags entry))))
    (if (and is-unread status)
        (elfeed-untag entry elfeed-sync-unread-tag)
      (when (not is-unread)
        (elfeed-tag entry elfeed-sync-unread-tag)))))

(defun elfeed-sync--set-entry-marked (entry status)
  "Set the marked status of ENTRY to STATUS.

STATUS is a boolean.  If nil, the entry is marked as
unmarked.  ENTRY is an instance of `elfeed-entry'."
  (let ((is-marked (member elfeed-sync-marked-tag
                           (elfeed-entry-tags entry))))
    (if (and is-marked status)
        (elfeed-untag entry elfeed-sync-marked-tag)
      (when (not is-marked)
        (elfeed-tag entry elfeed-sync-marked-tag)))))

(defun elfeed-sync--process-response (response bad-feeds)
  (cl-loop for entry in (alist-get 'missing-entries response)
           do (let* ((id (alist-get 'id entry))
                     (attempts (or
                                (gethash id (alist-get :missing elfeed-sync--state))
                                0)))
                (if (>= attempts elfeed-sync-max-retries)
                    (puthash id (1+ attempts) (alist-get :missing elfeed-sync--state))
                  (puthash id t (alist-get :discarded elfeed-sync--state))
                  (remhash id (alist-get :missing elfeed-sync--state)))))
  (setf (alist-get :missing-target elfeed-sync--state)
        (seq-filter (lambda (datum)
                      (< (- (time-convert nil 'integer) (car datum))
                         elfeed-sync-missing-targets-keep))
                    (alist-get :missing-target elfeed-sync--state)))
  (let ((entries-by-title-date (make-hash-table :test #'equal))
        (entries-by-url (make-hash-table :test #'equal)))
    (elfeed-sync--sort-response-entries
     (alist-get 'updated response) bad-feeds entries-by-title-date entries-by-url t)
    (elfeed-sync--sort-response-entries
     (mapcar #'cdr (alist-get :missing-target elfeed-sync--state))
     bad-feeds entries-by-title-date entries-by-url nil)
    (with-elfeed-db-visit (entry feed)
      (when-let ((id (elfeed-ref-id (elfeed-entry-content entry))))
        (if-let ((entry (elfeed-sync--get-response-entry entry feed entries-by-title-date entries-by-url)))
            (progn
              (elfeed-sync--set-entry-unread entry (alist-get 'unread entry))
              (elfeed-sync--set-entry-marked entry (alist-get 'marked entry))
              (puthash id (elfeed-sync-datum--create
                           :id id
                           :tags (elfeed-entry-tags entry))))
          (unless (or (gethash id (alist-get :missing elfeed-sync--state))
                      (gethash id (alist-get :discarded elfeed-sync--state)))
            (puthash id (elfeed-sync-datum--create
                         :id id
                         :tags (elfeed-entry-tags entry))
                     (gethash
                      (or (elfeed-feed-url feed)
                          (elfeed-feed-id feed))
                      (alist-get :feeds elfeed-sync--state))))))
      (when (> (- (time-convert nil 'integer)
                  elfeed-sync-look-back)
               (elfeed-entry-date entry))
        (elfeed-db-return)))
    (maphash (lambda (_ datum)
               (when (alist-get 'is-new datum)
                 (push datum (alist-get :missing-target elfeed-sync--state))))
             entries-by-title-date)
    (maphash (lambda (_ datum)
               (when (alist-get 'is-new datum)
                 (push datum (alist-get :missing-target elfeed-sync--state))))
             entries-by-url))
  (setf (alist-get :last-sync elfeed-sync--state)
        (time-convert nil 'integer)))

(defun elfeed-sync ()
  (interactive)
  (elfeed-sync--session
   (let* ((bad-feeds (elfeed-sync--get-bad-feeds))
          (request (elfeed-sync--prepare-request bad-feeds)))
     (request (concat elfeed-sync-tt-rss-instance "/api/")
       :type "POST"
       :data (json-encode
              `(("op" . "syncElfeed")
                ("sid" . ,elfeed-sync--tt-rss-sid)
                ("data" . ,request)))
       :parser 'elfeed-sync--json-read-safe
       :headers '(("Content-Type" . "application/json"))
       :success (elfeed-sync--handler
                 (elfeed-sync--process-response
                  (alist-get 'content data)
                  bad-feeds))))))

;;;###autoload
(define-minor-mode elfeed-sync-mode
  "TODO"
  :global t
  (if elfeed-sync-mode
      (progn
        (add-hook 'kill-emacs-hook #'elfeed-sync-state-save-safe))
    (remove-hook 'kill-emacs-hook #'elfeed-sync-state-save-safe)))


(provide 'elfeed-sync)
;;; elfeed-sync.el ends here
