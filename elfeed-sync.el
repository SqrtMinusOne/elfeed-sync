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

(defvar elfeed-sync--tt-rss-sid nil
  "Session ID.")

(defvar elfeed-sync--state nil
  "State of the tt-rss sync.")

(defvar elfeed-sync--start-time nil
  "Start time of the tt-rss sync.")

(defvar elfeed-sync--elfeed-missed nil
  "List of elfeed entries missed in tt-rss.")

(defvar elfeed-sync--tt-rss-missed nil
  "List of tt-rss entries missed in elfeed.")

(cl-defstruct (elfeed-sync-datum (:constructor elfeed-sync-datum--create))
  id tags)

(defun elfeed-sync--state-empty ()
  "Create an empty elfeed-sync state."
  `((:last-sync-time . nil)
    (:ids-missing-tt-rss . ,(make-hash-table :test #'equal))))

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

(defun elfeed-sync--entry-unread-p (entry)
  "Return non-nil if ENTRY is unread."
  (not (null (member elfeed-sync-unread-tag (elfeed-entry-tags entry)))))

(defun elfeed-sync--entry-marked-p (entry)
  "Return non-nil if ENTRY is marked."
  (not (null (member elfeed-sync-marked-tag (elfeed-entry-tags entry)))))

(defun elfeed-sync--set-entry-unread (entry status)
  "Set the unread status of ENTRY to STATUS.

STATUS is a boolean.  If nil, the entry is marked as read. ENTRY
is an instance of `elfeed-entry'."
  (let ((is-unread (elfeed-sync--entry-unread-p entry)))
    (when (and is-unread (not status))
      (elfeed-untag entry elfeed-sync-unread-tag))
    (when (and (not is-unread) status)
      (elfeed-tag entry elfeed-sync-unread-tag))))

(defun elfeed-sync--set-entry-marked (entry status)
  "Set the marked status of ENTRY to STATUS.

STATUS is a boolean.  If nil, the entry is marked as
unmarked.  ENTRY is an instance of `elfeed-entry'."
  (let ((is-marked (elfeed-sync--entry-marked-p entry)))
    (when (and is-marked (not status))
      (elfeed-untag entry elfeed-sync-marked-tag))
    (when (and (not is-marked) status)
      (elfeed-tag entry elfeed-sync-marked-tag))))

(defun elfeed-sync--ttrss-key (bad-feeds ttrss-entry)
  (let ((feed-url (alist-get 'feed_url ttrss-entry)))
    (if (gethash feed-url bad-feeds)
        (format "%s---%s" (alist-get 'title ttrss-entry)
                (alist-get 'updated ttrss-entry))
      (alist-get 'link ttrss-entry))))

(defun elfeed-sync--elfeed-key (bad-feeds elfeed-entry feed-url)
  (if (gethash feed-url bad-feeds)
      (format "%s---%s" (elfeed-entry-title elfeed-entry)
              (floor (elfeed-entry-date elfeed-entry)))
    (elfeed-entry-link elfeed-entry)))

(defun elfeed-sync--ttrss-get-updated-time (ttrss-entry)
  (if (and (alist-get 'last_read ttrss-entry)
           (alist-get 'last_marked ttrss-entry))
      (max (alist-get 'last_read ttrss-entry)
           (alist-get 'last_marked ttrss-entry))
    (or (alist-get 'last_read ttrss-entry)
        (alist-get 'last_marked ttrss-entry))))

(defun elfeed-sync--ttrss-get-last-sync-time (ttrss-id ttrss-time)
  (if ttrss-time
      (if-let* ((val (gethash
                      ttrss-id
                      (alist-get :ids-missing-tt-rss
                                 elfeed-sync--state)))
                (time-equal (= (car var) ttrss-time)))
          (cdr var)
        (alist-get :last-sync-time elfeed-sync--state))
    (alist-get :last-sync-time elfeed-sync--state)))

(defun elfeed-sync--update-ttrss-missing (ttrss-entries ttrss-entries-processed)
  (let (all-missing)
    (maphash (lambda (ttrss-id ttrss-entry)
               (unless (gethash ttrss-id ttrss-entries-processed)
                 (push ttrss-entry all-missing)
                 (when-let ((ttrss-time (elfeed-sync--ttrss-get-updated-time
                                         ttrss-entry)))
                   (if-let ((old-val (gethash ttrss-id
                                              (alist-get :ids-missing-tt-rss
                                                         elfeed-sync--state)))
                            (is-equal (= (car old-val) ttrss-time)))
                       t ;; do nothing
                     (puthash ttrss-id (cons ttrss-time
                                             elfeed-sync--start-time)
                              (alist-get :ids-missing-tt-rss
                                         elfeed-sync--state))))))
             ttrss-entries)
    all-missing))

(defun elfeed-sync--do-sync (entries bad-feeds)
  (let ((ttrss-entries (make-hash-table :test #'equal))
        (ttrss-entries-processed (make-hash-table :test #'equal))
        (ttrss-toggle-marked nil)
        (ttrss-toggle-unread nil)
        (elfeed-toggle-unread-count 0)
        (elfeed-toggle-marked-count 0)
        (elfeed-total-entries 0)
        (missing-elfeed))
    (cl-loop for ttrss-entry being the elements of entries
             do (puthash (elfeed-sync--ttrss-key bad-feeds ttrss-entry)
                         ttrss-entry ttrss-entries))
    (with-elfeed-db-visit (entry feed)
      (cl-incf elfeed-total-entries)
      (if-let ((ttrss-entry
                (gethash (elfeed-sync--elfeed-key bad-feeds entry (elfeed-feed-url feed))
                         ttrss-entries)))
          (let* ((is-unread (elfeed-sync--entry-unread-p entry))
                 (is-marked (elfeed-sync--entry-marked-p entry))
                 (ttrss-id (alist-get 'id ttrss-entry))
                 (ttrss-time (elfeed-sync--ttrss-get-updated-time ttrss-entry))
                 (last-sync-time
                  (elfeed-sync--ttrss-get-last-sync-time ttrss-id ttrss-time))
                 (ttrss-priority (if (and last-sync-time ttrss-time)
                                     (> ttrss-time last-sync-time)
                                   (not (null ttrss-time))))
                 (ttrss-is-unread (eq (alist-get 'unread ttrss-entry) t))
                 (ttrss-is-marked (eq (alist-get 'marked ttrss-entry) t)))
            (when (not (eq ttrss-is-unread is-unread))
              (if ttrss-priority
                  (progn
                    (elfeed-sync--set-entry-unread entry ttrss-is-unread)
                    (cl-incf elfeed-toggle-unread-count))
                (push ttrss-id ttrss-toggle-unread)))
            (when (not (eq ttrss-is-marked is-marked))
              (if ttrss-priority
                  (progn
                    (elfeed-sync--set-entry-marked entry ttrss-is-marked)
                    (cl-incf elfeed-toggle-marked-count))
                (push ttrss-id ttrss-toggle-marked)))
            (puthash ttrss-id t ttrss-entries-processed))
        (push entry missing-elfeed))
      (when (> (- (time-convert nil 'integer)
                  elfeed-sync-look-back)
               (elfeed-entry-date entry))
        (elfeed-db-return)))
    (setq elfeed-sync--tt-rss-missed
          (elfeed-sync--update-ttrss-missing ttrss-entries ttrss-entries-processed))
    (setf (alist-get :last-sync-time elfeed-sync--state)
          elfeed-sync--start-time)
    `((:ttrss-toggle-unread . ,ttrss-toggle-unread)
      (:ttrss-toggle-marked . ,ttrss-toggle-marked)
      (:missing-elfeed . ,missing-elfeed)
      (:ttrss-entries . ,ttrss-entries)
      (:elfeed-total-entries . ,elfeed-total-entries)
      (:ttrss-total-entries . ,(hash-table-count ttrss-entries))
      (:elfeed-toggle-unread-count . ,elfeed-toggle-unread-count)
      (:elfeed-toggle-marked-count . ,elfeed-toggle-marked-count)
      (:ttrss-missing-count . ,(- (hash-table-count ttrss-entries)
                                  (hash-table-count ttrss-entries-processed))))))

(defun elfeed-sync--process-sync-data (sync-data)
  (elfeed-log 'info "Total entries in %s:   %s"
              (propertize "elfeed" 'face 'elfeed-log-info-level-face)
              (alist-get :elfeed-total-entries sync-data))
  (elfeed-log 'info "Total entries in %s:   %s"
              (propertize "tt-rss" 'face 'elfeed-log-warn-level-face)
              (alist-get :ttrss-total-entries sync-data))
  (elfeed-log 'info "Toggled unread in %s:  %s"
              (propertize "elfeed" 'face 'elfeed-log-info-level-face)
              (alist-get :elfeed-toggle-unread-count sync-data))
  (elfeed-log 'info "Toggled marked in %s:  %s"
              (propertize "elfeed" 'face 'elfeed-log-info-level-face)
              (alist-get :elfeed-toggle-marked-count sync-data))
  (elfeed-log 'info "Toggled unread in %s:  %s"
              (propertize "tt-rss" 'face 'elfeed-log-warn-level-face)
              (length (alist-get :ttrss-toggle-unread sync-data)))
  (elfeed-log 'info "Toggled marked in %s:  %s"
              (propertize "tt-rss" 'face 'elfeed-log-warn-level-face)
              (length (alist-get :ttrss-toggle-marked sync-data)))
  (elfeed-log 'info "Missing entries in %s: %s"
              (propertize "elfeed" 'face 'elfeed-log-info-level-face)
              (length (alist-get :missing-elfeed sync-data)))
  (elfeed-log 'info "Missing entries in %s: %s"
              (propertize "tt-rss" 'face 'elfeed-log-warn-level-face)
              (alist-get :ttrss-missing-count sync-data))
  (setq elfeed-sync--elfeed-missed (alist-get :missing-elfeed sync-data))
  (message "Sync complete!"))

(defun elfeed-sync--apply-to-ttrss (sync-data)
  (message "Propagating changes to tt-rss...")
  (elfeed-sync--session
   (request (concat elfeed-sync-tt-rss-instance "/api/")
     :type "POST"
     :data (json-encode
            `(("op" . "toggleEntries")
              ("sid" . ,elfeed-sync--tt-rss-sid)
              ("data" .
               (("toggle_unread" . ,(alist-get :ttrss-toggle-unread sync-data))
                ("toggle_marked" . ,(alist-get :ttrss-toggle-marked sync-data))))))
     :headers '(("Content-Type" . "application/json"))
     :parser 'elfeed-sync--json-read-safe
     :success (elfeed-sync--handler
               (elfeed-sync--process-sync-data sync-data))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
                    (message "Error: %S" error-thrown))))))

(defun elfeed-sync ()
  (interactive)
  (elfeed-sync--session
   (setq elfeed-sync--start-time (time-convert nil 'integer))
   (elfeed-log 'info "Sync start: %s" (format-time-string "%Y-%m-%d %H:%M:%S"))
   (let ((bad-feeds (elfeed-sync--get-bad-feeds)))
     (request (concat elfeed-sync-tt-rss-instance "/api/")
       :type "POST"
       :data (json-encode
              `(("op" . "getSyncEntries")
                ("sid" . ,elfeed-sync--tt-rss-sid)
                ("data" .
                 (("bad_feeds" . ,(cl-loop for feed being the hash-keys of bad-feeds
                                           collect feed))
                  ("look_back" . ,elfeed-sync-look-back)))))
       :headers '(("Content-Type" . "application/json"))
       :parser 'elfeed-sync--json-read-safe
       :success (elfeed-sync--handler
                 (elfeed-sync--apply-to-ttrss
                  (elfeed-sync--do-sync
                   (alist-get 'entries
                              (alist-get 'content data))
                   bad-feeds)))
       :error
       (cl-function (lambda (&key error-thrown &allow-other-keys)
                      (message "Error: %S" error-thrown)))))))

(defun elfeed-sync-search-missing ()
  (interactive)
  (switch-to-buffer (elfeed-search-buffer))
  (unless (eq major-mode 'elfeed-search-mode)
    (elfeed-search-mode))
  (with-current-buffer (elfeed-search-buffer)
    (elfeed-save-excursion
      (let ((inhibit-read-only t)
            (standard-output (current-buffer)))
        (erase-buffer)
        (setf elfeed-search-entries elfeed-sync--elfeed-missed)
        (unless (eq elfeed-sort-order 'ascending)
          (setf elfeed-search-entries (nreverse elfeed-search-entries)))
        (dolist (entry elfeed-search-entries)
          (funcall elfeed-search-print-entry-function entry)
          (insert "\n"))
        (setf elfeed-search-last-update (float-time))))
    (when (zerop (buffer-size))
      ;; If nothing changed, force a header line update
      (force-mode-line-update))
    (run-hooks 'elfeed-search-update-hook)))

(defun elfeed-sync-read-ttrss-missing ()
  (interactive)
  (when (y-or-n-p "This will read all the missing tt-rss entries. Are you sure? ")
    (elfeed-sync--session
     (request (concat elfeed-sync-tt-rss-instance "/api/")
       :type "POST"
       :data (json-encode
              `(("op" . "toggleEntries")
                ("sid" . ,elfeed-sync--tt-rss-sid)
                ("data" .
                 (("toggle_unread"
                   . ,(cl-loop for ttrss-entry in elfeed-sync--tt-rss-missed
                               if (eq t (alist-get 'unread ttrss-entry))
                               collect (alist-get 'id ttrss-entry)))
                  ("toggle_marked" . ())))))
       :headers '(("Content-Type" . "application/json"))
       :parser 'elfeed-sync--json-read-safe
       :success (elfeed-sync--handler
                 (message "Success!"))
       :error
       (cl-function (lambda (&key error-thrown &allow-other-keys)
                      (message "Error: %S" error-thrown)))))))

(defun elfeed-sync-read-ttrss-old ()
  (interactive)
  (when (y-or-n-p "This will read all the old tt-rss entries. Are you sure? ")
    (elfeed-sync--session
     (request (concat elfeed-sync-tt-rss-instance "/api/")
       :type "POST"
       :data (json-encode
              `(("op" . "readOldEntries")
                ("sid" . ,elfeed-sync--tt-rss-sid)
                ("data" .
                 (("look_back" . ,elfeed-sync-look-back)))))
       :headers '(("Content-Type" . "application/json"))
       :parser 'elfeed-sync--json-read-safe
       :success (elfeed-sync--handler
                 (message "Success!"))
       :error
       (cl-function (lambda (&key error-thrown &allow-other-keys)
                      (message "Error: %S" error-thrown)))))))

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
