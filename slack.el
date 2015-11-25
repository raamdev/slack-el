;;; slack.el --- slack client                        -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 Kyle W T Sherman

;; Author: Kyle W T Sherman <kylewsherman@gmail.com>
;; Keywords: comm, convenience
;; Created:  2013-12-10
;; Version:  0.1
;; Url: https://github.com/raamdev/slack-el

;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; slack is a client for the slack service (http://slack.com/).
;;
;; It can be customized with the following command:
;;
;;   (customize-group "slack")
;;
;; `slack-auth' must be set to one or more of your teams and tokens found
;; here:
;;
;;   https://api.slack.com/
;;
;; If, like me, you store your customization.el file in a git repository, you
;; can instead do the following:
;;
;;   $ touch ~/.slack-auth && chmod 600 ~/.slack-auth
;;
;; Now edit this file and add something like the following:
;;
;;   (setq slack-auth
;;         '(("TEAM-1" . "TOKEN-1")
;;           ("TEAM-2" . "TOKEN-2")
;;           ("TEAM-N" . "TOKEN-N")))
;;
;; Originally authored by Kyle W T Sherman <kylewsherman at gmail dot com>

;;; Code:

(require 'json)

(defgroup slack nil
  "Slack client."
  :prefix "slack-"
  :group 'applications)

(defcustom slack-buffer-name
  "*Slack*"
  "Slack buffer name."
  :type 'string
  :group 'slack)

(defcustom slack-token
  "xoxp-2151144447-2151498120-2155203918-21ecbe"
  "Slack token."
  :type 'string
  :group 'slack)

(defvar slack-url "https://slack.com/api/")

;; slack token
(defconst slack-auth-default-team nil)
(defconst slack-auth-default-token nil)
(defconst slack-auth-file-name (expand-file-name "~/.slack-auth"))
(defcustom slack-auth
  `((,slack-auth-default-team . ,slack-auth-default-token))
  "Slack authorizations."
  :type 'list
  :group 'slack)

(defconst slack-url "https://slack.com/api/" "Url of the Slack Api.")

;; data
(defvar slack-team nil "Current slack team name.")
(defvar slack-token nil "Current slack token.")
(defvar slack-state  nil "Current state of client.")
(defvar slack-data (make-hash-table) "Most recent data queried from server.")
(defvar slack-new-data nil "Newer server data to process.")


;;; helpers
(defmacro json-true (val)
  "Return non-nil if val is not `eq' to ':json_false`,
otherwise return nil."
  `(not (eq ,val :json-false)))

(defmacro json-false (val)
  "Return non-nil if val is `eq' to ':json_false`,
otherwise return nil."
  `(eq ,val :json-false))

(defmacro generate-header (text)
  "Return TEXT as a header."
  `(propertize ,text 'face 'bold))


(defun slack-query (callback type method &optional args)
  "Call CALLBACK with resulting JSON from submitting ARGS to slack METHOD."
  (let* ((url-request-method "GET")
         (args (concat "?token=" (url-hexify-string slack-token)
                       (mapconcat (lambda (arg)
                                    (concat (url-hexify-string (car arg))
                                            "="
                                            (url-hexify-string (cdr arg))))
                                  args
                                  "&")))
         (query (concat slack-url method args))
         url-http-end-of-headers)
    ;;(message query)
    (url-retrieve query `(lambda (status)
                           (goto-char url-http-end-of-headers)
                           ;;(message "%s" (json-read))))))
                           (funcall (function ,callback) ,type (json-read))))))

(defun slack-new-data-set (type json)
  "Set data TYPE to JSON."
  (push (cons type json) slack-new-data)
  (when (slack-load-data-sets)
    (slack-refresh)))

(defun slack-load-data-sets ()
  "Process new data sets and return non-nil if a change was made."
  (let ((change))
    (mapcar (lambda (new-entry)
              "Update `slack-data' with received input"
              (let* ((current-key (car new-entry))
                     (current-item (gethash current-key slack-data)))
                (when (cl-set-difference new-entry current-item)
                  (setf (gethash current-key slack-data) (cdr new-entry))
                  (setq change t))))
            slack-new-data)
    (setq slack-new-data '())
    change))

;; Doc: add json content
;; ((groups . [])
;;  (ok . t))

;; ((channels . [((num_members . 1) (purpose (last_set . 0) (creator . ) (value . This channel is for team-wide communication and announcements. All team members are in this channel.)) (topic (last_set . 0) (creator . ) (value . )) (members . [U0321RGMR]) (is_member . t) (is_general . t) (is_archived . :json-false) (creator . U0321RGMR) (created . 1416769379) (is_channel . t) (name . general) (id . C0321RGMX))
;;               ((num_members . 1) (purpose (last_set . 0) (creator . ) (value . A place for non-work banter, links, articles of interest, humor or anything else which you'd like concentrated in some place other than work-related channels.)) (topic (last_set . 0) (creator . ) (value . )) (members . [U0321RGMR]) (is_member . t) (is_general . :json-false) (is_archived . :json-false) (creator . U0321RGMR) (created . 1416769379) (is_channel . t) (name . random) (id . C0321RGMZ))
;;               ])
;;  (ok . t))

(defun slack-refresh ()
  "Display data and interface according to `slack-state'."
  (save-excursion
    (cl-labels ((wc (x &optional s)
                    (widget-insert (make-string (or s 0) ? ))
                    (widget-create 'push-button
                                   :value (format "%s" (cdr (assoc 'name x)))
                                   :notify `(lambda (&rest ignore)
                                              (kill-buffer nil)
                                              ;; open channel
                                              (message ,(format "channel id: %s" (cdr (assoc 'id x))))))
                    (widget-insert "\n")))
      ;; setup buffer
      (when (get-buffer slack-buffer-name)
        (kill-buffer slack-buffer-name))
      (let ((buffer (get-buffer-create slack-buffer-name))
            (channels-json (gethash :channel slack-data)))
        (set-buffer buffer)
        ;; add header
        (widget-insert (generate-header "Channels"))
        (widget-insert "\n\n")
        (if (json-true (cdr (assoc 'ok channels-json)))
            (let* ((channels (cdr (assoc 'channels channels-json)))
                   (non-member-channels (cl-remove-if #'(lambda (x) (json-false (cdr (assoc 'is_member x)))) channels))
                   (member-channels (cl-remove-if #'(lambda (x) (json-true (cdr (assoc 'is_member x)))) channels)))
              (when non-member-channels
                (mapc #'(lambda (x) (wc x 2)) non-member-channels)
                (widget-insert "\n"))
              (when member-channels
                (mapc #'(lambda (x) (wc x 2)) member-channels)
                (widget-insert "\n")))
          (widget-insert (format "Error fetching channels: %s\n\n" (cdr (assoc 'error channels-json)))))
        (progn
          (widget-create 'push-button
                         :value "Refresh"
                         :notify (lambda (&rest ignore)
                                   (kill-buffer)
                                   (slack-channel-list)))
          (widget-insert "\n")
          (widget-create 'push-button
                         :value "Customize"
                         :notify (lambda (&rest ignore)
                                   (customize-group 'slack))))
        ;; final setup
        (use-local-map widget-keymap)
        (widget-setup)
        (switch-to-buffer buffer)
        (goto-char (point-min))
        (widget-forward 1)))))

(defun slack-group-list ()
  "Fetch list of groups."
  (slack-query 'slack-new-data-set :group "groups.list"))

(defun slack-channel-list ()
  "Fetch list of channels."
  (slack-query 'slack-new-data-set :channel "channels.list"))

;;;###autoload
(defun slack (&optional team token)
  "Start Slack client.

load slack authentication file if it exists and `slack-auth' has not been customized"
  (interactive)
  (let ((auth (or (and team token (cons team token))
                  (and slack-auth (car slack-auth) (caar slack-auth) slack-auth)
                  (and (file-exists-p slack-auth-file-name) (load slack-auth-file-name)
                       slack-auth (car slack-auth) (caar slack-auth) slack-auth)
                  (cons (read-string "Team: ") (read-string "Token: ")))))
    (setq slack-team (caar auth)
          slack-token (cdar auth))
    ;; setup buffer
    ;; (when (get-buffer slack-buffer-name)
    ;;   (kill-buffer slack-buffer-name))
    (let ((buffer (get-buffer-create slack-buffer-name)))
      (set-buffer buffer)
      ;; (kill-all-local-variables)
      (unless (local-variable-if-set-p 'slack-state)
        (set (make-local-variable 'slack-state) '(:top . nil)))
      (unless (local-variable-if-set-p 'slack-data)
        (make-local-variable 'slack-data))
      (unless (local-variable-if-set-p 'slack-new-data)
        (make-local-variable 'slack-new-data))
      (slack-group-list)
      (slack-channel-list))))

(provide 'slack)
;;; slack.el ends here
