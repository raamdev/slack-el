;;; slack.el --- slack client                        -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 Kyle W T Sherman

;; Author: Kyle W T Sherman <kylewsherman@gmail.com>
;; Keywords: comm, convenience
;; Created:  2013-12-10
;; Version:  1.0
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
;; Originally authored by Kyle W T Sherman <kylewsherman at gmail dot com>
;; See http://nullman.net/tutorial/emacs-files/.emacs.d/local-modules/slack.el.html
;; =======================================
;; CURRENTLY NOT WORKING; INCOMPLETE
;; See https://github.com/raamdev/slack-el
;; =======================================
;;
;; slack is a client for the slack service (http://slack.com/).
;;
;; Can be customized with the following command:
;;
;;   (customize-group "slack")
;;
;; `slack-token' must be set to your token found here:
;;
;;   https://api.slack.com/

;;; Code:

(require 'json)

;; customize group
(defgroup slack nil
  "Slack client."
  :prefix "slack-"
  :group 'applications)

;; slack buffer name
(defcustom slack-buffer-name
  "*Slack*"
  "Slack buffer name."
  :type 'string
  :group 'slack)

;; slack token
(defcustom slack-token
  "xoxp-2151144447-2151498120-2155203918-21ecbe"
  "Slack token."
  :type 'string
  :group 'slack)

;; slack api url
(defvar slack-url "https://slack.com/api/")

;; data
(defvar slack-data)
(defvar slack-new-data)

(defmacro json-true (val)
  "Return non-nil if val is not `eq' to ':json_false`,
otherwise return nil."
  `(not (eq ,val :json-false)))

(defmacro json-false (val)
  "Return non-nil if val is `eq' to ':json_false`,
otherwise return nil."
  `(eq ,val :json-false))

;; submit query
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
  (slack-refresh))

;; FIXME: this is broken
;; (defun slack-load-data-sets ()
;;   "Process new data sets and return non-nil if a change was made."
;;   (let (change)
;;     (while (plusp (length slack-new-data))
;;       (let ((new (pop slack-new-data)))
;;         (let ((cur (assoc (car new) slack-data)))
;;           (when (cl-set-difference new cur)
;;             (setq (assoc (car new) slack-data) (cdr new)
;;                   change t)))))
;;     change))

(defun slack-refresh ()
  "Display data and interface according to `slack-state'."
  (when (slack-load-data-sets)
    (save-excursion
      (cl-labels ((wc (x)
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
        (let ((buffer (get-buffer-create slack-buffer-name)))
          (set-buffer buffer)
          ;; add header
          (widget-insert "Channels\n\n")
          (if (json-true (cdr (assoc 'ok json)))
              (let ((channels (cdr (assoc 'channels json))))
                (mapcar #'(lambda (x) (wc x))
                        (remove-if #'(lambda (x) (json-false (cdr (assoc 'is_member x))))
                                   channels))
                (widget-insert "\n")
                (mapcar #'(lambda (x) (wc x))
                        (remove-if #'(lambda (x) (json-true (cdr (assoc 'is_member x))))
                                   channels)))
            (progn
              (widget-insert (format "Error fetching channels: %s\n\n" (cdr (assoc 'error json))))
              (widget-create 'push-button
                             :value "Reload"
                             :notify (lambda (&rest ignore)
                                       (kill-buffer nil)
                                       (slack-channel-list)))
              (widget-insert "  ")
              (widget-create 'push-button
                             :value "Customize Slack"
                             :notify (lambda (&rest ignore)
                                       (customize-group 'slack)))))
          ;; final setup
          (use-local-map widget-keymap)
          (widget-setup)
          (switch-to-buffer buffer)
          (goto-char (point-min))
          (widget-forward 1))))
    ))

(defun slack-group-list ()
  "Fetch list of groups."
  (slack-query 'slack-new-data-set :group "groups.list"))

(defun slack-channel-list ()
  "Fetch list of channels."
  (slack-query 'slack-new-data-set :channel "channels.list"))

;; start slack client
;;;###autoload
(defun slack ()
  "Start Slack client."
  (interactive)
  ;; setup buffer
  ;; (when (get-buffer slack-buffer-name)
  ;;   (kill-buffer slack-buffer-name))
  (let ((buffer (get-buffer-create slack-buffer-name)))
    (set-buffer buffer)
    ;; (kill-all-local-variables)
    (unless (local-variable-if-set-p slack-state)
      (set (make-local-variable slack-state) '(:top . nil)))
    (unless (local-variable-if-set-p slack-data)
      (make-local-variable slack-data))
    (unless (local-variable-if-set-p slack-new-data)
      (make-local-variable slack-new-data))
    (slack-group-list)
    (slack-channel-list)))

(provide 'slack)
;;; slack.el ends here
