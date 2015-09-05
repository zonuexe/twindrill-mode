;;; twindrill-render.el --- Tweet renderer

;; Copyright (C) 2009-2015 Tadashi MATSUO
;;               2007, 2009-2011 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO
;;               2014, 2015 Xavier Maillard

;; Author: Tadashi MATSUO <tad@mymail.twin.ne.jp>
;;         Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;         Alberto Garcia <agarcia@igalia.com>
;;         Xavier Maillard <xavier@maillard.im>
;; Created: Sep 4, 2007
;; Version: 4.0
;; Identity: $Id: dae9936d49078a7e394100014b868c2df9abde4c $
;; Keywords: twitter web
;; URL: https://github.com/zonuexe/twindrill-mode
;; Package-Requires: ((emacs "24"))


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; twindrill-mode is forked from twittering-mode http://twmode.sourceforge.net/
;; thanks!

;; ξ ^ω^)ξ ＜You are an idiot, really.

;;; Code:

;;;;
;;;; Rendering
;;;;

(defun twindrill-field-id< (field1 field2)
  "Return t if version `FIELD1' is lower (older) than `FIELD2'."
  (string< field1 field2))

(defun twindrill-field-id= (field1 field2)
  "Return t if version `FIELD1' is equal to `FIELD2'."
  (string= field1 field2))

(defun twindrill-make-field-id-from-id (id &optional base-id)
  "Generate a field property for the tweet corresponding to ID.
Tweets are rendered in order of the field on `twindrill-mode'.

If BASE-ID is non-nil, generate a field id for a tweet rendered
as a popped ancestor tweet by `twindrill-show-replied-statuses'.
In the case, BASE-ID means the ID of the descendant."
  (let ((format-func (lambda (id) (format "%02d-%s" (length id) id))))
    (cond
     (base-id
      (format "O:%s:5:ancestor:%s"
	      (funcall format-func base-id)
	      (funcall format-func id)))
     (t
      (format "O:%s:8" (funcall format-func id))))))

(defun twindrill-make-field-id (status &optional base-id)
  "Generate a field property for STATUS.
Tweets are rendered in order of the field on `twindrill-mode'.

If BASE-ID is non-nil, generate a field id for a tweet rendered
as a popped ancestor tweet by `twindrill-show-replied-statuses'.
In the case, BASE-ID means the ID of the descendant."
  (let ((id (cdr (assq 'id status))))
    (twindrill-make-field-id-from-id id base-id)))

(defun twindrill-make-properties-of-popped-ancestors (base-id)
  `(rendered-as ((ancestor-of . ,base-id))))

(defun twindrill-make-field-id-of-timeline-oldest-end (spec-string)
  "Return the field ID for the oldest end.
This is given to a special field, header or footer, which does not correspond
to a tweet.
It must be less than IDs made by `twindrill-make-field-id' for any other
normal fields in the meaning of `twindrill-field-id<'."
  (format "H:%s" spec-string))

(defun twindrill-make-field-id-of-timeline-latest-end (spec-string)
  "Return the field ID for the oldest end.
This is given to a special field, header or footer, which does not correspond
to a tweet.
It must be greater than IDs made by `twindrill-make-field-id' for any other
normal fields in the meaning of `twindrill-field-id<'."
  (format "U:%s" spec-string))

(defun twindrill-field-id-is-timeline-oldest-end (field-id)
  "Return non-nil if FIELD-ID corresponds to the oldest end field.
Return non-nil if FIELD-ID is made by
`twindrill-make-field-id-of-timeline-oldest-end'."
  (and (stringp field-id) (string= (substring field-id 0 2) "H:")))

(defun twindrill-field-id-is-timeline-latest-end (field-id)
  "Return non-nil if FIELD-ID corresponds to the latest end field.
Return non-nil if FIELD-ID is made by
`twindrill-make-field-id-of-timeline-latest-end'."
  (and (stringp field-id) (string= (substring field-id 0 2) "U:")))

(defun twindrill-rendered-as-ancestor-status-p (&optional pos)
  "Return non-nil if the status at POS is rendered as an ancestor.
Ancestor statuses are rendered by `twindrill-show-replied-statuses'."
  (let ((pos (or pos (point))))
    (assq 'ancestor-of (get-text-property pos 'rendered-as))))

(defun twindrill-get-base-id-of-ancestor-at (&optional pos)
  "Return the base ID of a popped ancestor status rendered at POS.
If the status at POS is not a popped ancestor status or no status is
rendered at POS, return nil."
  (let ((pos (or pos (point))))
    (cdr (assq 'ancestor-of (get-text-property pos 'rendered-as)))))

(eval-and-compile
  (defsubst twindrill-fill-string (str &optional adjustment prefix keep-newline)
    (when (and (not (boundp 'kinsoku-limit))
	       enable-kinsoku)
      ;; `kinsoku-limit' is defined on loading "international/kinsoku.el".
      ;; Without preloading, "kinsoku.el" will be loaded by auto-loading
      ;; triggered by `fill-region-as-paragraph'.
      ;; In that case, the local binding of `kinsoku-limit' conflicts the
      ;; definition by `defvar' in "kinsoku.el".
      ;; The below warning is displayed;
      ;; "Warning: defvar ignored because kinsoku-limit is let-bound".
      ;; So, we load "kinsoku.el" in advance if necessary.
      (load "international/kinsoku"))
    (let* ((kinsoku-limit 1)
	   (adjustment (+ (or adjustment 0)
			  (if enable-kinsoku
			      kinsoku-limit
			    0)))
	   (min-width
	    (apply 'min
		   (or
		    (mapcar 'window-width
			    (get-buffer-window-list (current-buffer) nil t))
		    ;; Use `(frame-width)' if no windows display
		    ;; the current buffer.
		    `(,(frame-width)))))
	   (temporary-fill-column (- (or twindrill-fill-column (1- min-width))
				     adjustment)))
      (with-temp-buffer
	(let ((fill-column temporary-fill-column)
	      (fill-prefix (or prefix fill-prefix))
	      (adaptive-fill-regexp ""))
	  (if keep-newline
	      (let* ((hard-newline (propertize "\n" 'hard t))
		     (str (mapconcat 'identity (split-string str "\n")
				     (concat hard-newline fill-prefix))))
		(use-hard-newlines)
		(insert (concat prefix str))
		(fill-region (point-min) (point-max) nil t)
		(remove-text-properties (point-min) (point-max) '(hard nil)))
	    (insert (concat prefix str))
	    (fill-region-as-paragraph (point-min) (point-max)))
	  (buffer-substring (point-min) (point-max))))))

  (defsubst twindrill-update-filled-string (beg end formater status prefix local-prefix &optional keep-newline)
    (let* ((str (twindrill-fill-string (funcall formater status prefix)
					(length prefix) local-prefix
					keep-newline))
	   (next (next-single-property-change 0 'need-to-be-updated str)))
      (if (or (get-text-property 0 'need-to-be-updated str)
	      (and next (< next (length str))))
	  (put-text-property 0 (length str) 'need-to-be-updated
			     `(twindrill-update-filled-string
			       ,formater ,status ,prefix ,local-prefix
			       ,keep-newline)
			     str)
	;; Remove the property required no longer.
	(remove-text-properties 0 (length str) '(need-to-be-updated nil) str))
      str))

  (defsubst twindrill-make-passed-time-string
    (beg end encoded-created-at time-format &optional additional-properties)
    (let* ((now (current-time))
	   (secs (+ (* (- (car now) (car encoded-created-at)) 65536)
		    (- (cadr now) (cadr encoded-created-at))))
	   (time-string
	    (cond
	     ((< secs 5) "less than 5 seconds ago")
	     ((< secs 10) "less than 10 seconds ago")
	     ((< secs 20) "less than 20 seconds ago")
	     ((< secs 30) "half a minute ago")
	     ((< secs 60) "less than a minute ago")
	     ((< secs 150) "1 minute ago")
	     ((< secs 2400) (format "%d minutes ago"
				    (/ (+ secs 30) 60)))
	     ((< secs 5400) "about 1 hour ago")
	     ((< secs 84600) (format "about %d hours ago"
				     (/ (+ secs 1800) 3600)))
	     (t (format-time-string time-format encoded-created-at))))
	   (properties (append additional-properties
			       (and beg (text-properties-at beg))))
	   (time-string
	    ;; Copy a string and restore properties.
	    (apply 'propertize time-string properties)))
      (if (< secs 84600)
	  (put-text-property 0 (length time-string)
			     'need-to-be-updated
			     `(twindrill-make-passed-time-string
			       ,encoded-created-at ,time-format)
			     time-string)
	;; Remove the property required no longer.
	(remove-text-properties 0 (length time-string)
				'(need-to-be-updated nil)
				time-string))
      time-string)))

(defmacro twindrill-render-a-field (pos field-id generator &optional without-separator)
  "Render a field on the current buffer managed by `twindrill-mode'.
Insert a field to the position pointed by FIELD-ID.  The position is searched
after POS.  The string for the field is generated by the GENERATOR expression.
This function does not render the status if a status with the same field ID
as FIELD-ID is already rendered.
Return non-nil if the status is rendered.  Otherwise, return nil."
  `(lexical-let ((pos ,pos)
		 (field-id ,field-id))
     (while
	 (let ((buf-field-id (get-text-property pos 'field)))
	   (if (and buf-field-id
		    (if twindrill-reverse-mode
			(twindrill-field-id< buf-field-id field-id)
		      (twindrill-field-id< field-id buf-field-id)))
	       (let ((next-pos
		      (twindrill-get-next-status-head pos)))
		 (setq pos (or next-pos (point-max)))
		 next-pos)
	     nil)))
     (goto-char pos)
     (unless (twindrill-field-id= field-id (get-text-property pos 'field))
       (let ((formatted-status (propertize ,generator 'field ,field-id))
	     (separator (if ,without-separator
			    ""
			  "\n")))
	 (if (eq pos (point-max))
	     ;; Use `insert' only if no statuses are rendered on the below.
	     (insert formatted-status separator)
	   ;; Use `insert-before-markers' in order to keep
	   ;; which status is pointed by each marker.
	   (insert-before-markers formatted-status separator))
	 t))))

(defun twindrill-render-timeline (buffer timeline-data &optional invoke-hook keep-point)
  "Render statuses for BUFFER and return the list of the rendered statuses.
TIMELINE-DATA is a list of statuses being rendered.
If INVOKE-HOOK is non-nil and one or more tweets are rendered, run hooks
specified by `twindrill-new-tweets-rendered-hook'.
If KEEP-POINT is nil and BUFFER is empty, this function moves cursor positions
to the latest status.

This function returns a list of the statuses newly rendered by the invocation."
  (with-current-buffer buffer
    (let* ((spec (twindrill-get-timeline-spec-for-buffer buffer))
	   (referring-id-table
	    (twindrill-current-timeline-referring-id-table spec))
	   (timeline-data
	    ;; Collect visible statuses.
	    (let ((prev-id nil))
	      (remove
	       nil
	       (mapcar
		(lambda (status)
		  (let ((id (cdr (assq 'id status)))
			(retweeted-id (cdr (assq 'retweeted-id status))))
		    (if (twindrill-status-id= prev-id id)
			;; `status' is equivalent the previous one.
			nil
		      (setq prev-id id)
		      (cond
		       ((null retweeted-id)
			;; `status' is not a retweet.
			status)
		       ((and retweeted-id
			     (twindrill-status-id=
			      id (gethash retweeted-id referring-id-table)))
			;; `status' is the first retweet.
			status)
		       ((null (gethash retweeted-id referring-id-table))
			;; If the first ID referring the retweet is unknown,
			;; render it.
			;; This is necessary because a referring ID table
			;; of a composite timeline may lack information of
			;; some component timelines.
			status)
		       (t
			;; Otherwise, do not render it.
			nil)))))
		timeline-data))))
	   (timeline-data (if twindrill-reverse-mode
			      (reverse timeline-data)
			    timeline-data))
	   (rendering-entire (null (twindrill-get-first-status-head)))
	   (result-tweets nil)
	   (buffer-read-only nil))
      (twindrill-update-status-format)
      (twindrill-update-mode-line)
      (save-excursion
	(let ((pos (point-min))
	      (spec-string
	       (twindrill-get-timeline-spec-string-for-buffer buffer)))
	  (cond
	   (rendering-entire
	    (let* ((latest-id
		    (twindrill-make-field-id-of-timeline-latest-end
		     spec-string))
		   (oldest-id
		    (twindrill-make-field-id-of-timeline-oldest-end
		     spec-string))
		   (footer-id
		    (if twindrill-reverse-mode
			latest-id
		      oldest-id))
		   (header-id
		    (if twindrill-reverse-mode
			oldest-id
		      latest-id)))
	      (setq
	       pos
	       (let ((footer
		      ;; To avoid adding a face to newlines.
		      (mapconcat
		       (lambda (substr)
			 (propertize substr
				     'face twindrill-timeline-footer-face))
		       (split-string (or twindrill-timeline-footer "") "\n")
		       "\n"))
		     (header
		      ;; To avoid adding a face to newlines.
		      (mapconcat
		       (lambda (substr)
			 (propertize substr
				     'face twindrill-timeline-header-face))
		       (split-string (or twindrill-timeline-header "") "\n")
		       "\n")))
		 (twindrill-render-a-field (point-min) footer-id footer t)
		 (twindrill-render-a-field (point-min) header-id header t)
		 (point)))))
	   (t
	    (setq pos (twindrill-get-first-status-head))))
	  (goto-char pos)
	  (let* ((rendered-tweets
		  (remove nil
			  (mapcar
			   (lambda (status)
			     (when (twindrill-render-a-field
				    (point)
				    (twindrill-make-field-id status)
				    (twindrill-format-status status))
			       (when twindrill-default-show-replied-tweets
				 (twindrill-show-replied-statuses
				  twindrill-default-show-replied-tweets))
			       status))
			   timeline-data)))
		 (twindrill-rendered-new-tweets
		  (if twindrill-reverse-mode
		      (reverse rendered-tweets)
		    rendered-tweets))
		 (twindrill-rendered-new-tweets-spec spec)
		 (twindrill-rendered-new-tweets-spec-string spec-string))
	    (setq result-tweets rendered-tweets)
	    (when (and invoke-hook twindrill-rendered-new-tweets)
	      (run-hooks 'twindrill-new-tweets-rendered-hook)))))
      (debug-print (current-buffer))
      (cond
       ((and (not keep-point) rendering-entire)
	;; Go to the latest status of buffer after full insertion.
	(let ((dest (if twindrill-reverse-mode
			(or (twindrill-get-last-normal-field-head)
			    (twindrill-get-last-status-head)
			    (point-max))
		      (or (twindrill-get-first-normal-field-head)
			  (twindrill-get-first-status-head)
			  (point-min))))
	      (window-list (get-buffer-window-list (current-buffer) nil t)))
	  (if window-list
	      (mapc
	       (lambda (window)
		 (set-window-point window dest)
		 (if twindrill-reverse-mode
		     (twindrill-set-window-end window (point-max))
		   (set-window-start window (point-min))))
	       window-list)
	    ;; Move the buffer position if the buffer is invisible.
	    (goto-char dest))))
       )
      result-tweets)
    ))

(defun twindrill-rerender-timeline-all (buffer &optional restore-point)
  "Re-render statuses on BUFFER after clearing BUFFER.
If RESTORE-POINT is non-nil, positions on buffers bound to the same timeline
will be restored after rendering statuses."
  (with-current-buffer buffer
    (let* ((window-list (get-buffer-window-list (current-buffer) nil t))
	   (point-window-list
	    (mapcar (lambda (window)
		      (cons (window-point window) window))
		    window-list))
	   (original-pos (point)))
      (let ((buffer-read-only nil))
	(erase-buffer))
      (twindrill-render-timeline
       (current-buffer) (twindrill-current-timeline-data) nil restore-point)
      (when restore-point
	;; Restore points.
	(mapc (lambda (pair)
		(let* ((point (car pair))
		       (window (cdr pair))
		       (dest (max (point-max) point)))
		  (set-window-point window dest)))
	      point-window-list)
	(goto-char original-pos)))))

(defun twindrill-retrieve-timeline (spec-string noninteractive api-arguments additional-info)
  "Retrieve and render a timeline specified by SPEC-STRING.
Retrieve a timeline specified by SPEC-STRING, which must be a timeline spec
string. Any timeline spec string including that for composite timeline can be
used as SPEC-STRING, though the primitive function `twindrill-call-api'
accepts only a spec of a primary timeline.

NONINTERACTIVE is sent to the sentinel as a parameter `noninteractive' via
an argument `additional-info' of `twindrill-call-api'.
API-ARGUMENTS is also sent to `twindrill-call-api' as its argument
`args-alist'."
  (let ((spec (twindrill-string-to-timeline-spec spec-string)))
    (cond
     ((not (twindrill-account-authorized-p))
      ;; ignore any requests if the account has not been authorized.
      (message "No account for Twitter has been authorized.")
      t)
     ((and noninteractive (twindrill-process-active-p spec))
      ;; ignore non-interactive request if a process is waiting for responses.
      t)
     ((twindrill-timeline-spec-primary-p spec)
      (let* ((args
	      `(,@api-arguments
		(timeline-spec . ,spec)
		(timeline-spec-string . ,spec-string)
		(format . ,(when (require 'json nil t)
			     'json))
		(clean-up-sentinel
		 . ,(lambda (proc status connection-info)
		      (when (memq status '(exit signal closed failed))
			(twindrill-release-process proc))))))
	     (additional-info
	      `(,@additional-info
		(noninteractive . ,noninteractive)
		(timeline-spec . ,spec)
		(timeline-spec-string . ,spec-string)))
	     (proc
	      (twindrill-call-api 'retrieve-timeline args additional-info)))
	(when proc
	  (twindrill-register-process proc spec spec-string)
	  (twindrill-initialize-retrieval-count spec))))
     ((twindrill-timeline-spec-composite-p spec)
      (mapc
       (lambda (spec)
	 (let* ((buffer (twindrill-get-buffer-from-spec spec))
		(spec-string
		 (if buffer
		     (twindrill-get-timeline-spec-string-for-buffer buffer)
		   (twindrill-timeline-spec-to-string spec))))
	   (twindrill-retrieve-timeline spec-string noninteractive
					 api-arguments additional-info)))
       (twindrill-get-base-timeline-specs spec)))
     (t
      (let ((type (car spec)))
	(error "%s has not been supported yet" type))))))

(defun twindrill-get-and-render-timeline (&optional noninteractive id spec spec-string)
  ""
  (let* ((spec (or spec (twindrill-current-timeline-spec)))
	 (spec-string
	  (or spec-string (twindrill-current-timeline-spec-string)))
	 (latest-status
	  ;; Assume that a list which was returned by
	  ;; `twindrill-current-timeline-data' is sorted.
	  (car (twindrill-current-timeline-data spec)))
	 (since_id (cdr-safe (assq 'id latest-status)))
	 (args `(,@(cond
		    (id `((max_id . ,id)))
		    (since_id `((since_id . ,since_id)))
		    (t nil)))))
    (twindrill-retrieve-timeline spec-string noninteractive args nil)))

;;;;
;;;; Map function for statuses on buffer
;;;;

(defun twindrill-for-each-property-region (prop func &optional buffer interrupt)
  "Apply FUNC to each region, where property PROP is non-nil, on BUFFER.
If INTERRUPT is non-nil, the iteration is stopped if FUNC returns nil."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
	  (end-marker (make-marker)))
      (set-marker-insertion-type end-marker t)
      (while
	  (let ((value (get-text-property beg prop)))
	    (if value
		(let* ((end (next-single-property-change beg prop))
		       (end (or end (point-max)))
		       (end-marker (set-marker end-marker end))
		       (func-result (funcall func beg end value))
		       (end (marker-position end-marker)))
		  (when (or (null interrupt) func-result)
		    (if (get-text-property end prop)
			(setq beg end)
		      (setq beg (next-single-property-change end prop)))))
	      (setq beg (next-single-property-change beg prop)))))
      (set-marker end-marker nil))))

;;;;
;;;; Automatic redisplay of statuses on buffer
;;;;

(defun twindrill-redisplay-status-on-buffer ()
  ""
  (mapc (lambda (buffer)
	  (unless (with-current-buffer buffer
		    (or (and (fboundp 'use-region-p) (use-region-p))
			(and transient-mark-mode mark-active)))
	    (twindrill-redisplay-status-on-each-buffer buffer)))
	(twindrill-get-buffer-list)))

(defun twindrill-redisplay-status-on-each-buffer (buffer &optional prop)
  "Redisplay regions with the text property PROP on BUFFER."
  (let ((prop (or prop 'need-to-be-updated))
	(deactivate-mark deactivate-mark)
	(window-list (get-buffer-window-list buffer nil t))
	(marker (with-current-buffer buffer (point-marker)))
	(result nil))
    (with-current-buffer buffer
      (save-excursion
	(twindrill-for-each-property-region
	 prop
	 (lambda (beg end value)
	   (let* ((func (car value))
		  (args (cdr value))
		  (current-str (buffer-substring beg end))
		  (updated-str (apply func beg end args))
		  (config (twindrill-current-window-config window-list))
		  (buffer-read-only nil))
	     ;; Replace `current-str' if it differs to `updated-str' with
	     ;; ignoring properties. This is an ad-hoc solution.
	     ;; `current-str' is a part of the displayed status, but it has
	     ;; properties which are determined by the whole status.
	     ;; (For example, the `id' property.)
	     ;; Therefore, we cannot compare the strings with their
	     ;; properties.
	     (unless (string= current-str updated-str)
	       ;; If the region to be modified includes the current position,
	       ;; the point moves to the beginning of the region.
	       (when (and (< beg marker) (< marker end))
		 ;; This is required because the point moves to the center if
		 ;; the point becomes outside of the window by the effect of
		 ;; `set-window-start'.
		 (setq result beg))
	       (let ((common-properties
		      (twindrill-get-common-properties beg)))
		 ;; Restore common properties.
		 (delete-region beg end)
		 (goto-char beg)
		 (insert (apply 'propertize updated-str common-properties)))
	       (twindrill-restore-window-config-after-modification
		config beg end))))
	 buffer))
      (set-marker marker nil)
      (when (and result (eq (window-buffer) buffer))
	(let ((win (selected-window)))
	  (when (< result (window-start win))
	    (set-window-start win result))
	  (set-window-point win result))))))

;;;;
;;;; Display replied statuses
;;;;

(defun twindrill-replied-statuses-visible-p (&optional pos)
  "Return non-nil if a replied status related to POS is visible.
Return non-nil if a replied status has been rendered at POS by
`twindrill-show-replied-statuses'.
Return non-nil if a reply is rendered at POS and the replied statuses
has been rendered by `twindrill-show-replied-statuses'.
Otherwise, return nil."
  (let* ((pos (twindrill-get-current-status-head pos))
	 (id (twindrill-get-id-at pos))
	 (prev (twindrill-get-previous-status-head pos))
	 (next (twindrill-get-next-status-head pos)))
    (when id
      ;; If ID is nil, it means that no normal tweets are rendered at POS.
      (or
       (twindrill-get-base-id-of-ancestor-at pos)
       (and prev
	    (twindrill-status-id=
	     id (twindrill-get-base-id-of-ancestor-at prev)))
       (and next
	    (twindrill-status-id=
	     id (twindrill-get-base-id-of-ancestor-at next)))))))

(defun twindrill-get-beginning-of-visible-replied-statuses (&optional pos)
  "Return the beginning position of visible replied statuses at POS.
If POS is nil, the current position is used instead.
If `twindrill-show-replied-statuses' has rendered a replied status at POS,
return the beginning position of the replied statuses with the common base
status.
If a reply is rendered at POS and its ancestors has been rendered by
`twindrill-show-replied-statuses', return the beginning position of the
replied statuses.
Otherwise, return nil."
  (let* ((pos (or pos (point)))
	 (base-id (twindrill-get-base-id-of-ancestor-at pos)))
    (cond
     (base-id
      ;; A replied status is rendered at POS.
      (while
	  (let* ((prev (twindrill-get-previous-status-head pos))
		 (prev-base-id
		  (when prev
		    (twindrill-get-base-id-of-ancestor-at prev))))
	    (and prev prev-base-id
		 (twindrill-status-id= base-id prev-base-id)
		 (setq pos prev))))
      (or pos (point-min)))
     ((twindrill-replied-statuses-visible-p pos)
      ;; A reply is rendered at POS and its replied statuses are visible.
      (if twindrill-reverse-mode
	  (twindrill-get-beginning-of-visible-replied-statuses
	   (twindrill-get-previous-status-head pos))
	(twindrill-get-next-status-head pos)))
     (t
      nil))))

(defun twindrill-get-end-of-visible-replied-statuses (&optional pos)
  "Return the end position of visible replied statuses at POS.
If POS is nil, the current position is used instead.
If `twindrill-show-replied-statuses' has rendered a replied status at POS,
return the end position of the replied statuses with the common base status.
If a reply is rendered at POS and its ancestors has been rendered by
`twindrill-show-replied-statuses', return the beginning position of the
replied statuses.
Otherwise, return nil."
  (let* ((pos (or pos (point)))
	 (base-id (twindrill-get-base-id-of-ancestor-at pos)))
    (cond
     (base-id
      ;; A replied status is rendered at POS.
      (while
	  (let ((current-base-id (twindrill-get-base-id-of-ancestor-at pos)))
	    (and current-base-id
		 (twindrill-status-id= base-id current-base-id)
		 (setq pos (twindrill-get-next-status-head pos)))))
      (or pos (point-max)))
     ((twindrill-replied-statuses-visible-p pos)
      ;; A reply is rendered at POS and its replied statuses are visible.
      (if twindrill-reverse-mode
	  (twindrill-get-current-status-head pos)
	(twindrill-get-end-of-visible-replied-statuses
	 (twindrill-get-next-status-head pos))))
     (t
      nil))))

(defun twindrill-render-replied-statuses (&optional pos count)
  "Render replied statuses on the position specified by POS.
If POS is nil, the current position is used instead.
If COUNT is a positive integer, it specifies the number of rendered statuses.
If COUNT is nil, all ancestor statuses that have been already retrieved are
rendered.

Return non-nil if one or more statuses are rendered.
Return nil if no statuses are rendered."
  (let* ((pos (or pos (point)))
	 (id
	  ;; nil if no normal statuses are rendered at POS.
	  (twindrill-get-id-at pos))
	 (replied-status-are-visible
	  (when id
	    (twindrill-replied-statuses-visible-p pos)))
	 (base-id (if replied-status-are-visible
		      (or
		       ;; If a replied status is rendered at POS.
		       (twindrill-get-base-id-of-ancestor-at pos)
		       ;; If the base reply is rendered at POS.
		       id)
		    id))
	 (statuses
	  (when base-id
	    (twindrill-get-replied-statuses base-id (if (numberp count)
							 count))))
	 (statuses (if twindrill-reverse-mode
		       statuses
		     (reverse statuses))))
    (cond
     ((null id)
      ;; No normal statuses are rendered here.
      nil)
     (statuses
      (let ((pos
	     (cond
	      ((twindrill-replied-statuses-visible-p pos)
	       ;; Some replied statuses have been already rendered.
	       (twindrill-get-beginning-of-visible-replied-statuses pos))
	      (twindrill-reverse-mode
	       (twindrill-get-current-status-head pos))
	      (t
	       (or (twindrill-get-next-status-head pos)
		   (point-max)))))
	    (prefix "  ")
	    (buffer-read-only nil))
	(save-excursion
	  (goto-char pos)
	  (mapc
	   (lambda (status)
	     (twindrill-render-a-field
	      (point)
	      (twindrill-make-field-id status base-id)
	      (let ((formatted-status (twindrill-format-status status prefix))
		    (field-properties
		     (twindrill-make-properties-of-popped-ancestors base-id)))
		(add-text-properties 0 (length formatted-status)
				     field-properties formatted-status)
		formatted-status)))
	   statuses)
	  t)))
     (t
      nil))))

(defun twindrill-render-a-status-with-delay (beg end id prefix)
  "Render a status with a delay.
It is assumed that this function is used as a property value that is
processed by the function `twindrill-redisplay-status-on-each-buffer'."
  (let ((status (twindrill-find-status id)))
    (when status
      (let ((properties (and beg (text-properties-at beg))))
	(apply 'propertize (twindrill-format-status status prefix)
	       properties)))))

(defun twindrill-toggle-or-retrieve-replied-statuses ()
  "Show/Hide all of replied statuses or retrieve a replied status.
If the cursor points to a reply or one of expanded replied statuses and
some of ancestor replied statuses have been already retrieved but they have
not been rendered, render them.
If the cursor points to a reply or one of expanded replied statuses and
all of retrieved ancestor statuses have been already rendered but the oldest
one of them is also a reply, retrieve the replied status.
If the cursor points to a reply or one of expanded replied statuses and
all of ancestor replied statuses have been already rendered, hide them by
`twindrill-hide-replied-statuses'."
  (interactive)
  (let* ((pos (point))
	 (pos
	  ;; POS points to the head of the direct reply of the status being
	  ;; retrieved.
	  (cond
	   ((twindrill-replied-statuses-visible-p pos)
	    ;; If some replied statuses are visible, find the edge.
	    (if twindrill-reverse-mode
		(twindrill-get-beginning-of-visible-replied-statuses pos)
	      (twindrill-get-previous-status-head
	       (twindrill-get-end-of-visible-replied-statuses pos))))
	   (t
	    (twindrill-get-current-status-head pos))))
	 (id (twindrill-get-id-at pos))
	 (status (twindrill-find-status id))
	 (reply-id (cdr (assq 'in-reply-to-status-id status)))
	 (reply-username (cdr (assq 'in-reply-to-screen-name status)))
	 (base-id (or (twindrill-get-base-id-of-ancestor-at pos)
		      id)))
    (cond
     ((twindrill-find-status reply-id)
      ;; The status corresponding to REPLY-ID has been already retrieved
      ;; but it has not been rendered.
      ;;
      ;; `twindrill-render-replied-statuses' additionally renders all
      ;; of already retrieved statuses.
      (twindrill-render-replied-statuses))
     (reply-id
      (let* ((pos
	      ;; POS points to the position where the new field will be
	      ;; inserted.
	      (if twindrill-reverse-mode
		  pos
		(or (twindrill-get-next-status-head pos)
		    (point-max))))
	     (field-id (twindrill-make-field-id-from-id reply-id base-id))
	     (prefix "  ")
	     (label "[RETRIEVING...]")
	     (symbol-for-redisplay 'waiting-for-retrieval)
	     (properties
	      `(,@(twindrill-make-properties-of-popped-ancestors base-id)
		,symbol-for-redisplay
		(twindrill-render-a-status-with-delay ,reply-id ,prefix)))
	     (str (apply 'propertize (concat prefix label) properties))
	     (buffer-read-only nil))
	(twindrill-call-api
	 'retrieve-single-tweet
	 `((id . ,reply-id)
	   (username . ,reply-username)
	   (format . ,(when (require 'json nil t)
			'json))
	   (sentinel . twindrill-retrieve-single-tweet-sentinel))
	 `((buffer . ,(current-buffer))
	   (property-to-be-redisplayed . ,symbol-for-redisplay)))
	(save-excursion
	  (goto-char pos)
	  (twindrill-render-a-field (point) field-id str))
	(goto-char pos)))
     ((twindrill-replied-statuses-visible-p)
      ;; All ancestor replied statuses have been rendered.
      (twindrill-hide-replied-statuses))
     (t
      ;; The pointed status is not a reply.
      (message "This status is not a reply.")))))

(defun twindrill-show-replied-statuses (&optional count interactive)
  ""
  (interactive)
  (cond
   ((twindrill-replied-statuses-visible-p)
    (when interactive
      (message "The replied statuses were already showed.")))
   ((twindrill-render-replied-statuses (point) count)
    t)
   (t
    ;; Failed to render replied statuses.
    (when interactive
      (let ((base-id (twindrill-get-id-at)))
	(if (twindrill-have-replied-statuses-p base-id)
	    (message "The status this replies to has not been fetched yet.")
	  (message "This status is not a reply.")))))))

(defun twindrill-hide-replied-statuses (&optional interactive)
  ""
  (interactive)
  (cond
   ((twindrill-replied-statuses-visible-p)
    (let* ((pos (twindrill-get-current-status-head (point)))
	   (base-id (or (twindrill-get-base-id-of-ancestor-at pos)
			(twindrill-get-id-at pos)))
	   (pointing-to-base-status
	    (not (twindrill-rendered-as-ancestor-status-p pos)))
	   (beg (twindrill-get-beginning-of-visible-replied-statuses pos))
	   (end (twindrill-get-end-of-visible-replied-statuses pos))
	   (buffer-read-only nil))
      (unless pointing-to-base-status
	(goto-char (if twindrill-reverse-mode
		       beg
		     (or (twindrill-get-previous-status-head beg)
			 (point-min)))))
      (delete-region beg end)))
   (interactive
    (message "The status this replies to was already hidden."))))

(defun twindrill-toggle-show-replied-statuses ()
  ""
  (interactive)
  (if (twindrill-replied-statuses-visible-p)
      (twindrill-hide-replied-statuses (interactive-p))
    (twindrill-show-replied-statuses twindrill-show-replied-tweets
				      (interactive-p))))

(provide 'twindrill-render)
;;; twindrill-render.el ends here
