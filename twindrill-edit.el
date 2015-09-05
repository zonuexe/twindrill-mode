;;; twindrill-edit.el --- Major mode for editing tweet

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
;; Package-Requires: ((emacs "24") (dash "2.9.0") (s "1.9.0"))


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

(eval-when-compile (require 'cl))
(require 'twindrill-util)

;;;;
;;;; Edit mode skeleton
;;;;

(defcustom twindrill-edit-skeleton-footer ""
  "*String to be used as the footer in the edit skeleton."
  :group 'twindrill-mode
  :type 'string)

(defvar twindrill-edit-skeleton-footer-history nil)

(defcustom twindrill-edit-skeleton-alist
  '((none . nil)
    (footer . ((nil _ twindrill-edit-skeleton-footer)))
    (footer-only-normal
     . ((nil _ twindrill-edit-skeleton-footer) . normal))
    (inherit-hashtags
     . [(twindrill-edit-skeleton-inherit-hashtags . normal)
	(twindrill-edit-skeleton-inherit-hashtags . reply)])
    (inherit-mentions
     . (twindrill-edit-skeleton-inherit-mentions . reply))
    (inherit-any
     . [(twindrill-edit-skeleton-inherit-mentions . reply)
	(twindrill-edit-skeleton-inherit-hashtags . normal)
	(twindrill-edit-skeleton-inherit-hashtags . reply)]))
  "*Alist of skeletons performed on `twindrill-update-status-interactive'.
A key of the alist is a symbol and each value is nil, (SKELETON . PRED),
 (FUNC . PRED) or a vector of them.

When invoking `twindrill-update-status-interactive', the value corresponding
to the key specified `twindrill-edit-skeleton' are performed.

The value like (SKELETON . PRED) or (FUNC . PRED) is performed when the
current context matches with PRED.
PRED is nil, a symbol or a function.
If PRED is nil, the value is unconditionally performed.
If PRED is a symbol, the value is performed only when it equals to the
type of the tweet being edited. The type is one of 'direct-message, 'normal,
'organic-retweet and 'reply.
If PRED is a function, the value is performed only when the predicate
function PRED returns non-nil. PRED is invoked with three arguments
TWEET-TYPE, IN-REPLY-TO-ID and CURRENT-SPEC.
TWEET-TYPE is a symbol, which is one of 'direct-message, 'normal,
'organic-retweet and 'reply, specifying which type of tweet will be edited.
If the tweet will be edited as a reply or an organic retweet, IN-REPLY-TO-ID
is a string specifying the replied tweet. Otherwise, IN-REPLY-TO-ID is nil.
CURRENT-SPEC specifies where the action of posting a tweet is performed.
If the action is performed on a twindrill-mode buffer, CURRENT-SPEC is
a timeline spec string of the buffer.
If the action is performed on other buffers, CURRENT-SPEC is nil.
If the option IGNORE-CURRENT-SPEC for `twindrill-update-status' is non-nil,
CURRENT-SPEC is also nil.

If PRED matches the current context, the value is performed as follows.
The value like (SKELETON . PRED) is performed by directly using SKELETON as an
argument of `skeleton-insert'.
The value like (FUNC . PRED) is performed by invoking FUNC with three
arguments, TWEET-TYPE, IN-REPLY-TO-ID and CURRENT-SPEC as same as PRED.

If the value is a vector, each element is performed in order of elements
in the vector.

Note that the effective skeleton is invoked after inserting a
recipient."
  :group 'twindrill-mode
  :type 'alist)

(defcustom twindrill-edit-skeleton 'none
  "*A symbol specifying an effective skeleton.

The list of valid value is defined in `twindrill-edit-skeleton-alist'.
To be valid, an entry should be added to `twindrill-edit-skeleton-alist'
first.

When entering `twindrill-edit-mode', the skeletons in the specified
entry in `twindrill-edit-skeleton-alist' are performed."
  :group 'twindrill-mode
  :type (if (> (length (mapcar #'car twindrill-edit-skeleton-alist)) 0)
	    `(choice ,@(mapcar (lambda (entry) `(const ,(car entry)))
			       twindrill-edit-skeleton-alist))
	  'symbol))

(defun twindrill-switch-edit-skeleton ()
  (interactive)
  (let ((skeleton-keys
	 (mapcar (lambda (entry) (symbol-name (car entry)))
		 twindrill-edit-skeleton-alist))
	(current (symbol-name (or twindrill-edit-skeleton 'none))))
    (let ((selected
	   (twindrill-completing-read
	    (format "Skeleton (%s): " current)
	    skeleton-keys nil t nil nil current)))
      (when selected
	(setq twindrill-edit-skeleton (intern selected)))))
  (when (null twindrill-edit-skeleton)
    (setq twindrill-edit-skeleton 'none))
  (message "Current skeleton: %s" twindrill-edit-skeleton))

(defun twindrill-edit-skeleton-change-footer (&optional footer-str)
  (interactive)
  (let ((footer-str
	 (or footer-str
	     (read-from-minibuffer "Footer: " twindrill-edit-skeleton-footer
				   nil nil
				   'twindrill-edit-skeleton-footer-history))))
    (when footer-str
      (setq twindrill-edit-skeleton-footer footer-str)))
  (message "Current footer: [%s]" twindrill-edit-skeleton-footer))

(defun twindrill-edit-skeleton-insert-base (&optional tweet-type in-reply-to-id current-spec)
  (let ((entry
	 (cdr (assq twindrill-edit-skeleton twindrill-edit-skeleton-alist))))
    (when entry
      (require 'skeleton)
      (let ((skeletons (if (vectorp entry)
			   entry
			 (list entry))))
	(mapcar (lambda (def)
		  (let ((skeleton-or-func (car def))
			(pred (cdr def)))
		    (when (or (null pred)
			      (and (functionp pred)
				   (funcall pred tweet-type in-reply-to-id))
			      (and (symbolp pred)
				   (eq pred tweet-type)))
		      (cond
		       ((functionp skeleton-or-func)
			(funcall skeleton-or-func tweet-type in-reply-to-id
				 current-spec))
		       (t
			(skeleton-insert skeleton-or-func))))))
		skeletons)))))

(defun twindrill-edit-skeleton-insert (&optional tweet-type in-reply-to-id current-spec)
  (if (> 22 emacs-major-version)
      ;; This prevents Emacs21 from inserting skeletons before the cursor.
      (let ((current (point))
	    (pair (with-temp-buffer
		    (twindrill-edit-skeleton-insert-base tweet-type
							  in-reply-to-id
							  current-spec)
		    `(,(buffer-string) . ,(point)))))
	(insert (car pair))
	(goto-char (+ -1 current (cdr pair))))
    (twindrill-edit-skeleton-insert-base tweet-type in-reply-to-id
					  current-spec)))

(defun twindrill-edit-skeleton-inherit-hashtags (tweet-type in-reply-to-id current-spec)
  (cond
   (in-reply-to-id
    (let* ((status (twindrill-find-status in-reply-to-id))
	   (text (cdr (assq 'text status)))
	   (hashtags
	    (twindrill-extract-matched-substring-all
	     (concat twindrill-regexp-hash
		     "\\([[:alpha:]0-9_-]+\\)")
	     text))
	   (footer
	    (mapconcat (lambda (tag) (concat "#" tag))
		       hashtags " ")))
      (when hashtags
	(skeleton-insert `(nil _ " " ,footer)))))
   ((twindrill-timeline-spec-is-search-p current-spec)
    (let* ((query-string
	    (twindrill-extract-query-string-from-search-timeline-spec
	     current-spec))
	   (hashtag-list
	    (twindrill-extract-matched-substring-all
	     (concat "\\(" twindrill-regexp-hash "[[:alpha:]0-9_-]+\\)")
	     query-string)))
      (when hashtag-list
	(let ((footer (mapconcat 'identity hashtag-list " ")))
	  (skeleton-insert `(nil _ " " ,footer))))))))

(defun twindrill-edit-skeleton-inherit-mentions (tweet-type in-reply-to-id current-spec)
  (when in-reply-to-id
    (let* ((status (twindrill-find-status in-reply-to-id))
	   (text (cdr (assq 'text status)))
	   (recipient (cdr (assq 'user-screen-name status)))
	   (mentions
	    (twindrill-extract-matched-substring-all
	     (concat twindrill-regexp-atmark
		     "\\([a-zA-Z0-9_-]+\\)")
	     text))
	   (reduced-mentions
	    (remove nil
		    (mapcar
		     (lambda (mention)
		       (unless (or (string= mention recipient)
				   (string= mention (twindrill-get-username)))
			 mention))
		     mentions))))
      (when reduced-mentions
	(let ((header (mapconcat (lambda (user) (concat "@" user))
				 reduced-mentions " ")))
	  (skeleton-insert `(nil ,header " " _)))))))

;;;;
;;;; Edit mode
;;;;

(defvar twindrill-edit-buffer "*twindrill-edit*")
(defvar twindrill-pre-edit-window-configuration nil)
(defvar twindrill-edit-history nil)
(defvar twindrill-edit-local-history nil)
(defvar twindrill-edit-local-history-idx nil)
(defvar twindrill-warning-overlay nil)

(define-derived-mode twindrill-edit-mode nil "twmode-status-edit"
  (use-local-map twindrill-edit-mode-map)

  ;; Prevent `global-font-lock-mode' enabling `font-lock-mode'.
  ;; This technique is derived from `lisp/bs.el' distributed with Emacs 22.2.
  (make-local-variable 'font-lock-global-modes)
  (setq font-lock-global-modes '(not twindrill-edit-mode))

  (make-local-variable 'twindrill-warning-overlay)
  (setq twindrill-warning-overlay (make-overlay 1 1 nil nil nil))
  (overlay-put twindrill-warning-overlay 'face 'font-lock-warning-face)

  (make-local-variable 'twindrill-edit-local-history)
  (setq twindrill-edit-local-history (cons (buffer-string)
					    twindrill-edit-history))
  (make-local-variable 'twindrill-edit-local-history-idx)
  (setq twindrill-edit-local-history-idx 0)

  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 'twindrill-edit-length-check)
  )

(when twindrill-edit-mode-map
  (let ((km twindrill-edit-mode-map))
    (define-key km (kbd "C-c C-c") 'twindrill-edit-post-status)
    (define-key km (kbd "C-c C-k") 'twindrill-edit-cancel-status)
    (define-key km (kbd "C-c C-r") 'twindrill-edit-toggle-reply)
    (define-key km (kbd "M-n") 'twindrill-edit-next-history)
    (define-key km (kbd "M-p") 'twindrill-edit-previous-history)))

(defun twindrill-effective-length (str &optional short-length-http short-length-https)
  "Return the effective length of STR with taking account of shortening URIs.

The returned length is calculated with taking account of shortening URIs
if `twindrill-service-method' is the symbol `twitter' or `twitter-api-v1.1'.
It is assumed that a URI via HTTP will be converted into a URI consisting of
SHORT-LENGTH-HTTP characters.
It is assumed that a URI via HTTPS will be converted into a URI consisting of
SHORT-LENGTH-HTTPS characters.

If SHORT-LENGTH-HTTP is nil, the value of
 (twindrill-get-service-configuration 'short_url_length) is used instead.
If SHORT-LENGTH-HTTPS is nil, the value of
 (twindrill-get-service-configuration 'short_url_length_https) is used
instead."
  (cond
   ((memq twindrill-service-method '(twitter twitter-api-v1.1))
    (let ((regexp "\\(?:^\\|[[:space:]]\\)\\(http\\(s\\)?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)")
	  (short-length-http
	   (or short-length-http
	       (twindrill-get-service-configuration 'short_url_length)))
	  (short-length-https
	   (or short-length-https
	       (twindrill-get-service-configuration 'short_url_length_https)))
	  (rest str)
	  (pos 0)
	  (len 0))
      (save-match-data
	(while (string-match regexp str pos)
	  (let ((beg (match-beginning 1))
		(end (match-end 1))
		(short-len (if (match-beginning 2)
			       short-length-https
			     short-length-http)))
	    (let ((additional-length
		   ;; Ignore the original length to follow the change
		   ;; of t.co URL wrapper.
		   ;;
		   ;; https://dev.twitter.com/docs/tco-url-wrapper
		   ;; As of October 10, 2011 the t.co URL wrapper
		   ;; automatically wraps all links submitted to
		   ;; Twitter, regardless of length. This includes
		   ;; so-called URLs without protocols.
		   (+ (- beg pos) short-len)))
	      (setq len (+ len additional-length))
	      (setq pos end)))))
      (+ len (- (length str) pos))))
   (t
    (length str))))

(defun twindrill-edit-length-check (&optional beg end len)
  (let* ((status (twindrill-edit-extract-status))
	 (maxlen 140)
	 (length (twindrill-effective-length status)))
    (setq mode-name
	  (format "twmode-status-edit[%d/%d]" length maxlen))
    (force-mode-line-update)
    (unless twindrill-disable-overlay-on-too-long-string
      (if (< maxlen length)
	  (move-overlay twindrill-warning-overlay
			(- (point-max) (- length maxlen)) (point-max))
	(move-overlay twindrill-warning-overlay 1 1)))))

(defun twindrill-edit-get-help-end ()
  "Return the end position of the help on `twindrill-edit-mode'."
  (when (eq major-mode 'twindrill-edit-mode)
    (next-single-property-change (point-min) 'read-only nil (point-max))))

(defun twindrill-edit-extract-status ()
  "Return the text of the status being edited on `twindrill-edit-mode'."
  (if (eq major-mode 'twindrill-edit-mode)
      (buffer-substring-no-properties (twindrill-edit-get-help-end)
				      (point-max))
    ""))

(defun twindrill-edit-reset-status (str)
  "Reset the contents of the current `twindrill-edit-mode' buffer with STR."
  (when (eq major-mode 'twindrill-edit-mode)
    (let ((help-end (twindrill-edit-get-help-end)))
      (delete-region help-end (point-max))
      (goto-char help-end)
      (insert str)
      (goto-char help-end))))

(defun twindrill-edit-set-help-string (str)
  "Render STR as a help for `twindrill-edit-mode' to the current buffer."
  (let* ((help-str (propertize str 'read-only t))
	 (len (length help-str)))
    (add-text-properties 0 1 '(front-sticky (read-only)) help-str)
    (add-text-properties (1- len) len '(rear-nonsticky t) help-str)
    (save-excursion
      (let ((inhibit-read-only t)
	    (inhibit-modification-hooks t)
	    (help-end (twindrill-edit-get-help-end)))
	(goto-char help-end)
	(if (= (point-min) help-end)
	    ;; When no helps are rendered, the all markers should be
	    ;; placed after the new help.
	    (insert help-str)
	  ;; Use `insert-before-markers' because the marker of the current
	  ;; position should follow the new help.
	  ;; Delete the old help after inserting the new help to make
	  ;; the new help visible if possible.
	  (insert-before-markers help-str)
	  (delete-region (point-min) help-end))))))

(defun twindrill-edit-setup-help ()
  (let* ((direct-message-recipient
	  (cdr (assq 'direct-message-recipient twindrill-edit-mode-info)))
	 (tweet-type (cdr (assq 'tweet-type twindrill-edit-mode-info)))
	 (cited-id (cdr (assq 'cited-id twindrill-edit-mode-info)))
	 (item (cond
		((eq tweet-type 'direct-message)
		 (format "a direct message to %s" direct-message-recipient))
		((eq tweet-type 'reply)
		 "a reply")
		(t
		 "a tweet")))
	 (status-format
	  (cond
	   ((eq tweet-type 'direct-message)
	    (format "%%FILL{DIRECT MESSAGE to %s}\n" direct-message-recipient))
	   ((eq tweet-type 'reply)
	    "%FILL{REPLY to the tweet by %s at %C{%y/%m/%d %H:%M:%S};}\n%FILL{%FACE[font-lock-doc-face]{\"%T\"}}\n")
	   (t
	    nil)))
	 (func (when status-format
		 (twindrill-generate-format-status-function status-format)))
	 (help-str
	  (apply 'concat
		 `(,@(when func
		       `(,(funcall func
				   (twindrill-find-status cited-id)
				   nil)))
		   ,(propertize (format (substitute-command-keys "Keymap:
  \\[twindrill-edit-post-status]: send %s
  \\[twindrill-edit-cancel-status]: cancel %s
  \\[twindrill-edit-toggle-reply]: toggle a normal tweet and a reply.
  \\[twindrill-edit-next-history]: next history element
  \\[twindrill-edit-previous-history]: previous history element

---- text above this line is ignored ----
") item item)
				'face 'font-lock-comment-face)))))
    (twindrill-edit-set-help-string help-str)))

(defun twindrill-edit-close ()
  (kill-buffer (current-buffer))
  (when twindrill-pre-edit-window-configuration
    (set-window-configuration twindrill-pre-edit-window-configuration)
    (setq twindrill-pre-edit-window-configuration nil)))

(defvar twindrill-edit-mode-info nil
  "Alist of a tweet being edited.
Pairs of a key symbol and an associated value are following:
  direct-message-recipient -- the recipient when the edited message is
    sent as a direct message.
  cited-id -- the id of the tweet that the edited message refers to.
  tweet-type -- the type of the edited message, which is one of the
    following symbol; normal, reply or direct-message.")

(defun twindrill-ensure-whole-of-status-is-visible (&optional window)
  "Ensure that the whole of the tweet on the current point is visible."
  (interactive)
  (let* ((window (or window (selected-window)))
	 (buffer (window-buffer window)))
    (when (twindrill-buffer-p buffer)
      (with-current-buffer buffer
	(save-excursion
	  (let* ((next-head (or (twindrill-get-next-status-head) (point-max)))
		 (current-tail (max (1- next-head) (point-min))))
	    (when (< (window-end window t) current-tail)
	      (twindrill-set-window-end window current-tail))))))))

(defun twindrill-update-status-from-pop-up-buffer (&optional init-string-or-skeleton reply-to-id username tweet-type current-spec)
  (interactive)
  (let ((buf (generate-new-buffer twindrill-edit-buffer)))
    (setq twindrill-pre-edit-window-configuration
	  (current-window-configuration))
    (twindrill-pop-to-buffer buf)
    (twindrill-edit-mode)
    (make-local-variable 'twindrill-edit-mode-info)
    (setq twindrill-edit-mode-info
	  `((cited-id . ,reply-to-id)
	    (tweet-type . ,(cdr (assq tweet-type
				      '((direct-message . direct-message)
					(normal . normal)
					(organic-retweet . normal)
					(reply . reply)))))
	    (direct-message-recipient . ,username)))
    (twindrill-edit-setup-help)
    (setq buffer-undo-list nil)
    (goto-char (twindrill-edit-get-help-end))
    (if (eq tweet-type 'direct-message)
	(message "C-c C-c to send, C-c C-k to cancel")
      (and (null init-string-or-skeleton)
	   twindrill-current-hashtag
	   (setq init-string-or-skeleton
		 (format " #%s " twindrill-current-hashtag)))
      (message "C-c C-c to post, C-c C-k to cancel"))
    (when init-string-or-skeleton
      (require 'skeleton)
      (cond
       ((stringp init-string-or-skeleton)
	(insert init-string-or-skeleton))
       ((listp init-string-or-skeleton)
	(skeleton-insert init-string-or-skeleton))))
    (twindrill-edit-skeleton-insert tweet-type reply-to-id
				     current-spec)
    (set-buffer-modified-p nil)))

(defun twindrill-edit-post-status ()
  (interactive)
  (let* ((status (twindrill-edit-extract-status))
	 (cited-id (cdr (assq 'cited-id twindrill-edit-mode-info)))
	 (cited-tweet (twindrill-find-status cited-id))
	 (cited-username (cdr (assq 'user-screen-name cited-tweet)))
	 (direct-message-recipient
	  (cdr (assq 'direct-message-recipient twindrill-edit-mode-info)))
	 (tweet-type (cdr (assq 'tweet-type twindrill-edit-mode-info))))
    (cond
     ((string-match "\\` *\\'" status)
      (message "Empty tweet!"))
     ((< 140 (twindrill-effective-length status))
      (message "Tweet is too long!"))
     ((cond
       ((and (eq tweet-type 'reply)
	     (not (string-match
		   (concat "@" cited-username "\\(?:[\n\r \t]+\\)*") status)))
	(y-or-n-p
	 "Send this tweet without mentions as a normal tweet (not a reply)? "))
       (twindrill-request-confirmation-on-posting
	(y-or-n-p "Send this tweet? "))
       (t
	t))
      (setq twindrill-edit-history
	    (cons status twindrill-edit-history))
      (cond
       ((eq tweet-type 'direct-message)
	(if direct-message-recipient
	    (twindrill-call-api 'send-direct-message
				 `((username . ,direct-message-recipient)
				   (status . ,status)))
	  (message "No username specified")))
       ((eq tweet-type 'reply)
	(twindrill-call-api 'update-status
			     `((status . ,status)
			       (in-reply-to-status-id . ,cited-id))))
       (t
	(twindrill-call-api 'update-status `((status . ,status)))))
      (twindrill-edit-close))
     (t
      nil))))

(defun twindrill-edit-cancel-status ()
  (interactive)
  (when (or (not (buffer-modified-p))
	    (prog1 (if (y-or-n-p "Cancel this tweet? ")
		       (message "Request canceled")
		     (message nil))))
    (twindrill-edit-close)))

(defun twindrill-edit-next-history ()
  (interactive)
  (if (>= 0 twindrill-edit-local-history-idx)
      (message "End of history.")
    (let ((current-history (nthcdr twindrill-edit-local-history-idx
				   twindrill-edit-local-history)))
      (setcar current-history (twindrill-edit-extract-status))
      (decf twindrill-edit-local-history-idx)
      (twindrill-edit-reset-status (nth twindrill-edit-local-history-idx
					 twindrill-edit-local-history)))))

(defun twindrill-edit-previous-history ()
  (interactive)
  (if (>= twindrill-edit-local-history-idx
	  (- (length twindrill-edit-local-history) 1))
      (message "Beginning of history.")
    (let ((current-history (nthcdr twindrill-edit-local-history-idx
				   twindrill-edit-local-history)))
      (setcar current-history (twindrill-edit-extract-status))
      (incf twindrill-edit-local-history-idx)
      (twindrill-edit-reset-status (nth twindrill-edit-local-history-idx
					 twindrill-edit-local-history)))))

(defun twindrill-edit-toggle-reply ()
  "Toggle whether the tweet being edited will be sent as a reply or not."
  (interactive)
  (let ((tweet-type (cdr (assq 'tweet-type twindrill-edit-mode-info)))
	(cited-id (cdr (assq 'cited-id twindrill-edit-mode-info))))
    (cond
     ((eq tweet-type 'direct-message)
      (message "The current message is a direct message."))
     ((null cited-id)
      (message "The current message does not have a reply target."))
     (t
      (setq twindrill-edit-mode-info
	    (mapcar (lambda (entry)
		      (if (eq (car entry) 'tweet-type)
			  `(tweet-type
			    . ,(cdr (assq (cdr entry)
					  '((normal . reply)
					    (reply . normal)))))
			entry))
		    twindrill-edit-mode-info))
      (twindrill-edit-setup-help)))))

;;;;
;;;; Edit a status on minibuffer
;;;;

(defun twindrill-show-minibuffer-length (&optional beg end len)
  "Show the number of characters in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (if (and transient-mark-mode deactivate-mark)
	(deactivate-mark))
    (let* ((deactivate-mark deactivate-mark)
	   (status-len (- (twindrill-effective-length (buffer-string))
			  (minibuffer-prompt-width)))
	   (mes (format "%d" status-len)))
      (if (<= 23 emacs-major-version)
	  (minibuffer-message mes) ;; Emacs23 or later
	(minibuffer-message (concat " (" mes ")")))
      )))

(defun twindrill-setup-minibuffer ()
  (add-hook 'post-command-hook 'twindrill-show-minibuffer-length t t))

(defun twindrill-finish-minibuffer ()
  (remove-hook 'post-command-hook 'twindrill-show-minibuffer-length t))

(defun twindrill-status-not-blank-p (status)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (with-temp-buffer
      (insert status)
      (goto-char (point-min))
      ;; skip user name
      (re-search-forward "\\`[[:space:]]*@[a-zA-Z0-9_-]+\\([[:space:]]+@[a-zA-Z0-9_-]+\\)*" nil t)
      (re-search-forward "[^[:space:]]" nil t))))

(defun twindrill-update-status-from-minibuffer (&optional init-string-or-skeleton reply-to-id username tweet-type current-spec)
  (and (not (eq tweet-type 'direct-message))
       (null init-string-or-skeleton)
       twindrill-current-hashtag
       (setq init-string-or-skeleton
	     (format " #%s " twindrill-current-hashtag)))
  (let ((status
	 (with-temp-buffer
	   (when init-string-or-skeleton
	     (require 'skeleton)
	     (cond
	      ((stringp init-string-or-skeleton)
	       (insert init-string-or-skeleton))
	      ((listp init-string-or-skeleton)
	       (skeleton-insert init-string-or-skeleton))))
	   (twindrill-edit-skeleton-insert tweet-type reply-to-id
					    current-spec)
	   `(,(buffer-string) . ,(point))))
	(not-posted-p t)
	(prompt "status: ")
	(map minibuffer-local-map)
	(minibuffer-message-timeout nil))
    (when twindrill-use-show-minibuffer-length
      (add-hook 'minibuffer-setup-hook 'twindrill-setup-minibuffer t)
      (add-hook 'minibuffer-exit-hook 'twindrill-finish-minibuffer t))
    (unwind-protect
	(while not-posted-p
	  (setq status (read-from-minibuffer prompt status map nil 'twindrill-tweet-history nil t))
	  (let ((status status))
	    (if (< 140 (twindrill-effective-length status))
		(setq prompt "status (too long): ")
	      (setq prompt "status: ")
	      (when (twindrill-status-not-blank-p status)
		(cond
		 ((eq tweet-type 'direct-message)
		  (if username
		      (twindrill-call-api 'send-direct-message
					   `((username . ,username)
					     (status . ,status)))
		    (message "No username specified")))
		 (t
		  (let ((parameters `(("status" . ,status)))
			(as-reply
			 (and reply-to-id
			      username
			      (eq tweet-type 'reply)
			      (string-match
			       (concat "@" username "\\(?:[\n\r \t]+\\)*")
			       status))))
		    ;; Add in_reply_to_status_id only when a posting
		    ;; status begins with @username.
		    (twindrill-call-api
		     'update-status
		     `((status . ,status)
		       ,@(when as-reply
			   `((in-reply-to-status-id
			      . ,(format "%s" reply-to-id))))))
		    )))
		(setq not-posted-p nil))
	      )))
      ;; unwindforms
      (when (memq 'twindrill-setup-minibuffer minibuffer-setup-hook)
	(remove-hook 'minibuffer-setup-hook 'twindrill-setup-minibuffer))
      (when (memq 'twindrill-finish-minibuffer minibuffer-exit-hook)
	(remove-hook 'minibuffer-exit-hook 'twindrill-finish-minibuffer))
      )))

;;;;
;;;; Reading username/listname with completion
;;;;

(defun twindrill-get-usernames-from-timeline (&optional timeline-data)
  (let ((timeline-data (or timeline-data (twindrill-current-timeline-data))))
    (twindrill-remove-duplicates
     (mapcar
      (lambda (status)
	(let* ((base-str (cdr (assq 'user-screen-name status)))
	       ;; `copied-str' is independent of the string in timeline-data.
	       ;; This isolation is required for `minibuf-isearch.el',
	       ;; which removes the text properties of strings in history.
	       (copied-str (copy-sequence base-str)))
	  (set-text-properties 0 (length copied-str) nil copied-str)
	  copied-str))
      timeline-data))))

(defun twindrill-read-username-with-completion (prompt init-user &optional history)
  (let ((collection (append twindrill-user-history
			    (twindrill-get-usernames-from-timeline))))
    (twindrill-completing-read prompt collection nil nil init-user history)))

(defun twindrill-read-list-name (username &optional list-index)
  (let* ((list-index (or list-index
			 (twindrill-get-list-index-sync username)))
	 (username (prog1 (copy-sequence username)
		     (set-text-properties 0 (length username) nil username)))
	 (prompt (format "%s's list: " username))
	 (listname
	  (if list-index
	      (twindrill-completing-read prompt list-index nil t nil)
	    nil)))
    (if (string= "" listname)
	nil
      listname)))

(defun twindrill-read-subscription-list-name (username &optional list-index)
  (let* ((list-index (or list-index
			 (twindrill-get-list-subscriptions-sync username)))
	 (username (prog1 (copy-sequence username)
		     (set-text-properties 0 (length username) nil username)))
	 (prompt (format "%s's subscription: " username))
	 (listname
	  (if list-index
	      (twindrill-completing-read prompt list-index nil t nil)
	    nil)))
    (if (string= "" listname)
	nil
      listname)))

(defun twindrill-read-timeline-spec-with-completion (prompt initial &optional as-string)
  (let* ((dummy-hist
	  (append twindrill-timeline-history
		  (twindrill-get-usernames-from-timeline)
		  '(":direct_messages" ":direct_messages_sent"
		    ":favorites" ":friends"
		    ":home" ":mentions" ":public" ":replies"
		    ":retweeted_by_me" ":retweeted_by_user/"
		    ":retweeted_to_me" ":retweeted_to_user/"
		    ":retweets_of_me")
		  (mapcar (lambda (cell)
			    (concat "$" (car cell) (if (listp (cdr cell)) "()" "")))
			  twindrill-timeline-spec-alias)))
	 (spec-with-username
	  '((":favorites/" . "Whose favorites: ")
	    (":retweeted_by_user/" . "Who has retweeted? ")
	    (":retweeted_to_user/" . "Who has received the retweets? ")))
	 (regexp-spec-with-username
	  (concat "\\`\\("
		  (mapconcat (lambda (entry) (car entry))
			     spec-with-username "\\|")
		  "\\)\\'"))
	 (spec-string (twindrill-completing-read prompt dummy-hist
						  nil nil initial 'dummy-hist))
	 (spec-string
	  (cond
	   ((string-match regexp-spec-with-username spec-string)
	    (let* ((spec-and-prompt
		    (assoc (match-string 1 spec-string)
			   spec-with-username))
		   (prefix (car spec-and-prompt))
		   (prompt (cdr spec-and-prompt))
		   (username
		    (twindrill-read-username-with-completion
		     prompt ""
		     'twindrill-user-history)))
	      (if username
		  (concat prefix username)
		nil)))
	   ((string-match "^\\([a-zA-Z0-9_-]+\\)/$" spec-string)
	    (let* ((username (match-string 1 spec-string))
		   (list-index (twindrill-get-list-index-sync username))
		   (listname
		    (if list-index
			(twindrill-read-list-name username list-index)
		      nil)))
	      (if listname
		  (concat username "/" listname)
		nil)))
	   (t
	    spec-string)))
	 (spec (if (stringp spec-string)
		   (condition-case error-str
		       (twindrill-string-to-timeline-spec spec-string)
		     (error
		      (message "Invalid timeline spec: %s" error-str)
		      nil))
		 nil)))
    (cond
     ((null spec)
      nil)
     (spec (if as-string
	       spec-string
	     spec))
     ((string= "" spec-string)
      (message "No timeline specs are specified.")
      nil)
     (t
      (message "\"%s\" is invalid as a timeline spec." spec-string)
      nil))))

(provide 'twindrill-edit)
;;; twindrill-edit.el ends here
