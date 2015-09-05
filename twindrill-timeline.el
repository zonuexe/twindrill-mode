;;; twindrill-timeline.el --- Timeline procedures

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

(require 'twindrill-util)

;;;;
;;;; Timeline spec
;;;;

;;; Timeline spec as S-expression
;;; - (user USER): timeline of the user whose name is USER. USER is a string.
;;; - (list USER LIST):
;;;     the list LIST of the user USER. LIST and USER are strings.
;;;
;;; - (direct_messages): received direct messages.
;;; - (direct_messages_sent): sent direct messages.
;;; - (favorites): favorites timeline for the current user.
;;; - (favorites USER): favorites timeline for the specified user.
;;; - (friends): friends timeline.
;;; - (home): home timeline.
;;; - (mentions): mentions timeline.
;;;     mentions (status containing @username) for the authenticating user.
;;; - (public): public timeline.
;;; - (replies): replies.
;;; - (retweeted_by_me): retweets posted by the authenticating user.
;;; - (retweeted_by_user USER): retweets posted by the user.
;;; - (retweeted_to_me): retweets posted by the authenticating user's friends.
;;; - (retweeted_to_user USER): retweets posted to the user.
;;; - (retweets_of_me):
;;;     tweets of the authenticated user that have been retweeted by others.
;;; - (single ID): the single tweet specified by ID.
;;;
;;; - (search STRING): the result of searching with query STRING.
;;;
;;; - (exclude-if FUNC SPEC):
;;;     the same timeline as SPEC, except that it does not include tweets
;;;     that FUNC returns non-nil for.
;;; - (exclude-re REGEXP-STRING SPEC):
;;;     the same timeline as SPEC, except that it does not include tweets
;;;     that matches the regular expression specified by REGEXP-STRING.
;;;
;;; - (merge SPEC1 SPEC2 ...): result of merging timelines SPEC1 SPEC2 ...
;;;

;;; Timeline spec string
;;;
;;; SPEC ::= PRIMARY | COMPOSITE
;;; PRIMARY ::= USER | LIST | DIRECT_MESSSAGES | DIRECT_MESSSAGES_SENT
;;;             | FRIENDS | HOME | MENTIONS | PUBLIC | REPLIES
;;;             | RETWEETED_BY_ME | RETWEETED_BY_USER
;;;             | RETWEETED_TO_ME | RETWEETED_TO_USER | RETWEETS_OF_ME
;;;             | SEARCH
;;; COMPOSITE ::= EXCLUDE-IF | EXCLUDE-RE | MERGE
;;;
;;; USER ::= /[a-zA-Z0-9_-]+/
;;; LIST ::= USER "/" LISTNAME
;;; LISTNAME ::= /[a-zA-Z0-9_-]+/
;;; DIRECT_MESSSAGES ::= ":direct_messages"
;;; DIRECT_MESSSAGES_SENT ::= ":direct_messages_sent"
;;; FAVORITES ::= ":favorites" | ":favorites/" USER
;;; FRIENDS ::= ":friends"
;;; HOME ::= ":home" | "~"
;;; MENTIONS ::= ":mentions"
;;; PUBLIC ::= ":public"
;;; REPLIES ::= ":replies" | "@"
;;; RETWEETED_BY_ME ::= ":retweeted_by_me"
;;; RETWEETED_BY_USER ::= ":retweeted_by_user/" USER
;;; RETWEETED_TO_ME ::= ":retweeted_to_me"
;;; RETWEETED_TO_USER ::= ":retweeted_to_user/" USER
;;; RETWEETS_OF_ME ::= ":retweets_of_me"
;;; SINGLE ::= ":single/" ID
;;; ID ::= /[0-9]+/
;;;
;;; SEARCH ::= ":search/" QUERY_STRING "/"
;;; QUERY_STRING ::= any string, where "/" is escaped by a backslash.
;;;
;;; EXCLUDE-IF ::= ":exclude-if/" FUNC "/" SPEC
;;; FUNC ::= LAMBDA EXPRESSION | SYMBOL
;;; EXCLUDE-RE ::= ":exclude-re/" REGEXP "/" SPEC
;;;
;;; MERGE ::= "(" MERGED_SPECS ")"
;;; MERGED_SPECS ::= SPEC | SPEC "+" MERGED_SPECS
;;;

(defvar twindrill-regexp-hash
  (let ((full-width-number-sign (twindrill-ucs-to-char #xff03)))
    ;; Unicode Character 'FULLWIDTH NUMBER SIGN' (U+FF03)
    (concat "\\(?:#\\|" (char-to-string full-width-number-sign) "\\)")))

(defvar twindrill-regexp-atmark
  (let ((full-width-commercial-at (twindrill-ucs-to-char #xff20)))
    ;; Unicode Character 'FULLWIDTH COMMERCIAL AT' (U+FF20)
    (concat "\\(?:@\\|" (char-to-string full-width-commercial-at) "\\)")))

(defun twindrill-timeline-spec-to-string (timeline-spec &optional shorten)
  "Convert TIMELINE-SPEC into a string.
If SHORTEN is non-nil, the abbreviated expression will be used."
  (let ((type (car timeline-spec))
	(value (cdr timeline-spec)))
    (cond
     ;; user
     ((eq type 'user) (car value))
     ;; list
     ((eq type 'list) (concat (car value) "/" (cadr value)))
     ;; simple
     ((eq type 'direct_messages) ":direct_messages")
     ((eq type 'direct_messages_sent) ":direct_messages_sent")
     ((eq type 'favorites)
      (if value
	  (concat ":favorites/" (car value))
	":favorites"))
     ((eq type 'friends) ":friends")
     ((eq type 'home) (if shorten "~" ":home"))
     ((eq type 'mentions) (if shorten "@" ":mentions"))
     ((eq type 'public) ":public")
     ((eq type 'replies) ":replies")
     ((eq type 'retweeted_by_me) ":retweeted_by_me")
     ((eq type 'retweeted_by_user) (concat ":retweeted_by_user/" (car value)))
     ((eq type 'retweeted_to_me) ":retweeted_to_me")
     ((eq type 'retweeted_to_user) (concat ":retweeted_to_user/" (car value)))
     ((eq type 'retweets_of_me) ":retweets_of_me")
     ((eq type 'single) (concat ":single/" (car value)))
     ((eq type 'search)
      (let ((query (car value)))
	(concat ":search/"
		(replace-regexp-in-string "\\(\\\\\\|/\\)" "\\\\\\1" query)
		"/")))
     ;; composite
     ((eq type 'exclude-if)
      (let ((func (car value))
	    (spec (cadr value))
	    (print-level nil))
	(concat ":exclude-if/" (prin1-to-string func) "/"
		(twindrill-timeline-spec-to-string spec))))
     ((eq type 'exclude-re)
      (let ((regexp-str (car value))
	    (spec (cadr value))
	    (print-level nil))
	(concat ":exclude-re/"
		(replace-regexp-in-string "/" "\\\\\/" regexp-str)
		"/"
		(twindrill-timeline-spec-to-string spec))))
     ((eq type 'merge)
      (concat "("
	      (mapconcat 'twindrill-timeline-spec-to-string value "+")
	      ")"))
     (t
      nil))))

(eval-and-compile
  (defmacro twindrill-make-user-timeline-spec-direct (user)
    `(list 'user ,user))
  (defmacro twindrill-make-list-timeline-spec-direct (owner listname)
    `(list 'list ,owner ,listname))
  (defmacro twindrill-make-hashtag-timeline-spec-direct (tag)
    `(list 'search (concat "#" ,tag)))
  (defmacro twindrill-make-hashtag-timeline-spec-string-direct (tag)
    `(concat "#" ,tag)))

(defun twindrill-extract-timeline-spec (str &optional unresolved-aliases)
  "Extract one timeline spec from STR.
Return cons of the spec and the rest string."
  (cond
   ((null str)
    (error "STR is nil")
    nil)
   ((string-match "^\\([a-zA-Z0-9_-]+\\)/\\([[:word:]_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (listname (match-string 2 str))
	  (rest (substring str (match-end 0))))
      `(,(twindrill-make-list-timeline-spec-direct user listname) . ,rest)))
   ((string-match "^\\([a-zA-Z0-9_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (rest (substring str (match-end 0))))
      `(,(twindrill-make-user-timeline-spec-direct user) . ,rest)))
   ((string-match "^~" str)
    `((home) . ,(substring str (match-end 0))))
   ((string-match (concat "^" twindrill-regexp-atmark) str)
    `((mentions) . ,(substring str (match-end 0))))
   ((string-match (concat "^" twindrill-regexp-hash "\\([[:alpha:]0-9_-]+\\)")
		  str)
    (let* ((tag (match-string 1 str))
	   (rest (substring str (match-end 0))))
      `(,(twindrill-make-hashtag-timeline-spec-direct tag) . ,rest)))
   ((string-match "^:\\([a-z_-]+\\)" str)
    (let ((type (match-string 1 str))
	  (following (substring str (match-end 0)))
	  (alist '(("direct_messages" . direct_messages)
		   ("direct_messages_sent" . direct_messages_sent)
		   ("friends" . friends)
		   ("home" . home)
		   ("mentions" . mentions)
		   ("public" . public)
		   ("replies" . replies)
		   ("retweeted_by_me" . retweeted_by_me)
		   ("retweeted_to_me" . retweeted_to_me)
		   ("retweets_of_me" . retweets_of_me))))
      (cond
       ((assoc type alist)
	(let ((first-spec (list (cdr (assoc type alist)))))
	  (cons first-spec following)))
       ((string= type "favorites")
	(if (string-match "^:favorites/\\([a-zA-Z0-9_-]+\\)" str)
	    (let ((rest (substring str (match-end 0))))
	      `((favorites ,(match-string 1 str)) . ,rest))
	  `((favorites) . ,following)))
       ((string-match "^:retweeted_by_user/\\([a-zA-Z0-9_-]+\\)" str)
	(let ((user (match-string 1 str))
	      (rest (substring str (match-end 0))))
	  `((retweeted_by_user ,user) . ,rest)))
       ((string-match "^:retweeted_to_user/\\([a-zA-Z0-9_-]+\\)" str)
	(let ((user (match-string 1 str))
	      (rest (substring str (match-end 0))))
	  `((retweeted_to_user ,user) . ,rest)))
       ((string-match "^:single/\\([0-9]+\\)" str)
	(let ((id (match-string 1 str))
	      (rest (substring str (match-end 0))))
	  `((single ,id) . ,rest)))
       ((string= type "search")
	(if (string-match "^:search/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/"
			  str)
	    (let* ((escaped-query (or (match-string 1 str) ""))
		   (query (replace-regexp-in-string "\\\\\\(\\\\\\|/\\)" "\\1"
						    escaped-query))
		   (rest (substring str (match-end 0))))
	      (if (not (string= "" escaped-query))
		  `((search ,query) . ,rest)
		(error "\"%s\" has no valid regexp" str)
		nil))))
       ((string= type "exclude-if")
	(let ((result-pair
	       (cond
		((string-match "^:exclude-if/\\([^(/]+\\)/" str)
		 `(,(intern (match-string 1 str)) . ,(match-end 1)))
		((string-match "^:exclude-if/" str)
		 (condition-case err
		     (read-from-string str (match-end 0))
		   (error
		    nil))))))
	  (if result-pair
	      (let ((func (car result-pair))
		    (pos (cdr result-pair)))
		(cond
		 ((not (functionp func))
		  (error "\"%s\" has an invalid function" str)
		  nil)
		 ((<= (length str) (1+ pos))
		  (error "\"%s\" has no timeline spec" str)
		  nil)
		 ((not (char-equal ?/ (aref str pos)))
		  (error "\"%s\" has no delimiter" str)
		  nil)
		 (t
		  (let* ((pair (twindrill-extract-timeline-spec
				(substring str (1+ pos)) unresolved-aliases))
			 (spec (car pair))
			 (rest (cdr pair)))
		    `((exclude-if ,func ,spec) . ,rest)))))
	    (error "\"%s\" has an invalid function" str)
	    nil)))
       ((string= type "exclude-re")
	(cond
	 ((string-match "^:exclude-re/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/"
			str)
	  (let* ((escaped-regexp (or (match-string 1 str) ""))
		 (regexp
		  (replace-regexp-in-string "\\\\/" "/" escaped-regexp nil t))
		 (following (substring str (match-end 0))))
	    (cond
	     ((string= "" escaped-regexp)
	      (error "\"%s\" has no valid regexp" str)
	      nil)
	     (t
	      (let* ((pair (twindrill-extract-timeline-spec
			    following unresolved-aliases))
		     (spec (car pair))
		     (rest (cdr pair)))
		`((exclude-re ,regexp ,spec) . ,rest))))))
	 (t
	  (error "\"%s\" has no valid regexp" str)
	  nil)))
       (t
	(error "\"%s\" is invalid as a timeline spec" str)
	nil))))
   ((string-match "^\\$\\([a-zA-Z0-9_-]+\\)\\(?:(\\([^)]*\\))\\)?" str)
    (let* ((name (match-string 1 str))
	   (rest (substring str (match-end 0)))
	   (value (cdr-safe (assoc name twindrill-timeline-spec-alias)))
	   (arg (match-string 2 str)))
      (if (member name unresolved-aliases)
	  (error "Alias \"%s\" includes a recursive reference" name)
	(cond
	 ((stringp value)
	  (twindrill-extract-timeline-spec
	   (concat value rest)
	   (cons name unresolved-aliases)))
	 ((functionp value)
	  (twindrill-extract-timeline-spec
	   (funcall value arg)
	   (cons name unresolved-aliases)))
	 (t
	  (error "Alias \"%s\" is undefined" name))))))
   ((string-match "^(" str)
    (let ((rest (concat "+" (substring str (match-end 0))))
	  (result '()))
      (while (and rest (string-match "^\\+" rest))
	(let* ((spec-string (substring rest (match-end 0)))
	       (pair (twindrill-extract-timeline-spec
		      spec-string unresolved-aliases))
	       (spec (car pair))
	       (next-rest (cdr pair)))
	  (setq result (cons spec result))
	  (setq rest next-rest)))
      (if (and rest (string-match "^)" rest))
	  (let ((spec-list
		 (twindrill-remove-duplicates
		  (apply 'append
			 (mapcar (lambda (x) (if (eq 'merge (car x))
						 (cdr x)
					       (list x)))
				 (reverse result))))))
	    (if (= 1 (length spec-list))
		`(,(car spec-list) . ,(substring rest 1))
	      `((merge ,@spec-list) . ,(substring rest 1))))
	(if rest
	    ;; The string following the opening parenthesis `('
	    ;; can be interpreted without errors,
	    ;; but there is no corresponding closing parenthesis.
	    (error "\"%s\" lacks a closing parenthesis" str))
	;; Does not display additional error messages if an error
	;; occurred on interpreting the string following
	;; the opening parenthesis `('.
	nil)))
   (t
    (error "\"%s\" is invalid as a timeline spec" str)
    nil)
   ))

(defun twindrill-string-to-timeline-spec (spec-str &optional noerror)
  "Convert SPEC-STR into a timeline spec.
If SPEC-STR is invalid as a timeline spec string, raise an error or return
nil if NOERROR is non-nil."
  (let ((result-pair
	 (condition-case err
	     (twindrill-extract-timeline-spec spec-str)
	   (error
	    (if noerror
		nil
	      (signal (car err) (cdr err))
	      nil)))))
    (if (and result-pair (string= "" (cdr result-pair)))
	(car result-pair)
      nil)))

(defun twindrill-timeline-spec-primary-p (spec)
  "Return non-nil if SPEC is a primary timeline spec.
`primary' means that the spec is not a composite timeline spec such as
`merge'."
  (let ((primary-spec-types
	 '(user list
		direct_messages direct_messages_sent
		favorites friends home mentions public replies
		search
		retweeted_by_me retweeted_by_user
		retweeted_to_me retweeted_to_user
		retweets_of_me
		single))
	(type (car spec)))
    (memq type primary-spec-types)))

(defun twindrill-timeline-spec-composite-p (spec)
  "Return non-nil if SPEC is a composite timeline spec.
`composite' means that the spec depends on other timelines."
  (let ((composite-spec-types
	 '(exclude-if exclude-re merge))
	(type (car spec)))
    (memq type composite-spec-types)))

(defun twindrill-timeline-spec-depending-on-p (spec base-spec)
  "Return non-nil if SPEC depends on BASE-SPEC."
  (cond
   ((twindrill-timeline-spec-primary-p spec)
    (equal spec base-spec))
   ((equal spec base-spec)
    t)
   (t
    (remove
     nil
     (mapcar
      (lambda (direct-base-spec)
	(twindrill-timeline-spec-depending-on-p direct-base-spec base-spec))
      (twindrill-get-base-timeline-specs spec))))))

(defun twindrill-timeline-spec-is-user-p (spec)
  "Return non-nil if SPEC is a user timeline."
  (and (consp spec) (eq 'user (car spec))))

(defun twindrill-timeline-spec-is-direct-messages-p (spec)
  "Return non-nil if SPEC is a timeline spec which is related of
direct_messages."
  (and spec
       (memq (car spec) '(direct_messages direct_messages_sent))))

(defun twindrill-timeline-spec-is-search-p (spec)
  "Return non-nil if SPEC is a search timeline spec."
  (and (consp spec)
       (eq 'search (car spec))))

(defun twindrill-extract-query-string-from-search-timeline-spec (spec)
  "Return the query string if SPEC is a search timeline spec.
If SPEC is not a search timeline spec, return nil."
  (and (eq 'search (car spec))
       (cadr spec)))

(defun twindrill-equal-string-as-timeline (spec-str1 spec-str2)
  "Return non-nil if SPEC-STR1 equals SPEC-STR2 as a timeline spec.
If either SPEC-STR1 or SPEC-STR2 is invalid as a timeline spec string,
return nil."
  (if (and (stringp spec-str1) (stringp spec-str2))
      (let ((spec1 (twindrill-string-to-timeline-spec spec-str1 t))
	    (spec2 (twindrill-string-to-timeline-spec spec-str2 t)))
	(equal spec1 spec2))
    nil))

(defun twindrill-get-base-timeline-specs (spec)
  "Return the timeline specs on which the timeline SPEC depends.
If SPEC is primary, returns a list consisting of itself.
The result timelines may be a composite timeline."
  (let ((type (car spec)))
    (cond
     ((twindrill-timeline-spec-primary-p spec)
      `(,spec))
     ((memq type '(exclude-if exclude-re))
      `(,(elt spec 2)))
     ((eq type 'merge)
      (cdr spec))
     (t
      nil))))

(defun twindrill-get-primary-base-timeline-specs (spec)
  "Return the primary timeline specs on which the timeline SPEC depends.
If SPEC is primary, returns a list consisting of itself.
The result timelines are primary."
  (if (twindrill-timeline-spec-primary-p spec)
      `(,spec)
    (twindrill-remove-duplicates
     (apply 'append
	    (mapcar 'twindrill-get-primary-base-timeline-specs
		    (twindrill-get-base-timeline-specs spec))))))

(defun twindrill-get-dependent-timeline-specs (base-spec)
  "Return a list of timeline specs that depend on BASE-SPEC.
If BASE-SPEC is a primary timeline spec, the return value consists of
BASE-SPEC and composite timeline specs that depend on BASE-SPEC and are
bound to a live buffer.
If BASE-SPEC is a composite timeline spec, the return value consists of
composite timeline specs that depend on BASE-SPEC and are bound to a live
buffer."
  (twindrill-remove-duplicates
   `(;; BASE-SPEC may not be bound to a live buffer.
     ,@(when (twindrill-timeline-spec-primary-p base-spec)
	 `(,base-spec))
     ,@(remove
	nil
	(mapcar
	 (lambda (spec)
	   (when (twindrill-timeline-spec-depending-on-p spec base-spec)
	     spec))
	 (mapcar 'twindrill-get-timeline-spec-for-buffer
		 (twindrill-get-buffer-list)))))))

(defun twindrill-generate-composite-timeline (spec base-spec base-statuses)
  "Generate statuses for the timeline SPEC from BASE-STATUSES.
BASE-STATUSES must originate from the BASE-SPEC timeline.
If SPEC is a primary timeline and equals BASE-SPEC, just return BASE-STATUSES.
If SPEC is a primary timeline and does not equal BASE-SPEC, return nil."
  (let ((type (car spec)))
    (cond
     ((twindrill-timeline-spec-primary-p spec)
      (if (equal spec base-spec)
	  (let ((pattern-list
		 (twindrill-get-filter-list-for-timeline-spec
		  spec)))
	    (if pattern-list
		(remove
		 nil
		 (mapcar
		  (lambda (status)
		    (if (twindrill-match-pattern-list status pattern-list)
			(progn
			  (debug-printf "Exclude the status: %s" status)
			  nil)
		      status))
		  base-statuses))
	      base-statuses))
	nil))
     ((eq type 'exclude-if)
      (let* ((direct-base (car (twindrill-get-base-timeline-specs spec)))
	     (direct-base-statuses
	      (twindrill-generate-composite-timeline direct-base
						      base-spec base-statuses))
	     (func (elt spec 1)))
	(remove nil
		(mapcar (lambda (status)
			  (unless (funcall func status)
			    status))
			direct-base-statuses))))
     ((eq type 'exclude-re)
      (let* ((direct-base (car (twindrill-get-base-timeline-specs spec)))
	     (direct-base-statuses
	      (twindrill-generate-composite-timeline direct-base
						      base-spec base-statuses))
	     (regexp (elt spec 1)))
	(remove nil
		(mapcar
		 (lambda (status)
		   (unless (string-match regexp (cdr (assq 'text status)))
		     status))
		 direct-base-statuses))))
     ((eq type 'merge)
      (sort
       (apply 'append
	      (mapcar (lambda (direct-base-spec)
			;; `copy-sequence' is required because `sort'
			;; modifies the appended list that may include
			;; `base-statuses' as a tail.
			;; `base-statuses' may refer to the original list
			;; which already retrieved tweets are registered
			;; with. It must not be modified.
			(copy-sequence
			 (twindrill-generate-composite-timeline
			  direct-base-spec base-spec base-statuses)))
		      (twindrill-get-base-timeline-specs spec)))
       (lambda (status1 status2)
	 (let ((id1 (cdr (assq 'id status1)))
	       (id2 (cdr (assq 'id status2))))
	   (twindrill-status-id< id2 id1)))))
     (t
      nil))))

;;;;
;;;; Filter
;;;;

(defun twindrill-get-filter-list-for-timeline-spec-string (spec-string)
  (let ((entry-list twindrill-filter-alist))
    (remove
     nil
     (mapcar
      (lambda (entry)
	(let ((spec-regexp
	       (if (listp (car entry))
		   (concat "\\(?:"
			   (mapconcat 'identity (car entry) "\\|")
			   "\\)")
		 (car entry)))
	      (pattern-list (cdr entry)))
	  (when (string-match spec-regexp spec-string)
	    pattern-list)))
      entry-list))))

(defun twindrill-get-filter-list-for-timeline-spec (spec)
  (when twindrill-filter-alist
    (let* ((spec-string (twindrill-timeline-spec-to-string spec))
	   (short-spec-string (twindrill-timeline-spec-to-string spec t))
	   (regexp-list
	    (twindrill-get-filter-list-for-timeline-spec-string
	     spec-string)))
      (if (string= spec-string short-spec-string)
	  regexp-list
	(append regexp-list
		(twindrill-get-filter-list-for-timeline-spec-string
		 short-spec-string))))))

(defun twindrill-match-pattern (status pattern)
  (let* ((rest pattern)
	 (matched t))
    (while (and rest matched)
      (let* ((current (car rest))
	     (sym (car current))
	     (regexp (cdr current))
	     (value (cdr (assq sym status)))
	     (value
	      (if (eq sym 'text)
		  (twindrill-make-fontified-tweet-text-with-entity status)
		value)))
	(unless (and (stringp value)
		     (string-match regexp value))
	  (setq matched nil))
	(setq rest (cdr rest))))
    matched))

(defun twindrill-match-pattern-list (status pattern-list)
  (let* ((rest pattern-list)
	 (matched nil))
    (while (and rest (not matched))
      (let ((current (car rest)))
	(when (twindrill-match-pattern status current)
	  (setq matched t))
	(setq rest (cdr rest))))
    matched))

;;;;
;;;; Retrieved statuses (timeline data)
;;;;

(defun twindrill-current-timeline-id-table (&optional spec)
  (let ((spec (or spec (twindrill-current-timeline-spec))))
    (if spec
	(elt (gethash spec twindrill-timeline-data-table) 0)
      nil)))

(defun twindrill-current-timeline-referring-id-table (&optional spec)
  "Return the hash from a ID to the ID of the first observed status
referring the former ID."
  (let* ((spec (or spec (twindrill-current-timeline-spec)))
	 (type (car spec)))
    (cond
     ((null spec)
      nil)
     ((memq type '(exclude-if exclude-re merge))
      ;; Use the first non-nil table instead of merging the all tables
      ;; because it may take a long time to merge them.
      (car
       (remove
	nil
	(mapcar (lambda (base-spec)
		  (elt (gethash base-spec twindrill-timeline-data-table) 1))
		(twindrill-get-primary-base-timeline-specs spec)))))
     ((eq type 'single)
      ;; Single tweet timelines are registered in a special way.
      ;; See `twindrill-retrieve-single-tweet-sentinel'.
      (elt (gethash '(:single) twindrill-timeline-data-table) 1))
     (t
      (elt (gethash spec twindrill-timeline-data-table) 1)))))

(defun twindrill-current-timeline-data (&optional spec)
  (let* ((spec (or spec (twindrill-current-timeline-spec)))
	 (type (car spec)))
    (cond
     ((null spec)
      nil)
     ((eq type 'single)
      (let* ((id (cadr spec))
	     (status (twindrill-find-status id)))
	(if status
	    `(,status)
	  nil)))
     ((memq type '(exclude-if exclude-re merge))
      (let ((primary-base-specs
	     (twindrill-get-primary-base-timeline-specs spec)))
	(sort
	 (apply
	  'append
	  (mapcar
	   (lambda (primary-spec)
	     ;; `copy-sequence' is required to prevent `sort'
	     ;; from modifying lists of statuses in the database
	     ;; `twindrill-timeline-data-table'.
	     ;; The result of `twindrill-generate-composite-timeline'
	     ;; may include a list in the database. If so, the simply
	     ;; appended list include it as a tail.
	     (copy-sequence
	      (twindrill-generate-composite-timeline
	       spec
	       primary-spec (twindrill-current-timeline-data primary-spec))))
	   primary-base-specs))
	 (lambda (status1 status2)
	   (let ((id1 (cdr (assq 'id status1)))
		 (id2 (cdr (assq 'id status2))))
	     (twindrill-status-id< id2 id1))))))
     ((eq type :single)
      ;; The timeline spec '(:single) does not correspond to an ordinary
      ;; timeline. It means an unordered set of tweets retrieved by the
      ;; 'retrieve-single-tweet command of `twindrill-call-api'.
      ;; If this function is used with the spec '(:single), a specific tweet
      ;; will be required with the user's intention.
      ;; In this case, exclusion by patterns does not required.
      (elt (gethash spec twindrill-timeline-data-table) 2))
     (t
      (let ((statuses (elt (gethash spec twindrill-timeline-data-table) 2))
	    (pattern-list
	     (twindrill-get-filter-list-for-timeline-spec spec)))
	(if pattern-list
	    (remove
	     nil
	     (mapcar
	      (lambda (status)
		(if (twindrill-match-pattern-list status pattern-list)
		    (progn
		      (debug-printf "Exclude the status: %s" status)
		      nil)
		  status))
	      statuses))
	  statuses))))))

(defun twindrill-remove-timeline-data (&optional spec)
  (let ((spec (or spec (twindrill-current-timeline-spec))))
    (remhash spec twindrill-timeline-data-table)))

(defun twindrill-find-status (id)
  (let ((result nil))
    (maphash
     (lambda (spec pair)
       (let* ((id-table (car pair))
	      (entry (gethash id id-table)))
	 ;; Take the most detailed status.
	 (when (and entry
		    (or (null result) (< (length result) (length entry))))
	   (setq result entry))))
     twindrill-timeline-data-table)
    result))

(defun twindrill-delete-status-from-data-table (id)
  (let ((modified-spec nil))
    (maphash
     (lambda (spec data)
       (let* ((id-table (elt data 0))
	      (referring-id-table (elt data 1))
	      (timeline-data (elt data 2))
	      (status (gethash id id-table)))
	 (when status
	   (remhash id id-table)
	   ;; Here, `referring-id-table' is not modified.
	   ;; Therefore, the retweet observed secondly will not appear even
	   ;; if the retweet observed first for the same tweet is deleted.
	   (setq modified-spec
		 (cons `(,spec
			 ,id-table
			 ,referring-id-table
			 ,(remove status timeline-data))
		       modified-spec)))))
     twindrill-timeline-data-table)
    (mapc
     (lambda (spec)
       (let ((buffer (twindrill-get-buffer-from-spec spec)))
	 (when (buffer-live-p buffer)
	   (with-current-buffer buffer
	     (save-excursion
	       (twindrill-for-each-property-region
		'id
		(lambda (beg end value)
		  (when (twindrill-status-id= id value)
		    (let ((buffer-read-only nil)
			  (separator-pos (min (point-max) (1+ end))))
		      (delete-region beg separator-pos)
		      (goto-char beg))))
		buffer))))))
     (twindrill-remove-duplicates
      (apply 'append
	     (mapcar
	      (lambda (data)
		(let ((spec (car data)))
		  ;; Update the entry for `spec' in
		  ;; `twindrill-timeline-data-table' with the new
		  ;; timeline-data that does not include `status'.
		  (puthash spec (cdr data) twindrill-timeline-data-table)
		  (twindrill-get-dependent-timeline-specs spec)))
	      modified-spec))))))

(defun twindrill-get-replied-statuses (id &optional count)
  "Return a list of replied statuses starting from the status specified by ID.
Statuses are stored in ascending-order with respect to their IDs."
  (let ((result nil)
	(status (twindrill-find-status id)))
    (while
	(and (if (numberp count)
		 (<= 0 (setq count (1- count)))
	       t)
	     (let ((replied-id (or (cdr (assq 'in-reply-to-status-id status))
				   "")))
	       (unless (string= "" replied-id)
		 (let ((replied-status (twindrill-find-status replied-id)))
		   (when replied-status
		     (setq result (cons replied-status result))
		     (setq status replied-status)
		     t))))))
    result))

(defun twindrill-have-replied-statuses-p (id)
  (let ((status (twindrill-find-status id)))
    (when status
      (let ((replied-id (cdr (assq 'in-reply-to-status-id status))))
	(and replied-id (not (string= "" replied-id)))))))

(defun twindrill-add-statuses-to-timeline-data (statuses &optional spec)
  "Add STATUSES as new statuses for SPEC and update derived timelines.
The function returns a list of lists including an updated timeline spec
string and the number of new statuses for the timeline."
  (let* ((spec (or spec (twindrill-current-timeline-spec)))
	 (id-table
	  (or (twindrill-current-timeline-id-table spec)
	      (make-hash-table :test 'equal)))
	 (referring-id-table
	  (or (twindrill-current-timeline-referring-id-table spec)
	      (make-hash-table :test 'equal)))
	 (timeline-data (twindrill-current-timeline-data spec)))
    (let* ((new-statuses
	    (remove nil
		    (mapcar
		     (lambda (status)
		       (let ((id (cdr (assq 'id status)))
			     (retweeted-id (cdr (assq 'retweeted-id status))))
			 (unless (or (not retweeted-id)
				     (gethash retweeted-id referring-id-table))
			   ;; Store the id of the first observed tweet
			   ;; that refers `retweeted-id'.
			   (puthash retweeted-id id referring-id-table))
			 (if (gethash id id-table)
			     nil
			   (puthash id status id-table)
			   (puthash id id referring-id-table)
			   `((source-spec . ,spec)
			     ,@status))))
		     statuses)))
	   (new-statuses
	    ;; Sort tweets by ID.
	    ;; This is necessary because `twindrill-render-timeline' assumes
	    ;; that given tweets are ordered.
	    (sort new-statuses
		  (lambda (status1 status2)
		    (let ((id1 (cdr (assq 'id status1)))
			  (id2 (cdr (assq 'id status2))))
		      (twindrill-status-id< id2 id1))))))
      (when new-statuses
	(let ((new-timeline-data
	       (sort (append new-statuses timeline-data)
		     (lambda (status1 status2)
		       (let ((id1 (cdr (assq 'id status1)))
			     (id2 (cdr (assq 'id status2))))
			 (twindrill-status-id< id2 id1))))))
	  (puthash spec `(,id-table ,referring-id-table ,new-timeline-data)
		   twindrill-timeline-data-table))
	(let ((twindrill-new-tweets-spec spec)
	      (twindrill-new-tweets-statuses new-statuses)
	      (twindrill-new-tweets-count (length new-statuses)))
	  (run-hooks 'twindrill-new-tweets-hook)))
      ;; Update timelines derived from SPEC and return the number of
      ;; new tweets for each updated timeline.
      (remove
       nil
       (mapcar
	(lambda (buffer)
	  (let ((other-spec (twindrill-get-timeline-spec-for-buffer buffer))
		(other-spec-string
		 (twindrill-get-timeline-spec-string-for-buffer buffer)))
	    (when (twindrill-timeline-spec-depending-on-p other-spec spec)
	      (let* ((twindrill-new-tweets-spec other-spec)
		     (twindrill-new-tweets-statuses
		      (twindrill-generate-composite-timeline
		       other-spec spec new-statuses))
		     (twindrill-new-tweets-count
		      (length twindrill-new-tweets-statuses))
		     (rendered-tweets
		      (twindrill-render-timeline
		       buffer twindrill-new-tweets-statuses t)))
		(when rendered-tweets
		  (when (not (equal spec other-spec))
		    ;; The hook has been alreadly invoked for `spec'.
		    (run-hooks 'twindrill-new-tweets-hook))
		  `(,other-spec-string ,(length rendered-tweets)))))))
	(twindrill-get-buffer-list))))))

;;;;
;;;; URIs related to a tweet
;;;;

(defun twindrill-get-status-url (username &optional id)
  "Generate a URL of a user or a specific status."
  (let ((func
	 (cdr (assq
	       'status-url
	       (assq twindrill-service-method
		     twindrill-service-method-table)))))
    (funcall func username id)))

(defun twindrill-get-status-url-from-alist (status)
  "Generate a URL of a tweet specified by an alist STATUS."
  (let ((username (cdr (or (assq 'retweeted-user-screen-name status)
			   (assq 'user-screen-name status))))
	(id (cdr (or (assq 'retweeted-id status)
		     (assq 'id status))))
	(func
	 (cdr (assq
	       'status-url
	       (assq twindrill-service-method
		     twindrill-service-method-table)))))
    (funcall func username id)))

(defun twindrill-get-list-url (username listname)
  "Generate a URL of a specific list."
  (let ((func
	 (cdr (assq
	       'status-url
	       (assq twindrill-service-method
		     twindrill-service-method-table))))
	(str (concat username "/" listname)))
    (funcall func str nil)))

(defun twindrill-get-status-url-twitter (username &optional id)
  "Generate status URL for Twitter."
  (if id
      (format "http://%s/%s/status/%s" twindrill-web-host username id)
    (format "http://%s/%s" twindrill-web-host username)))

(defun twindrill-get-status-url-statusnet (username &optional id)
  "Generate status URL for StatusNet."
  (if id
      (format "http://%s/%s/notice/%s" twindrill-web-host twindrill-web-path-prefix id)
    (format "http://%s/%s/%s" twindrill-web-host twindrill-web-path-prefix username)))

(defun twindrill-get-search-url (query-string)
  "Generate a URL for searching QUERY-STRING."
  (let ((func (cdr (assq
		    'search-url (assq twindrill-service-method
				      twindrill-service-method-table)))))
    (funcall func query-string)))

(defun twindrill-get-search-url-twitter (query-string)
  (format "http://%s/search?q=%s"
	  twindrill-web-host (twindrill-percent-encode query-string)))

(defun twindrill-get-search-url-statusnet (query-string)
  (if (string-match "^#\\(.+\\)" query-string)
      (format "http://%s/%s/tag/%s"
	      twindrill-web-host
	      twindrill-web-path-prefix
	      (twindrill-percent-encode (match-string 1 query-string)))
    (format "http://%s/search?q=%s"
	    twindrill-web-host (twindrill-percent-encode query-string))))

(defun twindrill-extract-id-from-url (url-string)
  "Extract the ID from URL-STRING.
Return nil if URL-STRING cannot be interpreted as a URL pointing a tweet."
  (when (string-match
	 "\\`https?://twitter.com/\\(?:#!/\\)?[^/]+/status\\(?:es\\)?/\\([0-9]+\\)/?\\'"
	 url-string)
    (match-string 1 url-string)))

(provide 'twindrill-timeline)
;;; twindrill-timeline.el ends here
