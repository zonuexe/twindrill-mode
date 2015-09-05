;;; twindrill-util.el --- Utilities for twindrill-mode

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

(require 'cl-macs)

;;;;
;;;; Utility for portability
;;;;

(defun twindrill-remove-duplicates (list)
  "Return a copy of LIST with all duplicate elements removed.
This is non-destructive version of `delete-dups' which is not
defined in Emacs21."
  (if (fboundp 'delete-dups)
      (delete-dups (copy-sequence list))
    (let ((rest list)
	  (result nil))
      (while rest
	(unless (member (car rest) result)
	  (setq result (cons (car rest) result)))
	(setq rest (cdr rest)))
      (nreverse result))))

(defun twindrill-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a string in the minibuffer, with completion.
This is a modified version of `completing-read' and accepts candidates
as a list of a string on Emacs21."
  ;; completing-read() of Emacs21 does not accepts candidates as
  ;; a list. Candidates must be given as an alist.
  (let* ((collection (twindrill-remove-duplicates collection))
	 (collection
	  (if (and (> 22 emacs-major-version)
		   (listp collection)
		   (stringp (car collection)))
	      (mapcar (lambda (x) (cons x nil)) collection)
	    collection)))
    (completing-read prompt collection predicate require-match
		     initial-input hist def inherit-input-method)))

(defun twindrill-add-to-history (history-var elt &optional maxelt keep-all)
  (if (functionp 'add-to-history)
      (add-to-history history-var elt maxelt keep-all)
    (let* ((added (cons elt
			(if (and (not keep-all)
				 (boundp 'history-delete-duplicates)
				 history-delete-duplicates)
			    (delete elt (symbol-value history-var))
			  (symbol-value history-var))))
	   (maxelt (or maxelt history-length))
	   (len (length added)))
      (set history-var
	    (if (<= len maxelt)
		added
	      (butlast added (- len maxelt)))))))

(if (fboundp 'assoc-string)
    (defalias 'twindrill-assoc-string 'assoc-string)
  (defun twindrill-assoc-string (key list &optional case-fold)
    "Like `assoc' but specifically for strings (and symbols).
This returns the first element of LIST whose car matches the string or
symbol KEY, or nil if no match exists.  When performing the
comparison, symbols are first converted to strings, and unibyte
strings to multibyte.  If the optional arg CASE-FOLD is non-nil, case
is ignored.

Unlike `assoc', KEY can also match an entry in LIST consisting of a
single string, rather than a cons cell whose car is a string.

This is reimplemented version of `assoc-string' which is not
defined in Emacs21."
    (let* ((key (if (stringp key)
		    key
		  (symbol-name key)))
	   (regexp (concat "\\`" key "\\'"))
	   (rest list)
	   (result nil)
	   (case-fold-search case-fold))
      (while (not (null rest))
	(let* ((current (car rest))
	       (current-key
		(if (listp current)
		    (car current)
		  current))
	       (current-key
		(if (stringp current-key)
		    current-key
		  (symbol-name current-key))))
	  (if (string-match key current-key)
	      (setq result current
		    rest nil)
	    (setq rest (cdr rest)))))
      result)))

;;;;
;;;; Debug mode
;;;;

(defvar twindrill-debug-mode nil)
(defvar twindrill-debug-buffer "*debug*")

(defun twindrill-get-or-generate-buffer (buffer)
  (if (bufferp buffer)
      (if (buffer-live-p buffer)
	  buffer
	(generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
	(or (get-buffer buffer)
	    (generate-new-buffer buffer)))))

(defun twindrill-debug-buffer ()
  (twindrill-get-or-generate-buffer twindrill-debug-buffer))

(defmacro debug-print (obj)
  (let ((obsym (cl-gensym)))
    `(let ((,obsym ,obj))
       (if twindrill-debug-mode
	   (with-current-buffer (twindrill-debug-buffer)
	     (insert "[debug] " (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))

(defun debug-printf (fmt &rest args)
  (when twindrill-debug-mode
    (with-current-buffer (twindrill-debug-buffer)
      (insert "[debug] " (apply 'format fmt args))
      (newline))))

(defun twindrill-debug-mode ()
  (interactive)
  (setq twindrill-debug-mode
	(not twindrill-debug-mode))
  (message (if twindrill-debug-mode "debug mode:on" "debug mode:off")))

;;;;
;;;; Macro and small utility function
;;;;

(defun assocref (item alist)
  (cdr (assoc item alist)))

(defmacro list-push (value listvar)
  `(setq ,listvar (cons ,value ,listvar)))

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key))
				 keylist))
		't)
	     ,@body)))
       clauses)))

(defmacro twindrill-wait-while (timeout interval condition &optional form &rest timeout-forms)
  "Wait while CONDITION return non-nil until TIMEOUT seconds passes.

The form CONDITION is repeatedly evaluated for every INTERVAL seconds
until CONDITION returns nil or TIMEOUT seconds passes unless TIMEOUT is nil.
If TIMEOUT is nil, there is no time limit.

If CONDITION returns nil, evaluate the form FORM and return its value.
If TIMEOUT seconds passes, evaluate the forms TIMEOUT-FORMS and return
the value of the last form in TIMEOUT-FORMS."
  `(lexical-let (,@(when timeout `((timeout ,timeout)))
		 (interval ,interval)
		 (current 0.0))
     (while (and ,@(when timeout '((< current timeout)))
		 ,condition)
       (sleep-for interval)
       (setq current (+ current interval)))
     ,(when (or form timeout-forms)
	(if (null timeout)
	    form
	  `(if (< current timeout)
	       ,form
	     ,@timeout-forms)))))


(defun twindrill-extract-matched-substring-all (regexp str)
  (let ((pos 0)
	(result nil))
    (while (string-match regexp str pos)
      (setq result (cons (match-string 1 str) result))
      (setq pos (match-end 0)))
    (reverse result)))

(defun twindrill-process-alive-p (proc)
  "Return non-nil if PROC is alive."
  (not (memq (process-status proc) '(nil closed exit failed signal))))

(defun twindrill-start-process-with-sentinel (name buffer program args sentinel)
  "Start a program in a subprocess with a sentinel.

This function is the same as `start-process' except that SENTINEL must
be invoked when the process is successfully started."
  (let ((proc (apply 'start-process name buffer program args)))
    (when (and proc (functionp sentinel))
      (if (twindrill-process-alive-p proc)
	  (set-process-sentinel proc sentinel)
	;; Ensure that the sentinel is invoked if a subprocess is
	;; successfully started.
	(funcall sentinel proc "finished")))
    proc))

(defun twindrill-parse-time-string (str &optional round-up)
  "Parse the time-string STR into (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
This function is the same as `parse-time-string' except to complement the
lacked parameters with the current time.
If ROUND-UP is nil, complement the lacked parameters with the oldest ones.
If ROUND-UP is non-nil, complement the lacked parameters with the latest ones.
For example, (twindrill-parse-time-string \"2012-04-20\")
returns (0 0 0 20 4 2012 nil nil 32400).
And (twindrill-parse-time-string \"2012-04-20\" t)
returns (59 59 23 20 4 2012 nil nil 32400).
The values are identical to those of `decode-time', but any values that are
unknown are returned as nil."
  (let* ((parsed (parse-time-string str))
	 (current (decode-time (current-time)))
	 (replacement-alist
	  `((SEC . ,(if round-up 59 0))
	    (MIN . ,(if round-up 59 0))
	    (HOUR . ,(if round-up 23 0))
	    (DAY . nil)
	    (MON . nil)
	    (YEAR . nil)
	    (DOW . nil)
	    (DST . nil)
	    (TZ . nil)))
	 (sym-list (mapcar 'car replacement-alist))
	 (result nil))
    (while (and parsed current sym-list)
      (let* ((sym (car sym-list))
	     (v (or (car parsed)
		    (cdr (assq sym replacement-alist))
		    ;; If `sym' is not 'DOW and it is bound to nil
		    ;; in `replacement-alist', use `current'.
		    (unless (eq sym 'DOW)
		      (car current)))))
	(setq result (cons v result)))
      (setq parsed (cdr parsed))
      (setq current (cdr current))
      (setq sym-list (cdr sym-list)))
    (reverse result)))

(defun twindrill-normalize-string (str)
  (if (require 'ucs-normalize nil t)
      (ucs-normalize-NFC-string str)
    str))

;;;;
;;;; Utility of status IDs
;;;;

(defun twindrill-status-id< (id1 id2)
  "Return t if version `ID1' is lower (older) than `ID2'."
  (let ((len1 (length id1))
	(len2 (length id2)))
    (cond
     ((= len1 len2) (string< id1 id2))
     ((< len1 len2) t)
     (t nil))))

(defun twindrill-status-id= (id1 id2)
  "Return t if version `ID1' is equal to `ID2'."
  (equal id1 id2))

(defun twindrill-snowflake-epoch-time ()
  "Return the epoch time of Snowflake."
  (require 'calc)
  (let ((epoch-str
	 ;; This corresponds to 2010-11-04 01:42:54+00:00 in RFC3339.
	 ;; The value comes from the following page.
	 ;; https://github.com/twitter/snowflake/blob/6d4634aa490de26e22425538291fe0a03071a170/src/main/scala/com/twitter/service/snowflake/IdWorker.scala#L22
	 ;; 22    val twepoch = 1288834974657L
	 "1288834974657"))
    (let ((str
	   (calc-eval `(,(concat "floor(10#" epoch-str "/10#1000)")
			calc-word-size 64 calc-number-radix 16)))
	  (milisec-str
	   (calc-eval `(,(concat "10#" epoch-str "%10#1000")
			calc-word-size 64 calc-number-radix 10))))
      (mapcar (lambda (s) (string-to-number s 16))
	      `(,(substring str 3 7) ,(substring str 7)
		,milisec-str)))))

(defun twindrill-id-to-time (id)
  "Return the time corresonding to ID generated by Snowflake.
If ID is a string consisting of 12 or more digits, return the corresponding
time in Emacs internal representation the same as `encode-time'.
Otherwise, return nil.

This is because ID consisting of 11 or less digits may not be generated
by Snowflake."
  (require 'calc)
  (cond
   ((not (stringp id))
    nil)
   ((< (length id) 12)
    ;; The given ID may not be generated by Snowflake.
    ;;
    ;; The first tweet in the example response of
    ;; https://dev.twitter.com/docs/api/1/get/statuses/home_timeline
    ;; has "18700887835" as the ID.
    ;; Assuming that it is generated by Snowflake, it corresponds to
    ;; "2010-11-04 01:42:58+00:00".
    ;; However, the created_at is "Fri Jul 16 16:58:46 +0000 2010".
    ;; It can be confirmed at http://twitter.com/cindyli/status/18700887835 .
    ;;
    ;; Therefore we can not suppose that an ID is generated by Snowflake
    ;; if the ID consists of 11-digits.
    ;; Assuming that an ID reaches 10^11 before Snowflake is turned on at
    ;; Nov 04 2010, the average number of tweets per day will exceed
    ;; 533 million.
    ;; ( 10^11 - 18,700,887,835 > 80,000,000,000
    ;;   80 billion / 5 month (from Jul 16 to Nov 4) > 533 million / day )
    ;;
    ;; It is impossible.
    ;; So, I assume that an ID is generated by Snowflake if the ID consists of
    ;; 12 or more digits.
    ;; The first ID with 12-digits, 100,000,000,000 (10^11), corresponds to
    ;; 2010-11-04 01:43:17+00:00.
    nil)
   (t
    (let* ((epoch (twindrill-snowflake-epoch-time))
	   (str (calc-eval
		 `(,(format "floor(rsh(10#%s,22)/1000)" id)
		   calc-word-size 64 calc-number-radix 16)))
	   (milisec
	    (string-to-number
	     (calc-eval
	      `(,(format "rsh(10#%s,22)%%1000" id)
		calc-word-size 64 calc-number-radix 10))
	     10)))
      (when (string-match "^16#\\([[:xdigit:]]+\\)" str)
	(let* ((hex-str (match-string 1 str))
	       (hex-str
		(if (< (length hex-str) 4)
		    (concat "0000" hex-str)
		  hex-str)))
	  (time-add epoch
		    `(,(string-to-number
			(substring hex-str 0 (- (length hex-str) 4)) 16)
		      ,(string-to-number
			(substring hex-str (- (length hex-str) 4)) 16)
		      ,(* milisec 1000)
		      ))))))))

(defun twindrill-time-to-id (time)
  "Return the ID corresponding to TIME by Snowflake.
Bits other than timestamp are zero.  The least significant 22 bits are zero.
TIME must be an Emacs internal representation as a return value of
`current-time'."
  (require 'calc)
  (let* ((epoch-time (twindrill-snowflake-epoch-time))
	 (dt (if (time-less-p epoch-time time)
		 (time-subtract time epoch-time)
	       nil))
	 (sec-high (nth 0 dt))
	 (sec-low (nth 1 dt))
	 (microsec (or (nth 2 dt) "0")))
    (when dt
      (calc-eval
       `(,(format "lsh((16#%04x%04x) * 1000 + floor(%d/1000), 22)"
		  sec-high sec-low microsec)
	 calc-word-size 64 calc-number-radix 10)))))

(provide 'twindrill-util)
;;; twindrill-util.el ends here
