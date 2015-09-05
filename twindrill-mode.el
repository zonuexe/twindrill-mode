;;; twindrill-mode.el --- Twitter client for Emacs

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
;; Identity: $Id$
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

;; twindrill-mode.el is a major mode for Twitter.
;; You can check friends timeline, and update your status on Emacs.

;; twindrill-mode is forked from twittering-mode http://twmode.sourceforge.net/
;; thanks!

;;; Feature Request:

;; URL : http://twitter.com/d00dle/statuses/577876082
;; * Status Input from Popup buffer and C-cC-c to POST.
;; URL : http://code.nanigac.com/source/view/419
;; * update status for region

;; ξ ^ω^)ξ ＜You are an idiot, really.

;;; Code:

(require 'twindrill-util)
(require 'twindrill-edit)
(eval-when-compile (require 'cl))
(require 'xml)
(require 'url)

(defgroup twindrill-mode nil
  "Settings for twindrill-mode."
  :group 'hypermedia)

(defconst twindrill-mode-version "4.0.0")
(defconst twindrill-mode-identity "$Id$")
(defvar twindrill-api-host "api.twitter.com")
(defvar twindrill-api-search-host "search.twitter.com")
(defvar twindrill-web-host "twitter.com")
(defvar twindrill-oauth-request-token-url-without-scheme
  "://api.twitter.com/oauth/request_token")
(defvar twindrill-oauth-authorization-url-base-without-scheme
  "://api.twitter.com/oauth/authorize?oauth_token=")
(defvar twindrill-oauth-access-token-url-without-scheme
  "://api.twitter.com/oauth/access_token")

(defun twindrill-mode-version ()
  "Display a message for twindrill-mode version."
  (interactive)
  (let ((version-string
	 (format "twindrill-mode-v%s" twindrill-mode-version)))
    (if (called-interactively-p 'interactive)
	(message "%s" version-string)
      version-string)))

(defcustom twindrill-auth-method 'oauth
  "*Authentication method to use with `twindrill-mode'.

Choose between symbols `oauth' (default), `basic' or `xauth'.

OAuth Authentication requires `twindrill-oauth-consumer-key' and
`twindrill-oauth-consumer-secret'.

Additionally, it requires an external command `curl' or another
command included in `tls-program', which may be `openssl' or
`gnutls-cli', for SSL."
  :group 'twindrill-mode
  :type '(choice :tag "Twitter authentication method"
		 (const :tag "Basic authentication" :value basic)
		 (const :tag "OAuth authentication" :value oauth)
		 (const :tag "xAuth authentication" :value xauth)))

(defvar twindrill-account-authorization nil
  "State of account authorization for `twindrill-username' and
`twindrill-password'.  The value is one of the following symbols:
nil -- The account have not been authorized yet.
queried -- The authorization has been queried, but not finished yet.
authorized -- The account has been authorized.")

(defcustom twindrill-oauth-use-ssl t
  "*If non-nil, use SSL authentication for OAuth.

Twitter requires SSL on authorization via OAuth."
  :group 'twindrill-mode
  :type 'boolean)

(defcustom twindrill-oauth-invoke-browser nil
  "*If non-nil, invoke a browser on authorization of access key automatically."
  :type 'boolean
  :group 'twindrill-mode)

(defvar twindrill-oauth-consumer-key nil)
(defvar twindrill-oauth-consumer-secret nil)
(defvar twindrill-oauth-access-token-alist nil)

(defconst twindrill-max-number-of-tweets-on-retrieval 200
  "The maximum number of `twindrill-number-of-tweets-on-retrieval'.")

(defcustom twindrill-number-of-tweets-on-retrieval 20
  "*Number of tweets which will be retrieved in one request.

The upper limit is `twindrill-max-number-of-tweets-on-retrieval'."
  :type 'integer
  :group 'twindrill-mode)

(defvar twindrill-mode-map (make-sparse-keymap))
(defvar twindrill-mode-menu-on-uri-map (make-sparse-keymap "Twindrill Mode"))
(defvar twindrill-mode-on-uri-map (make-sparse-keymap))

(defvar twindrill-tweet-history nil)
(defvar twindrill-user-history nil)
(defvar twindrill-timeline-history nil)
(defvar twindrill-hashtag-history nil)
(defvar twindrill-search-history nil)

(defvar twindrill-current-hashtag nil
  "A hash tag string currently set. You can set it by calling
`twindrill-set-current-hashtag'.")

(defvar twindrill-timer nil
  "Timer object for timeline refreshing will be stored here.
DO NOT SET VALUE MANUALLY.")

(defcustom twindrill-timer-interval 90
  "Number of seconds to wait before an auto-reload occurs.

Number of API calls per hour is limited so this value should be 60 or more."
  :type 'integer
  :group 'twindrill-mode)

(defvar twindrill-timer-for-redisplaying nil
  "Timer object for timeline redisplay statuses will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar twindrill-timer-interval-for-redisplaying 5.0
  "The interval of auto redisplaying statuses.
Each time Emacs remains idle for the interval, twindrill-mode updates parts
requiring to be redrawn.")

(defcustom twindrill-username nil
  "*A username of your Twitter account."
  :type '(choice (const nil)
		 string)
  :group 'twindrill-mode)

(defcustom twindrill-password nil
  "*A password of your Twitter account. Leave it blank is the
recommended way because writing a password in .emacs file is so
dangerous."
  :type '(choice (const nil)
		 string)
  :group 'twindrill-mode)

(defcustom twindrill-initial-timeline-spec-string ":home"
  "*An initial timeline spec string or a list of timeline spec strings.
This specifies one or more initial timeline spec strings, which are
automatically visited when invoking `twindrill-mode' or `twit'.

If it is a string, it specifies a timeline spec string.
If it is a list of strings, it specifies multiple timeline spec strings."
  :type '(choice (const nil)
		 string)
  :group 'twindrill-mode)

(defvar twindrill-timeline-spec nil
  "The timeline spec for the current buffer.")
(defvar twindrill-timeline-spec-string ""
  "The timeline spec string for the current buffer.")

(defcustom twindrill-timeline-spec-alias nil
  "*Alist for aliases of timeline spec.
Each element is (NAME . SPEC-STRING), where NAME is a string and
SPEC-STRING is a string or a function that returns a timeline spec string.

The alias can be referred as \"$NAME\" or \"$NAME(ARG)\" in timeline spec
string. If SPEC-STRING is a string, ARG is simply ignored.
If SPEC-STRING is a function, it is called with a string argument.
For the style \"$NAME\", the function is called with nil.
For the style \"$NAME(ARG)\", the function is called with a string ARG.

For example, if you specify
 `((\"FRIENDS\" . \"my-account/friends-list\")
   (\"related-to\" .
            ,(lambda (username)
               (if username
                   (format \":search/to:%s OR from:%s OR @%s/\"
                           username username username)
                 \":home\")))),
then you can use \"$FRIENDS\" and \"$related-to(USER)\" as
\"my-account/friends-list\" and \":search/to:USER OR from:USER OR @USER/\",
respectively."
  :type 'alist
  :group 'twindrill-mode)

(defvar twindrill-current-timeline-spec-string nil
  "The current timeline spec string. This variable should not be referred
directly. Use `twindrill-current-timeline-spec-string' or
`twindrill-current-timeline-spec'.")
(defvar twindrill-list-index-retrieved nil)

(defvar twindrill-process-info-alist nil
  "Alist of active process and timeline spec retrieved by the process.")

(defvar twindrill-server-info-alist nil
  "Alist of server information.")

(defvar twindrill-api-limit-info-alist '()
  "Alist of an API identifier and an alist representing rate limit for the API.")

(defvar twindrill-timeline-spec-to-api-table '()
  "Alist of a timeline spec and an API identifier for retrieving the timeline.")

(defcustom twindrill-mode-init-hook nil
  "*Hook run after initializing global variables for `twindrill-mode'."
  :type 'hook
  :group 'twindrill-mode)

(defcustom twindrill-mode-hook nil
  "*Hook run every time a buffer is initialized as a `twindrill-mode' buffer."
  :type 'hook
  :group 'twindrill-mode)

(defvar twindrill-cookie-alist nil
  "Alist for stroing cookies for each account.
This variable stores an alist.
A key of the alist is a string that is a screen name of an account.
A value of the alist is a cookie alist which corresponds to a list of
a pair of a cookie name and value.")

(defvar twindrill-new-tweets-count 0
  "Number of new tweets when `twindrill-new-tweets-hook' is run.")
(defvar twindrill-new-tweets-spec nil
  "Timeline spec, which new tweets belong to, when
`twindrill-new-tweets-hook' is run.")
(defvar twindrill-new-tweets-statuses nil
  "New tweet status messages, when
`twindrill-new-tweets-hook' is run.")

(defcustom twindrill-new-tweets-hook nil
  "*Hook run when new tweets are received.

You can read `twindrill-new-tweets-count' or `twindrill-new-tweets-spec'
to get the number of new tweets received when this hook is run."
  :type 'hook
  :group 'twindrill-mode)

(defvar twindrill-rendered-new-tweets-spec nil
  "A timeline spec of newly rendered tweets.
This variable is bound when invoking hooks registered with
`twindrill-new-tweets-rendered-hook'.")

(defvar twindrill-rendered-new-tweets-spec-string nil
  "A timeline spec string of newly rendered tweets.
This variable is bound when invoking hooks registered with
`twindrill-new-tweets-rendered-hook'.")

(defvar twindrill-rendered-new-tweets nil
  "A list of newly rendered tweets.
Hooks registered with `twindrill-new-tweets-rendered-hook' can use this
variable as a list of rendered tweets. Each tweet is represented as an alist.
You can refer to a property of a tweet alist as
 (cdr (assq PROPERTY-SYMBOL TWEET-ALIST)).
Valid symbols are following; id, text, user-name, user-screen-name, user-id,
 source, source-uri.
In the list, tweets are placed in order of time.  The car of the list is the
latest one, and the last is the oldest one.")

(defcustom twindrill-new-tweets-rendered-hook nil
  "*Hook run when new tweets are rendered.
When the registered functions are called, the current buffer is the buffer
that the new tweets are just rendered on.
The functions can refer to the timeline spec and timeline spec string as
`twindrill-rendered-new-tweets-spec' and
`twindrill-rendered-new-tweets-spec-string', repectively.
Hooks can also use the local variable `twindrill-rendered-new-tweets' as a
list of rendered tweets.
For the detail of the representation of tweets, see the variable
`twindrill-rendered-new-tweets'."
  :type 'hook
  :group 'twindrill-mode)

(defvar twindrill-active-mode nil
  "Non-nil if new statuses should be retrieved periodically.
Do not modify this variable directly. Use `twindrill-activate-buffer',
`twindrill-deactivate-buffer', `twindrill-toggle-activate-buffer' or
`twindrill-set-active-flag-for-buffer'.")

(defcustom twindrill-reverse-mode nil
  "*Non-nil means tweets are aligned in reverse order of `http://twitter.com/'."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-display-remaining nil
  "*If non-nil, display remaining of rate limit on the mode-line."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-display-connection-method t
  "*If non-nil, display the current connection method on the mode-line."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-status-format "%i %s,  %@:\n%FILL[  ]{%T // from %f%L%r%R}\n "
  "Format string for rendering statuses.
Ex. \"%i %s,  %@:\\n%FILL{  %T // from %f%L%r%R}\n \"

Items:
 %s - screen_name
 %S - name
 %i - profile_image
 %d - description
 %l - location
 %L - \" [location]\"
 %r - \" sent to user\" (use on direct_messages{,_sent})
 %r - \" in reply to user\" (use on other standard timeline)
 %R - \" (retweeted by user)\"
 %RT{...} - strings rendered only when the tweet is a retweet.
            The braced strings are rendered with the information of the
            retweet itself instead of that of the retweeted original tweet.
            For example, %s for a retweet means who posted the original
            tweet, but %RT{%s} means who retweeted it.
 %u - url
 %j - user.id
 %p - protected?
 %c - created_at (raw UTC string)
 %C{time-format-str} - created_at (formatted with time-format-str)
 %@{time-format-str} - X seconds ago (formatted with time-format-str)
 %T - raw text
 %t - text filled as one paragraph
 %' - truncated
 %FACE[face-name]{...} - strings decorated with the specified face.
 %FIELD[format-str]{field-name}
   - a value of the given field of a tweet formatted with format-str.
     The format-str is optional. As a field-name, you can use
     \"retweet_count\", \"favorite_count\" and so on.
 %FIELD-IF-NONZERO[format-str]{field-name}
   - similar to %FIELD[...]{...} except that this makes an empty string
     if the field value is zero.
 %FILL[prefix]{...} - strings filled as a paragraph. The prefix is optional.
                      You can use any other specifiers in braces.
 %FOLD[prefix]{...} - strings folded within the frame width.
                      The prefix is optional. This keeps newlines and does not
                      squeeze a series of white spaces.
                      You can use any other specifiers in braces.
 %f - source
 %# - id"
  :type 'string
  :group 'twindrill-mode)

(defcustom twindrill-retweet-format '(nil _ " RT: %t (via @%s)")
  "*A format string or a skeleton for retweet.
If the value is a string, it means a format string for generating an initial
string of a retweet. The format string is converted with the below replacement
table. And then, the cursor is placed on the next of the initial string.
It is equivalent to the skeleton '(nil STRING _).
Note that this string is inserted before the edit skeleton specified by
`twindrill-edit-skeleton' is performed.

If the value is a list, it is treated as a skeleton used with
`skeleton-insert'. The strings included in the list are converted with the
following replacement table. And then, the list with converted strings is
inserted by `skeleton-insert'.
Note that this skeleton is performed before the edit skeleton specified by
`twindrill-edit-skeleton' is performed.

Replacement table:
 %s - The screen-name of the cited tweet.
 %t - The text of the cited tweet.
 %u - The URL of the cited tweet.
 %# - The ID of the cited tweet.
 %% - % itself."
  :type 'sexp
  :group 'twindrill-mode)

(defcustom twindrill-fill-column nil
  "*The `fill-column' used for \"%FILL{...}\" in `twindrill-status-format'.
If nil, the fill-column is automatically calculated."
  :type '(choice (const nil)
		 integer)
  :group 'twindrill-mode)

(defcustom twindrill-show-replied-tweets t
  "*The number of replied tweets which will be showed in one tweet.

If the value is not a number and is non-nil, show all replied tweets
which is already fetched.
If the value is nil, doesn't show replied tweets."

  :type '(choice (const :tag "Do not show replied tweets"
			:value nil)
		 (const :tag "Show all replied tweets"
			:value t)
		 (integer :tag "Number of replied tweet"))
  :group 'twindrill-mode)

(defcustom twindrill-default-show-replied-tweets nil
  "*The number of default replied tweets which will be shown in one tweet.
This value will be used only when showing new tweets.

See `twindrill-show-replied-tweets' for more details."
  :type '(choice (const nil)
		 integer)
  :group 'twindrill-mode)

(defcustom twindrill-disable-overlay-on-too-long-string nil
  "*If non-nil, disable overlay on too long string on edit buffer.

If nil, `twindrill-edit-mode' puts an overlay `twindrill-warning-overlay' on
characters following the 140th character.

On some environments, some input methods seem to interfere the update of the
overlay.  In such case, you may avoid the problems by setting this variable to
non-nil."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-use-show-minibuffer-length t
  "*Show current length of minibuffer if this variable is non-nil.

We suggest that you should set to nil to disable the showing function
when it conflict with your input method (such as AquaSKK, etc.)"
  :type 'boolean
  :group 'twindrill-mode)

(defvar twindrill-notify-successful-http-get t)

(defcustom twindrill-use-ssl t
  "*Use SSL connection if this variable is non-nil.

SSL connections use an external command as a backend."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-allow-insecure-server-cert nil
  "*If non-nil, `twindrill-mode' allows insecure server certificates."
  :type 'boolean
  :group 'twindrill-mode)

(defvar twindrill-curl-program nil
  "Cache a result of `twindrill-find-curl-program'.
DO NOT SET VALUE MANUALLY.")
(defvar twindrill-curl-program-https-capability nil
  "Cache a result of `twindrill-start-http-session-curl-https-p'.
DO NOT SET VALUE MANUALLY.")

(defvar twindrill-wget-program nil
  "Cache a result of `twindrill-find-wget-program'.
DO NOT SET VALUE MANUALLY.")

(defcustom twindrill-tls-program nil
  "*List of strings containing commands to start TLS stream to a host.
Each entry in the list is tried until a connection is successful.
%h is replaced with server hostname, %p with port to connect to.
Also see `tls-program'.
If nil, this is initialized with a list of valied entries extracted from
`tls-program'."
  :type '(repeat string)
  :group 'twindrill-mode)

(defcustom twindrill-connection-type-order
  '(curl wget urllib-http native urllib-https)
  "*A list of connection methods in the preferred order."
  :type 'list
  :group 'twindrill-mode)

(defun twindrill-connection-build-customize-option ()
  "Generate a valid `defcustom' entry to build `twindrill-connection-type-table' variable."
  (list 'repeat
	(list
	 'cons :tag "Connection"
	 '(symbol :tag "Name" :value "")
	 '(repeat
	   :tag "Connection method definition"
	   (choice
	    (cons :tag "Check test method"
		  (const :format "" check)
		  (choice :value t (const :tag "Do not check" t)
			  (function :tag "Check function")))
	    (cons :tag "Display name"
		  (const :format "" display-name)
		  string)
	    (cons :tag "HTTPS connection method"
		  (const :format "" https)
		  (choice :value nil (const :tag "None" nil)
			  (const :tag "True" t)
			  (function :tag "HTTPS test function")))
	    (cons :tag "Send HTTP request function"
		  (const :format "" send-http-request)
		  function)
	    (cons :tag "Pre process buffer"
		  (const :format "" pre-process-buffer)
		  function))))))

(defcustom twindrill-connection-type-table
  '((native
     (check . t)
     (send-http-request . twindrill-send-http-request-native)
     (pre-process-buffer . twindrill-pre-process-buffer-native))
    (curl
     (check . twindrill-start-http-session-curl-p)
     (https . twindrill-start-http-session-curl-https-p)
     (send-http-request . twindrill-send-http-request-curl)
     (pre-process-buffer . twindrill-pre-process-buffer-curl))
    (wget
     (check . twindrill-start-http-session-wget-p)
     (https . ignore)
     (send-http-request . twindrill-send-http-request-wget)
     (pre-process-buffer . twindrill-pre-process-buffer-wget))
    (urllib-http
     (display-name . "urllib")
     (check . twindrill-start-http-session-urllib-p)
     (https . nil)
     (send-http-request . twindrill-send-http-request-urllib)
     (pre-process-buffer . twindrill-pre-process-buffer-urllib))
    (urllib-https
     (display-name . "urllib")
     (check . twindrill-start-http-session-urllib-p)
     (https . twindrill-start-http-session-urllib-https-p)
     (send-http-request . twindrill-send-http-request-urllib)
     (pre-process-buffer . twindrill-pre-process-buffer-urllib)))
  "A list of alist of connection methods."
  :group 'twindrill-mode
  :type (twindrill-connection-build-customize-option))

(defvar twindrill-format-status-function-source ""
  "The status format string that has generated the current
`twindrill-format-status-function'.")
(defvar twindrill-format-status-function nil
  "The formating function generated from `twindrill-format-status-function-source'.")
(defvar twindrill-format-status-function-without-compile nil
  "The formating function generated from `twindrill-format-status-function-source',
which is a lambda expression without being compiled.")

(defvar twindrill-timeline-data-table (make-hash-table :test 'equal))

(defcustom twindrill-username-face 'twindrill-username-face
  "*Face used to display USERNAME."
  :type 'face
  :group 'twindrill-mode)

(defcustom twindrill-uri-face 'twindrill-uri-face
  "*Face used to display URIs."
  :type 'face
  :group 'twindrill-mode)

(defcustom twindrill-use-native-retweet nil
  "*If non-nil, post retweet using native retweets."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-update-status-function
  'twindrill-update-status-from-pop-up-buffer
  "*The function which is used to post a tweet.

It takes the following 5 arguments, INIT-STR, REPLY-TO-ID, USERNAME,
TWEET-TYPE and CURRENT-SPEC.
The first argument INIT-STR is nil or an initial text to be edited.
REPLY-TO-ID and USERNAME are an ID and a user-screen-name of a tweet to
which you are going to reply. If the tweet is not a reply, they are nil.
TWEET-TYPE is a symbol specifying a type of a tweet being edited. It must
be one of 'direct-message, 'normal, 'organic-retweet and 'reply.
CURRENT-SPEC means on which timeline the function is called.

Twindrill-mode provides two functions for updating status:
* `twindrill-update-status-from-minibuffer': edit tweets in minibuffer
* `twindrill-update-status-from-pop-up-buffer': edit tweets in pop-up buffer"
  :type '(choice  (const :tag "built-in: from minibuffer"
			 twindrill-update-status-from-minibuffer)
		  (const :tag "built-in: from a popup buffer"
			 twindrill-update-status-from-pop-up-buffer)
		  (function :tag "Your own function"))
  :group 'twindrill-mode)

(defcustom twindrill-request-confirmation-on-posting nil
  "*If non-nil, confirmation will be requested on posting a tweet edited in
pop-up buffer."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-request-confirmation-on-favouriting nil
  "*If non-nil, confirmation will be requested on favorite a tweet."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-use-master-password nil
  "*If non-nil, store private information encrypted with a master password."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-private-info-file (expand-file-name "~/.twindrill-mode.gpg")
  "*File for storing encrypted private information.

 Only used when `twindrill-use-master-password' is non-nil."
  :group 'twindrill-mode
  :type 'file)

(defvar twindrill-private-info-file-loaded nil
  "Whether the private info file has been loaded or not.")
(defvar twindrill-variables-stored-with-encryption
  '(twindrill-oauth-access-token-alist))

(defvar twindrill-api-prefix "1/")
(defvar twindrill-search-api-method "search")
(defvar twindrill-web-path-prefix "")

(defconst twindrill-service-method-table
  '((twitter (status-url . twindrill-get-status-url-twitter)
	     (search-url . twindrill-get-search-url-twitter))
    (twitter-api-v1.1
     (status-url . twindrill-get-status-url-twitter)
     (search-url . twindrill-get-search-url-twitter))
    (statusnet (status-url . twindrill-get-status-url-statusnet)
	       (search-url . twindrill-get-search-url-statusnet)))
  "A list of alist of service methods.")

(defcustom twindrill-service-method 'twitter-api-v1.1
  "*Service method for `twindrill-mode'.

The symbol `twitter' means Twitter Service.
The symbol `statusnet' means StatusNet Service.

Default to `twitter-api-v1.1' which is an alias for `twitter'.

See also `twindrill-service-method-table'."
  :type (if (> (length (mapcar #'car twindrill-service-method-table)) 0)
	    `(choice ,@(mapcar (lambda (entry) `(const ,(car entry)))
			       twindrill-service-method-table))
	  'symbol)
  :group 'twindrill-mode)

(defcustom twindrill-timeline-header-face 'twindrill-timeline-header-face
  "*Face for the header on `twindrill-mode'.

The face is used for rendering `twindrill-timeline-header'."
  :type 'face
  :group 'twindrill-mode)

(defcustom twindrill-timeline-footer-face 'twindrill-timeline-footer-face
  "*Face for the footer on `twindrill-mode'.

The face is used for rendering `twindrill-timeline-footer'."
  :type 'face
  :group 'twindrill-mode)

(defcustom twindrill-timeline-header "-- Press Enter here to update --\n"
  "*Timeline header string on `twindrill-mode'.

The string is rendered on the beginning of a `twindrill-mode' buffer.
Its face is specified by `twindrill-timeline-header-face'."
  :type 'string
  :group 'twindrill-mode)

(defcustom twindrill-timeline-footer "-- Press Enter here to update --"
  "*Timeline footer string on `twindrill-mode'.

The string is rendered on the end of a `twindrill-mode' buffer.
Its face is specified by `twindrill-timeline-footer-face'."
  :type 'string
  :group 'twindrill-mode)

(defcustom twindrill-pop-to-buffer-function
  'twindrill-pop-to-buffer-in-bottom-largest-window
  "*Function being invoked by `twindrill-pop-to-buffer'.

It will receive an argument, the buffer being selected.
For example, the following functions can be used; `pop-to-buffer',
`twindrill-pop-to-buffer-simple',
`twindrill-pop-to-buffer-in-current-window',
`twindrill-pop-to-buffer-in-largest-window', and
`twindrill-pop-to-buffer-in-bottom-largest-window'."
  :type 'function
  :group 'twindrill-mode)

;; FIXME: change to something better than alist
(defcustom twindrill-relative-retrieval-interval-alist
  '(("\\`:direct.*\\'" 4)
    (":home" ":mentions" 1)
    (t 1))
  "*An alist of relative intervals of retrieving timelines.
Each element looks like (TIMELINE-SPEC-REGEXP RELATIVE-INTERVAL).

TIMELINE-SPEC-REGEXP must be t or a regexp string specifying primary
timeline specs.
If TIMELINE-SPEC-REGEXP is t, it matches all timelines.
RELATIVE-INTERVAL must be zero or a positive integer specifying relative
interval of retrieving timelines that match TIMELINE-SPEC-REGEXP.

An interval for a timeline is determined as follows;
1. Find the first element where TIMELINE-SPEC-REGEXP matches the
   timeline or TIMELINE-SPEC-REGEXP is t.
   If no elements are found, the interval is `twindrill-timer-interval'.
2. Check the RELATIVE-INTERVAL of the element.
   If RELATIVE-INTERVAL is a positive integer, the interval is
   RELATIVE-INTERVAL times as long as `twindrill-timer-interval'.

   If RELATIVE-INTERVAL is zero, the interval is infinity.
   The timeline is not retrieved automatically."
  :type 'alist
  :group 'twindrill-mode)

(defvar twindrill-relative-retrieval-count-alist '()
  "An alist for counting retrieval of primary timelines.")

(defvar twindrill-filter-alist '()
  "*An alist of hidden tweet patterns for each primary timeline.
Each element looks like:
 (TIMELINE-SPECIFIER (SYM1 . REGEXP1) (SYM2 . REGEXP2) ...).

TIMELINE-SPECIFIER must be a string or a list of strings.
Each string is a regexp for specifying primary timelines.
Note that you cannot specify composite timelines such as \":merge\",
\":exclude-if\" or \":exclude-re\".
Following regexps (REGEXP1, REGEXP2, ...) specify which tweet should
be hidden in a certain timeline.

In a timeline that matches TIMELINE-SPECIFIER, a tweet is hidden if
its elements specified by SYM1, SYM2, ... match corresponding REGEXP1, REGEXP2,
... respectively.

If a timeline matches multiple specifiers, all regexps of matched elements
are effective.

For example, if you specify
 '(((\":home\" \":mentions\") (text . \"http://\"))
   (\"^[^:]\" (text . \"sample\") (user-screen-name . \"\\`FOO\\'\"))
   (\"twitter/.*\" (text . \"^aa\"))),
the following tweets are hidden.

- tweets including \"http://\" in the home timeline and the mentions timeline,
- tweets that are posted by the user FOO and include \"sample\"
  in user timelines and list timelines,
- tweets including \"aa\" at a beginning of a line in list timelines of
  twitter, such as \"twitter/media\" or \"twitter/support\".")

;;;;
;;;; Proxy setting / functions
;;;;

(defgroup twindrill-proxy nil
  "Subgroup handling `twindrill-mode' proxy setup."
  :group 'twindrill-mode)

(defcustom twindrill-proxy-use nil
  "*If non-nil, use PROXY.

See also `twindrill-proxy-server' for documentation."
  :type 'boolean
  :group 'twindrill-mode)

(defcustom twindrill-proxy-server nil
  "*Proxy server for `twindrill-mode'.

If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twindrill-proxy-server'
and `twindrill-proxy-port' must be nil."
  :group 'twindrill-proxy
  :type '(choice (const nil) string))

(defcustom twindrill-proxy-port nil
  "*Port number for `twindrill-mode'.

If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twindrill-proxy-server'
and `twindrill-proxy-port' must be nil."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 integer))

(defvar twindrill-proxy-keep-alive nil)
(defcustom twindrill-proxy-user nil
  "*Username for `twindrill-proxy-server'.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS.")

(defcustom twindrill-proxy-password nil
  "*Password for `twindrill-proxy-server'.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 string))

(defcustom twindrill-http-proxy-server nil
  "*HTTP proxy server for `twindrill-mode'.
If nil, it is initialized on entering `twindrill-mode'.
The port number is specified by `twindrill-http-proxy-port'.
For HTTPS connection, the proxy specified by `twindrill-https-proxy-server'
and `twindrill-https-proxy-port' is used.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 string))

(defcustom twindrill-http-proxy-port nil
  "*Port number of a HTTP proxy server for `twindrill-mode'.
If nil, it is initialized on entering `twindrill-mode'.
The server is specified by `twindrill-http-proxy-server'.
For HTTPS connection, the proxy specified by `twindrill-https-proxy-server'
and `twindrill-https-proxy-port' is used.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 integer))

(defcustom twindrill-http-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled.  This is experimental."
  :group 'twindrill-proxy
  :type 'boolean)

(defcustom twindrill-http-proxy-user nil
  "*Username for `twindrill-http-proxy-server'.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 string))

(defcustom twindrill-http-proxy-password nil
  "*Password for `twindrill-http-proxy-server'.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 string))

(defcustom twindrill-https-proxy-server nil
  "*HTTPS proxy server for `twindrill-mode'.
If nil, it is initialized on entering `twindrill-mode'.
The port number is specified by `twindrill-https-proxy-port'.
For HTTP connection, the proxy specified by `twindrill-http-proxy-server'
and `twindrill-http-proxy-port' is used.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 string))

(defcustom twindrill-https-proxy-port nil
  "*Port number of a HTTPS proxy server for `twindrill-mode'.
If nil, it is initialized on entering `twindrill-mode'.
The server is specified by `twindrill-https-proxy-server'.
For HTTP connection, the proxy specified by `twindrill-http-proxy-server'
and `twindrill-http-proxy-port' is used.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 integer))

(defcustom twindrill-https-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled.  This is experimental."
  :group 'twindrill-proxy
  :type 'boolean)

(defcustom twindrill-https-proxy-user nil
  "*Username for `twindrill-https-proxy-server'.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 string))

(defcustom twindrill-https-proxy-password nil
  "*Password for `twindrill-https-proxy-server'.

NOTE: If both `twindrill-proxy-server' and `twindrill-proxy-port' are
non-nil, the variables `twindrill-proxy-*' have priority over other
variables `twindrill-http-proxy-*' or `twindrill-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twindrill-proxy
  :type '(choice (const nil)
		 string))

(defun twindrill-normalize-proxy-vars ()
  "Normalize the type of `twindrill-http-proxy-port' and
`twindrill-https-proxy-port'."
  (mapc (lambda (sym)
	  (let ((value (symbol-value sym)))
	    (cond
	     ((null value)
	      nil)
	     ((integerp value)
	      nil)
	     ((stringp value)
	      (set sym (string-to-number value)))
	     (t
	      (set sym nil)))))
	'(twindrill-proxy-port
	  twindrill-http-proxy-port
	  twindrill-https-proxy-port)))

(defun twindrill-proxy-info (scheme &optional item)
  "Return an alist for proxy configuration registered for SCHEME.
SCHEME must be a string \"http\", \"https\" or a symbol 'http or 'https.
The server name is a string and the port number is an integer."
  (twindrill-normalize-proxy-vars)
  (let ((scheme (if (symbolp scheme)
		    (symbol-name scheme)
		  scheme))
	(info-list
	 `((("http" "https")
	    . ((server . ,twindrill-proxy-server)
	       (port . ,twindrill-proxy-port)
	       (keep-alive . ,twindrill-proxy-keep-alive)
	       (user . ,twindrill-proxy-user)
	       (password . ,twindrill-proxy-password)))
	   (("http")
	    . ((server . ,twindrill-http-proxy-server)
	       (port . ,twindrill-http-proxy-port)
	       (keep-alive . ,twindrill-http-proxy-keep-alive)
	       (user . ,twindrill-http-proxy-user)
	       (password . ,twindrill-http-proxy-password)))
	   (("https")
	    . ((server . ,twindrill-https-proxy-server)
	       (port . ,twindrill-https-proxy-port)
	       (keep-alive . ,twindrill-https-proxy-keep-alive)
	       (user . ,twindrill-https-proxy-user)
	       (password . ,twindrill-https-proxy-password))))))
    (let ((info
	   (car (remove nil
			(mapcar
			 (lambda (entry)
			   (when (member scheme (car entry))
			     (let ((info (cdr entry)))
			       (when (and (cdr (assq 'server info))
					  (cdr (assq 'port info)))
				 info))))
			 info-list)))))
      (if item
	  (cdr (assq item info))
	info))))

(defun twindrill-url-proxy-services ()
  "Return the current proxy configuration for `twindrill-mode' in the format
of `url-proxy-services'."
  (remove nil (mapcar
	       (lambda (scheme)
		 (let ((server (twindrill-proxy-info scheme 'server))
		       (port (twindrill-proxy-info scheme 'port)))
		   (when (and server port)
		     `(,scheme . ,(format "%s:%s" server port)))))
	       '("http" "https"))))

(defun twindrill-find-proxy (scheme)
  "Find proxy server and its port from the environmental variables and return
a cons pair of them.
SCHEME must be \"http\" or \"https\"."
  (cond
   ((require 'url-methods nil t)
    (url-scheme-register-proxy scheme)
    (let* ((proxy-service (assoc scheme url-proxy-services))
	   (proxy (if proxy-service (cdr proxy-service) nil)))
      (if (and proxy
	       (string-match "^\\([^:]+\\):\\([0-9]+\\)$" proxy))
	  (let ((host (match-string 1 proxy))
		(port (string-to-number (match-string 2 proxy))))
	    (cons host port))
	nil)))
   (t
    (let* ((env-var (concat scheme "_proxy"))
	   (env-proxy (or (getenv (upcase env-var))
			  (getenv (downcase env-var))))
	   (default-port (if (string= "https" scheme) "443" "80")))
      (if (and env-proxy
	       (string-match
		"^\\(https?://\\)?\\([^:/]+\\)\\(:\\([0-9]+\\)\\)?/?$"
		env-proxy))
	  (let* ((host (match-string 2 env-proxy))
		 (port-str (or (match-string 4 env-proxy) default-port))
		 (port (string-to-number port-str)))
	    (cons host port))
	nil)))))

(defun twindrill-setup-proxy ()
  (when (require 'url-methods nil t)
    ;; If `url-scheme-registry' is not initialized,
    ;; `url-proxy-services' will be reset by calling
    ;; `url-insert-file-contents' or `url-retrieve-synchronously', etc.
    ;; To avoid it, initialize `url-scheme-registry' by calling
    ;; `url-scheme-get-property' before calling such functions.
    (url-scheme-get-property "http" 'name)
    (url-scheme-get-property "https" 'name))
  (unless (and twindrill-http-proxy-server
	       twindrill-http-proxy-port)
    (let ((info (twindrill-find-proxy "http")))
      (setq twindrill-http-proxy-server (car-safe info))
      (setq twindrill-http-proxy-port (cdr-safe info))))
  (unless (and twindrill-https-proxy-server
	       twindrill-https-proxy-port)
    (let ((info (twindrill-find-proxy "https")))
      (setq twindrill-https-proxy-server (car-safe info))
      (setq twindrill-https-proxy-port (cdr-safe info))))
  (if (and twindrill-proxy-use
	   (null (twindrill-proxy-info "http"))
	   (null (twindrill-proxy-info "https")))
      (progn
	(message "Disabling proxy due to lack of configuration.")
	(setq twindrill-proxy-use nil))
    t))

(defun twindrill-toggle-proxy ()
  (interactive)
  (setq twindrill-proxy-use
	(not twindrill-proxy-use))
  (if (twindrill-setup-proxy)
      (message (if twindrill-proxy-use "Use Proxy:on" "Use Proxy:off")))
  (twindrill-update-mode-line))

;;;;
;;;; Functions for URL library
;;;;

(defcustom twindrill-url-show-status nil
  "*If non-nil, show a running total of bytes transferred by urllib.

This has effect only if either \"urllib-httpp\" or \"urllib-https\" is used
as the connection method."
  :group 'twindrill-mode
  :type 'boolean)

;;;;
;;;; CA certificate
;;;;

(defvar twindrill-cert-file nil
  "The full-path of the file including the certificates authorizing
servers on SSL.")

(defconst twindrill-ca-cert-list
  '(
;; #BEGIN-CERTIFICATE
;; Equifax Secure CA
;; issuer= /C=US/O=Equifax/OU=Equifax Secure Certificate Authority
;; subject= /C=US/O=Equifax/OU=Equifax Secure Certificate Authority
;; serial=35DEF4CF
;; SHA1 Fingerprint=D2:32:09:AD:23:D3:14:23:21:74:E4:0D:7F:9D:62:13:97:86:63:3A
;; notBefore=Aug 22 16:41:51 1998 GMT
;; notAfter=Aug 22 16:41:51 2018 GMT
"-----BEGIN CERTIFICATE-----
MIIDIDCCAomgAwIBAgIENd70zzANBgkqhkiG9w0BAQUFADBOMQswCQYDVQQGEwJV
UzEQMA4GA1UEChMHRXF1aWZheDEtMCsGA1UECxMkRXF1aWZheCBTZWN1cmUgQ2Vy
dGlmaWNhdGUgQXV0aG9yaXR5MB4XDTk4MDgyMjE2NDE1MVoXDTE4MDgyMjE2NDE1
MVowTjELMAkGA1UEBhMCVVMxEDAOBgNVBAoTB0VxdWlmYXgxLTArBgNVBAsTJEVx
dWlmYXggU2VjdXJlIENlcnRpZmljYXRlIEF1dGhvcml0eTCBnzANBgkqhkiG9w0B
AQEFAAOBjQAwgYkCgYEAwV2xWGcIYu6gmi0fCG2RFGiYCh7+2gRvE4RiIcPRfM6f
BeC4AfBONOziipUEZKzxa1NfBbPLZ4C/QgKO/t0BCezhABRP/PvwDN1Dulsr4R+A
cJkVV5MW8Q+XarfCaCMczE1ZMKxRHjuvK9buY0V7xdlfUNLjUA86iOe/FP3gx7kC
AwEAAaOCAQkwggEFMHAGA1UdHwRpMGcwZaBjoGGkXzBdMQswCQYDVQQGEwJVUzEQ
MA4GA1UEChMHRXF1aWZheDEtMCsGA1UECxMkRXF1aWZheCBTZWN1cmUgQ2VydGlm
aWNhdGUgQXV0aG9yaXR5MQ0wCwYDVQQDEwRDUkwxMBoGA1UdEAQTMBGBDzIwMTgw
ODIyMTY0MTUxWjALBgNVHQ8EBAMCAQYwHwYDVR0jBBgwFoAUSOZo+SvSspXXR9gj
IBBPM5iQn9QwHQYDVR0OBBYEFEjmaPkr0rKV10fYIyAQTzOYkJ/UMAwGA1UdEwQF
MAMBAf8wGgYJKoZIhvZ9B0EABA0wCxsFVjMuMGMDAgbAMA0GCSqGSIb3DQEBBQUA
A4GBAFjOKer89961zgK5F7WF0bnj4JXMJTENAKaSbn+2kmOeUJXRmm/kEd5jhW6Y
7qj/WsjTVbJmcVfewCHrPSqnI0kBBIZCe/zuf6IWUrVnZ9NA2zsmWLIodz2uFHdh
1voqZiegDfqnc1zqcPGUIWVEX/r87yloqaKHee9570+sB3c4
-----END CERTIFICATE-----
"
;; Verisign Class 3 Public Primary Certification Authority - G2
;; issuer= /C=US/O=VeriSign, Inc./OU=Class 3 Public Primary Certification Authority - G2/OU=(c) 1998 VeriSign, Inc. - For authorized use only/OU=VeriSign Trust Network
;; subject= /C=US/O=VeriSign, Inc./OU=Class 3 Public Primary Certification Authority - G2/OU=(c) 1998 VeriSign, Inc. - For authorized use only/OU=VeriSign Trust Network
;; serial=7DD9FE07CFA81EB7107967FBA78934C6
;; SHA1 Fingerprint=85:37:1C:A6:E5:50:14:3D:CE:28:03:47:1B:DE:3A:09:E8:F8:77:0F
;; notBefore=May 18 00:00:00 1998 GMT
;; notAfter=Aug  1 23:59:59 2028 GMT
"-----BEGIN CERTIFICATE-----
MIIDAjCCAmsCEH3Z/gfPqB63EHln+6eJNMYwDQYJKoZIhvcNAQEFBQAwgcExCzAJ
BgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoGA1UECxMzQ2xh
c3MgMyBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAtIEcy
MTowOAYDVQQLEzEoYykgMTk5OCBWZXJpU2lnbiwgSW5jLiAtIEZvciBhdXRob3Jp
emVkIHVzZSBvbmx5MR8wHQYDVQQLExZWZXJpU2lnbiBUcnVzdCBOZXR3b3JrMB4X
DTk4MDUxODAwMDAwMFoXDTI4MDgwMTIzNTk1OVowgcExCzAJBgNVBAYTAlVTMRcw
FQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoGA1UECxMzQ2xhc3MgMyBQdWJsaWMg
UHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAtIEcyMTowOAYDVQQLEzEo
YykgMTk5OCBWZXJpU2lnbiwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5
MR8wHQYDVQQLExZWZXJpU2lnbiBUcnVzdCBOZXR3b3JrMIGfMA0GCSqGSIb3DQEB
AQUAA4GNADCBiQKBgQDMXtERXVxp0KvTuWpMmR9ZmDCOFoUgRm1HP9SFIIThbbP4
pO0M8RcPO/mn+SXXwc+EY/J8Y8+iR/LGWzOOZEAEaMGAuWQcRXfH2G71lSk8UOg0
13gfqLptQ5GVj0VXXn7F+8qkBOvqlzdUMG+7AUcyM83cV5tkaWH4mx0ciU9cZwID
AQABMA0GCSqGSIb3DQEBBQUAA4GBAFFNzb5cy5gZnBWyATl4Lk0PZ3BwmcYQWpSk
U01UbSuvDV1Ai2TT1+7eVmGSX6bEHRBhNtMsJzzoKQm5EWR0zLVznxxIqbxhAe7i
F6YM40AIOw7n60RzKprxaZLvcRTDOaxxp5EJb+RxBrO6WVcmeQD2+A2iMzAo1KpY
oJ2daZH9
-----END CERTIFICATE-----
"
;; Verisign Class 3 Public Primary Certification Authority - G3
;; issuer= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 1999 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G3
;; subject= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 1999 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G3
;; serial=9B7E0649A33E62B9D5EE90487129EF57
;; SHA1 Fingerprint=13:2D:0D:45:53:4B:69:97:CD:B2:D5:C3:39:E2:55:76:60:9B:5C:C6
;; notBefore=Oct  1 00:00:00 1999 GMT
;; notAfter=Jul 16 23:59:59 2036 GMT
"-----BEGIN CERTIFICATE-----
MIIEGjCCAwICEQCbfgZJoz5iudXukEhxKe9XMA0GCSqGSIb3DQEBBQUAMIHKMQsw
CQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZl
cmlTaWduIFRydXN0IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAxOTk5IFZlcmlTaWdu
LCBJbmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxRTBDBgNVBAMTPFZlcmlT
aWduIENsYXNzIDMgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3Jp
dHkgLSBHMzAeFw05OTEwMDEwMDAwMDBaFw0zNjA3MTYyMzU5NTlaMIHKMQswCQYD
VQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZlcmlT
aWduIFRydXN0IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAxOTk5IFZlcmlTaWduLCBJ
bmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxRTBDBgNVBAMTPFZlcmlTaWdu
IENsYXNzIDMgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkg
LSBHMzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMu6nFL8eB8aHm8b
N3O9+MlrlBIwT/A2R/XQkQr1F8ilYcEWQE37imGQ5XYgwREGfassbqb1EUGO+i2t
KmFZpGcmTNDovFJbcCAEWNF6yaRpvIMXZK0Fi7zQWM6NjPXr8EJJC52XJ2cybuGu
kxUccLwgTS8Y3pKI6GyFVxEa6X7jJhFUokWWVYPKMIno3Nij7SqAP395ZVc+FSBm
CC+Vk7+qRy+oRpfwEuL+wgorUeZ25rdGt+INpsyow0xZVYnm6FNcHOqd8GIWC6fJ
Xwzw3sJ2zq/3avL6QaaiMxTJ5Xpj055iN9WFZZ4O5lMkdBteHRJTW8cs54NJOxWu
imi5V5cCAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAERSWwauSCPc/L8my/uRan2Te
2yFPhpk0djZX3dAVL8WtfxUfN2JzPtTnX84XA9s1+ivbrmAJXx5fj267Cz3qWhMe
DGBvtcC1IyIuBwvLqXTLR7sdwdela8wv0kL9Sd2nic9TutoAWii/gt/4uhMdUIaC
/Y4wjylGsB49Ndo4YhYYSq3mtlFs3q9i6wHQHiT+eo8SGhJouPtmmRQURVyu565p
F4ErWjfJXir0xuKhXFSbplQAz/DxwceYMBo7Nhbbo27q/a2ywtrvAkcTisDxszGt
TxzhT5yvDwyd93gN2PQ1VoDat20Xj50egWTh/sVFuq1ruQp6Tk9LhO5L8X3dEQ==
-----END CERTIFICATE-----
"
;; Verisign Class 4 Public Primary Certification Authority - G3
;; issuer= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 1999 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 4 Public Primary Certification Authority - G3
;; subject= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 1999 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 4 Public Primary Certification Authority - G3
;; serial=ECA0A78B6E756A01CFC47CCC2F945ED7
;; SHA1 Fingerprint=C8:EC:8C:87:92:69:CB:4B:AB:39:E9:8D:7E:57:67:F3:14:95:73:9D
;; notBefore=Oct  1 00:00:00 1999 GMT
;; notAfter=Jul 16 23:59:59 2036 GMT
"-----BEGIN CERTIFICATE-----
MIIEGjCCAwICEQDsoKeLbnVqAc/EfMwvlF7XMA0GCSqGSIb3DQEBBQUAMIHKMQsw
CQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZl
cmlTaWduIFRydXN0IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAxOTk5IFZlcmlTaWdu
LCBJbmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxRTBDBgNVBAMTPFZlcmlT
aWduIENsYXNzIDQgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3Jp
dHkgLSBHMzAeFw05OTEwMDEwMDAwMDBaFw0zNjA3MTYyMzU5NTlaMIHKMQswCQYD
VQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZlcmlT
aWduIFRydXN0IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAxOTk5IFZlcmlTaWduLCBJ
bmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxRTBDBgNVBAMTPFZlcmlTaWdu
IENsYXNzIDQgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkg
LSBHMzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAK3LpRFpxlmr8Y+1
GQ9Wzsy1HyDkniYlS+BzZYlZ3tCD5PUPtbut8XzoIfzk6AzufEUiGXaStBO3IFsJ
+mGuqPKljYXCKtbeZjbSmwL0qJJgfJxptI8kHtCGUvYynEFYHiK9zUVilQhu0Gbd
U6LM8BDcVHOLBKFGMzNcF0C5nk3T875Vg+ixiY5afJqWIpA7iCXy0lOIAgwLePLm
NxdLMEYH5IBtptiWLugs+BGzOA1mppvqySNb247i8xOOGlktqgLw7KSHZtzBP/XY
ufTsgsbSPZUd5cBPhMnZo0QoBmrXRazwa2rvTl/4EYIeOGM0ZlDUPpNz+jDDZq3/
ky2X7wMCAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAj/ola09b5KROJ1WrIhVZPMq1
CtRK26vdoV9TxaBXOcLORyu+OshWv8LZJxA6sQU8wHcxuzrTBXttmhwwjIDLk5Mq
g6sFUYICABFna/OIYUdfA5PVWw3g8dShMjWFsjrbsIKr0csKvE+MW8VLADsfKoKm
fjaF3H48ZwC15DtS4KjrXRX5xm3wrR0OhbepmnMUWluPQSjA1egtTaRezarZ7c7c
2NU8Qh0XwRJdRTjDOPP8hS6DRkiy1yBfkjaP53kPmF6Z6PDQpLv1U70qzlmwr25/
bLvSHgCwIe34QWKCudiyxLtGUPMxxY8BqHTr9Xgn2uf3ZkPznoM+IKrDNWCRzg==
-----END CERTIFICATE-----
"
;; Equifax Secure Global eBusiness CA
;; issuer= /C=US/O=Equifax Secure Inc./CN=Equifax Secure Global eBusiness CA-1
;; subject= /C=US/O=Equifax Secure Inc./CN=Equifax Secure Global eBusiness CA-1
;; serial=01
;; SHA1 Fingerprint=7E:78:4A:10:1C:82:65:CC:2D:E1:F1:6D:47:B4:40:CA:D9:0A:19:45
;; notBefore=Jun 21 04:00:00 1999 GMT
;; notAfter=Jun 21 04:00:00 2020 GMT
"-----BEGIN CERTIFICATE-----
MIICkDCCAfmgAwIBAgIBATANBgkqhkiG9w0BAQQFADBaMQswCQYDVQQGEwJVUzEc
MBoGA1UEChMTRXF1aWZheCBTZWN1cmUgSW5jLjEtMCsGA1UEAxMkRXF1aWZheCBT
ZWN1cmUgR2xvYmFsIGVCdXNpbmVzcyBDQS0xMB4XDTk5MDYyMTA0MDAwMFoXDTIw
MDYyMTA0MDAwMFowWjELMAkGA1UEBhMCVVMxHDAaBgNVBAoTE0VxdWlmYXggU2Vj
dXJlIEluYy4xLTArBgNVBAMTJEVxdWlmYXggU2VjdXJlIEdsb2JhbCBlQnVzaW5l
c3MgQ0EtMTCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAuucXkAJlsTRVPEnC
UdXfp9E3j9HngXNBUmCbnaEXJnitx7HoJpQytd4zjTov2/KaelpzmKNc6fuKcxtc
58O/gGzNqfTWK8D3+ZmqY6KxRwIP1ORROhI8bIpaVIRw28HFkM9yRcuoWcDNM50/
o5brhTMhHD4ePmBudpxnhcXIw2ECAwEAAaNmMGQwEQYJYIZIAYb4QgEBBAQDAgAH
MA8GA1UdEwEB/wQFMAMBAf8wHwYDVR0jBBgwFoAUvqigdHJQa0S3ySPY+6j/s1dr
aGwwHQYDVR0OBBYEFL6ooHRyUGtEt8kj2Puo/7NXa2hsMA0GCSqGSIb3DQEBBAUA
A4GBADDiAVGqx+pf2rnQZQ8w1j7aDRRJbpGTJxQx78T3LUX47Me/okENI7SS+RkA
Z70Br83gcfxaz2TE4JaY0KNA4gGK7ycH8WUBikQtBmV1UsCGECAhX2xrD2yuCRyv
8qIYNMR1pHMc8Y3c7635s3a0kr/clRAevsvIO1qEYBlWlKlV
-----END CERTIFICATE-----
"
;; Equifax Secure eBusiness CA 1
;; issuer= /C=US/O=Equifax Secure Inc./CN=Equifax Secure eBusiness CA-1
;; subject= /C=US/O=Equifax Secure Inc./CN=Equifax Secure eBusiness CA-1
;; serial=04
;; SHA1 Fingerprint=DA:40:18:8B:91:89:A3:ED:EE:AE:DA:97:FE:2F:9D:F5:B7:D1:8A:41
;; notBefore=Jun 21 04:00:00 1999 GMT
;; notAfter=Jun 21 04:00:00 2020 GMT
"-----BEGIN CERTIFICATE-----
MIICgjCCAeugAwIBAgIBBDANBgkqhkiG9w0BAQQFADBTMQswCQYDVQQGEwJVUzEc
MBoGA1UEChMTRXF1aWZheCBTZWN1cmUgSW5jLjEmMCQGA1UEAxMdRXF1aWZheCBT
ZWN1cmUgZUJ1c2luZXNzIENBLTEwHhcNOTkwNjIxMDQwMDAwWhcNMjAwNjIxMDQw
MDAwWjBTMQswCQYDVQQGEwJVUzEcMBoGA1UEChMTRXF1aWZheCBTZWN1cmUgSW5j
LjEmMCQGA1UEAxMdRXF1aWZheCBTZWN1cmUgZUJ1c2luZXNzIENBLTEwgZ8wDQYJ
KoZIhvcNAQEBBQADgY0AMIGJAoGBAM4vGbwXt3fek6lfWg0XTzQaDJj0ItlZ1MRo
RvC0NcWFAyDGr0WlIVFFQesWWDYyb+JQYmT5/VGcqiTZ9J2DKocKIdMSODRsjQBu
WqDZQu4aIZX5UkxVWsUPOE9G+m34LjXWHXzr4vCwdYDIqROsvojvOm6rXyo4YgKw
Env+j6YDAgMBAAGjZjBkMBEGCWCGSAGG+EIBAQQEAwIABzAPBgNVHRMBAf8EBTAD
AQH/MB8GA1UdIwQYMBaAFEp4MlIR21kWNl7fwRQ2QGpHfEyhMB0GA1UdDgQWBBRK
eDJSEdtZFjZe38EUNkBqR3xMoTANBgkqhkiG9w0BAQQFAAOBgQB1W6ibAxHm6VZM
zfmpTMANmvPMZWnmJXbMWbfWVMMdzZmsGd20hdXgPfxiIKeES1hl8eL5lSE/9dR+
WB5Hh1Q+WKG1tfgq73HnvMP2sUlG4tega+VWeponmHxGYhTnyfxuAxJ5gDgdSIKN
/Bf+KpYrtWKmpj29f5JZzVoqgrI3eQ==
-----END CERTIFICATE-----
"
;; VeriSign Class 3 Public Primary Certification Authority - G5
;; issuer= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2006 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G5
;; subject= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2006 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G5
;; serial=18DAD19E267DE8BB4A2158CDCC6B3B4A
;; SHA1 Fingerprint=4E:B6:D5:78:49:9B:1C:CF:5F:58:1E:AD:56:BE:3D:9B:67:44:A5:E5
;; notBefore=Nov  8 00:00:00 2006 GMT
;; notAfter=Jul 16 23:59:59 2036 GMT
"-----BEGIN CERTIFICATE-----
MIIE0zCCA7ugAwIBAgIQGNrRniZ96LtKIVjNzGs7SjANBgkqhkiG9w0BAQUFADCB
yjELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQL
ExZWZXJpU2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwNiBWZXJp
U2lnbiwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MUUwQwYDVQQDEzxW
ZXJpU2lnbiBDbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0
aG9yaXR5IC0gRzUwHhcNMDYxMTA4MDAwMDAwWhcNMzYwNzE2MjM1OTU5WjCByjEL
MAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQLExZW
ZXJpU2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwNiBWZXJpU2ln
biwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MUUwQwYDVQQDEzxWZXJp
U2lnbiBDbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9y
aXR5IC0gRzUwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCvJAgIKXo1
nmAMqudLO07cfLw8RRy7K+D+KQL5VwijZIUVJ/XxrcgxiV0i6CqqpkKzj/i5Vbex
t0uz/o9+B1fs70PbZmIVYc9gDaTY3vjgw2IIPVQT60nKWVSFJuUrjxuf6/WhkcIz
SdhDY2pSS9KP6HBRTdGJaXvHcPaz3BJ023tdS1bTlr8Vd6Gw9KIl8q8ckmcY5fQG
BO+QueQA5N06tRn/Arr0PO7gi+s3i+z016zy9vA9r911kTMZHRxAy3QkGSGT2RT+
rCpSx4/VBEnkjWNHiDxpg8v+R70rfk/Fla4OndTRQ8Bnc+MUCH7lP59zuDMKz10/
NIeWiu5T6CUVAgMBAAGjgbIwga8wDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8E
BAMCAQYwbQYIKwYBBQUHAQwEYTBfoV2gWzBZMFcwVRYJaW1hZ2UvZ2lmMCEwHzAH
BgUrDgMCGgQUj+XTGoasjY5rw8+AatRIGCx7GS4wJRYjaHR0cDovL2xvZ28udmVy
aXNpZ24uY29tL3ZzbG9nby5naWYwHQYDVR0OBBYEFH/TZafC3ey78DAJ80M5+gKv
MzEzMA0GCSqGSIb3DQEBBQUAA4IBAQCTJEowX2LP2BqYLz3q3JktvXf2pXkiOOzE
p6B4Eq1iDkVwZMXnl2YtmAl+X6/WzChl8gGqCBpH3vn5fJJaCGkgDdk+bW48DW7Y
5gaRQBi5+MHt39tBquCWIMnNZBU4gcmU7qKEKQsTb47bDN0lAtukixlE0kF6BWlK
WE9gyn6CagsCqiUXObXbf+eEZSqVir2G3l6BFoMtEMze/aiCKm0oHw0LxOXnGiYZ
4fQRbxC1lfznQgUy286dUV4otp6F01vvpX1FQHKOtw5rDgb7MzVIcbidJ4vEZV8N
hnacRHr2lVz2XTIIM6RUthg/aFzyQkqFOFSDX9HoLPKsEdao7WNq
-----END CERTIFICATE-----
"
;; VeriSign Universal Root Certification Authority
;; issuer= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2008 VeriSign, Inc. - For authorized use only/CN=VeriSign Universal Root Certification Authority
;; subject= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2008 VeriSign, Inc. - For authorized use only/CN=VeriSign Universal Root Certification Authority
;; serial=401AC46421B31321030EBBE4121AC51D
;; SHA1 Fingerprint=36:79:CA:35:66:87:72:30:4D:30:A5:FB:87:3B:0F:A7:7B:B7:0D:54
;; notBefore=Apr  2 00:00:00 2008 GMT
;; notAfter=Dec  1 23:59:59 2037 GMT
"-----BEGIN CERTIFICATE-----
MIIEuTCCA6GgAwIBAgIQQBrEZCGzEyEDDrvkEhrFHTANBgkqhkiG9w0BAQsFADCB
vTELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQL
ExZWZXJpU2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwOCBWZXJp
U2lnbiwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MTgwNgYDVQQDEy9W
ZXJpU2lnbiBVbml2ZXJzYWwgUm9vdCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eTAe
Fw0wODA0MDIwMDAwMDBaFw0zNzEyMDEyMzU5NTlaMIG9MQswCQYDVQQGEwJVUzEX
MBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZlcmlTaWduIFRydXN0
IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAyMDA4IFZlcmlTaWduLCBJbmMuIC0gRm9y
IGF1dGhvcml6ZWQgdXNlIG9ubHkxODA2BgNVBAMTL1ZlcmlTaWduIFVuaXZlcnNh
bCBSb290IENlcnRpZmljYXRpb24gQXV0aG9yaXR5MIIBIjANBgkqhkiG9w0BAQEF
AAOCAQ8AMIIBCgKCAQEAx2E3XrEBNNti1xWb/1hajCMj1mCOkdeQmIN65lgZOIzF
9uVkhbSicfvtvbnazU0AtMgtc6XHaXGVHzk8skQHnOgO+k1KxCHfKWGPMiJhgsWH
H26MfF8WIFFE0XBPV+rjHOPMee5Y2A7Cs0WTwCznmhcrewA3ekEzeOEz4vMQGn+H
LL729fdC4uW/h2KJXwBL38Xd5HVEMkE6HnFuacsLdUYI0crSK5XQz/u5QGtkjFdN
/BMReYTtXlT2NJ8IAfMQJQYXStrxHXpma5hgZqTZ79IugvHw7wnqRMkVauIDbjPT
rJ9VAMf2CGqUuV/c4DPxhGD5WycRtPwW8rtWaoAljQIDAQABo4GyMIGvMA8GA1Ud
EwEB/wQFMAMBAf8wDgYDVR0PAQH/BAQDAgEGMG0GCCsGAQUFBwEMBGEwX6FdoFsw
WTBXMFUWCWltYWdlL2dpZjAhMB8wBwYFKw4DAhoEFI/l0xqGrI2Oa8PPgGrUSBgs
exkuMCUWI2h0dHA6Ly9sb2dvLnZlcmlzaWduLmNvbS92c2xvZ28uZ2lmMB0GA1Ud
DgQWBBS2d/ppSEefUxLVwuoHMnYH0ZcHGTANBgkqhkiG9w0BAQsFAAOCAQEASvj4
sAPmLGd75JR3Y8xuTPl9Dg3cyLk1uXBPY/ok+myDjEedO2Pzmvl2MpWRsXe8rJq+
seQxIcaBlVZaDrHC1LGmWazxY8u4TB1ZkErvkBYoH1quEPuBUDgMbMzxPcP1Y+Oz
4yHJJDnp/RVmRvQbEdBNc6N9Rvk97ahfYtTxP/jgdFcrGJ2BtMQo2pSXpXDrrB2+
BxHw1dvd5Yzw1TKwg+ZX4o+/vqGqvz0dtdQ46tewXDpPaj+PwGZsY6rp2aQW9IHR
lRQOfc2VNNnSj3BzgXucfr2YYdhFh5iQxeuGMMY1v/D/w1WIg0vvBZIGcfK4mJO3
7M2CYfE45k+XmCpajQ==
-----END CERTIFICATE-----
"
;; VeriSign Class 3 Public Primary Certification Authority - G4
;; issuer= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2007 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G4
;; subject= /C=US/O=VeriSign, Inc./OU=VeriSign Trust Network/OU=(c) 2007 VeriSign, Inc. - For authorized use only/CN=VeriSign Class 3 Public Primary Certification Authority - G4
;; serial=2F80FE238C0E220F486712289187ACB3
;; SHA1 Fingerprint=22:D5:D8:DF:8F:02:31:D1:8D:F7:9D:B7:CF:8A:2D:64:C9:3F:6C:3A
;; notBefore=Nov  5 00:00:00 2007 GMT
;; notAfter=Jan 18 23:59:59 2038 GMT
"-----BEGIN CERTIFICATE-----
MIIDhDCCAwqgAwIBAgIQL4D+I4wOIg9IZxIokYesszAKBggqhkjOPQQDAzCByjEL
MAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQLExZW
ZXJpU2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwNyBWZXJpU2ln
biwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MUUwQwYDVQQDEzxWZXJp
U2lnbiBDbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9y
aXR5IC0gRzQwHhcNMDcxMTA1MDAwMDAwWhcNMzgwMTE4MjM1OTU5WjCByjELMAkG
A1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQLExZWZXJp
U2lnbiBUcnVzdCBOZXR3b3JrMTowOAYDVQQLEzEoYykgMjAwNyBWZXJpU2lnbiwg
SW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MUUwQwYDVQQDEzxWZXJpU2ln
biBDbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9yaXR5
IC0gRzQwdjAQBgcqhkjOPQIBBgUrgQQAIgNiAASnVnp8Utpkmw4tXNherJI9/gHm
GUo9FANL+mAnINmDiWn6VMaaGF5VKmTeBvaNSjutEDxlPZCIBIngMGGzrl0Bp3ve
fLK+ymVhAIau2o970ImtTR1ZmkGxvEeA3J5iw/mjgbIwga8wDwYDVR0TAQH/BAUw
AwEB/zAOBgNVHQ8BAf8EBAMCAQYwbQYIKwYBBQUHAQwEYTBfoV2gWzBZMFcwVRYJ
aW1hZ2UvZ2lmMCEwHzAHBgUrDgMCGgQUj+XTGoasjY5rw8+AatRIGCx7GS4wJRYj
aHR0cDovL2xvZ28udmVyaXNpZ24uY29tL3ZzbG9nby5naWYwHQYDVR0OBBYEFLMW
kf3upm7ktS5Jj4d4gYDs5bG1MAoGCCqGSM49BAMDA2gAMGUCMGYhDBgmYFo4e1ZC
4Kf8NoRRkSAsdk1DPcQdhCPQrNZ8NQbOzWm9kA3bbEhCHQ6qQgIxAJw9SDkjOVga
FRJZap7v1VmyHVIsmXHNxynfGyphe3HR3vPA5Q06Sqotp9iGKt0uEA==
-----END CERTIFICATE-----
"
;; #END-CERTIFICATE
))

(defun twindrill-delete-ca-cert ()
  (when (and twindrill-cert-file
	     (file-exists-p twindrill-cert-file))
    (delete-file twindrill-cert-file))
  (setq twindrill-cert-file nil))

(defun twindrill-ensure-ca-cert ()
  "Return a full-path of the file including CA certificates.

If it does not exist, create it. The directory includes root certificates
in \"hash format\". In detail, see verify(1SSL)."
  (unless twindrill-cert-file
    (let ((coding-system-for-write 'iso-safe)
	  (file (make-temp-file "twmode-cacert")))
      (with-temp-file file
	(apply 'insert twindrill-ca-cert-list))
      (setq twindrill-cert-file file)
      (add-hook 'kill-emacs-hook 'twindrill-delete-ca-cert)))
  twindrill-cert-file)

;;;;
;;;; User agent
;;;;

(defvar twindrill-user-agent-function 'twindrill-user-agent-default-function)

(defun twindrill-user-agent-default-function ()
  "Twindrill mode default User-Agent function."
  (format "Emacs/%d.%d Twindrill-mode/%s"
	  emacs-major-version emacs-minor-version
	  twindrill-mode-version))

(defun twindrill-user-agent ()
  "Return User-Agent header string."
  (funcall twindrill-user-agent-function))

;;;;
;;;; Basic HTTP functions (general)
;;;;

(defun twindrill-percent-encode (str &optional coding-system)
  "Encode STR according to Percent-Encoding defined in RFC 3986."
  (twindrill-oauth-url-encode str coding-system))

(defun twindrill-lookup-connection-type (use-ssl &optional order table)
  "Return available entry extracted fron connection type table.
TABLE is connection type table, which is an alist of type symbol and its
item alist, such as
 '((native (check . t)
           (https . twindrill-start-http-session-native-tls-p)
           (start . twindrill-start-http-session-native))
   (curl (check . twindrill-start-http-session-curl-p)
         (https . twindrill-start-http-session-curl-https-p)
         (start . twindrill-start-http-session-curl))) .
ORDER means the priority order of type symbols.
If USE-SSL is nil, the item `https' is ignored.
When the type `curl' has priority and is available for the above table,
the function returns
 '((check . twindrill-start-http-session-curl-p)
   (https . twindrill-start-http-session-curl-https-p)
   (start . twindrill-start-http-session-curl)) ."
  (let ((rest (or order twindrill-connection-type-order))
	(table (or table twindrill-connection-type-table))
	(result nil))
    (while (and rest (null result))
      (let* ((candidate (car rest))
	     (entry (cons `(symbol . ,candidate)
			  (cdr (assq candidate table))))
	     (entry (if (assq 'display-name entry)
			entry
		      (cons `(display-name . ,(symbol-name candidate))
			    entry)))
	     (validate (lambda (item)
			 (let ((v (cdr (assq item entry))))
			   (or (null v) (eq t v) (functionp v)))))
	     (confirm (lambda (item)
			(let ((v (cdr (assq item entry))))
			  (cond
			   ((null v) nil)
			   ((eq t v) t)
			   ((functionp v) (funcall v)))))))
	(if (and (funcall validate 'check)
		 (or (not use-ssl) (funcall validate 'https)))
	    (cond
	     ((and (funcall confirm 'check)
		   (or (not use-ssl) (funcall confirm 'https)))
	      (setq rest nil)
	      (setq result entry))
	     (t
	      (setq rest (cdr rest))))
	  (message "The configuration for conncetion type `%s' is invalid."
		   candidate)
	  (setq rest nil))))
    result))

(defun twindrill-get-connection-method-name (use-ssl)
  "Return a name of the preferred connection method.
If USE-SSL is non-nil, return a connection method for HTTPS.
If USE-SSL is nil, return a connection method for HTTP."
  (cdr (assq 'display-name (twindrill-lookup-connection-type use-ssl))))

(defun twindrill-lookup-http-start-function (&optional order table)
  "Decide a connection method from currently available methods."
  (let ((entry
	 (twindrill-lookup-connection-type twindrill-use-ssl order table)))
    (cdr (assq 'send-http-request entry))))

(defun twindrill-ensure-connection-method (&optional order table)
  "Ensure a connection method with a compromise.
Return nil if no connection methods are available with a compromise."
  (let* ((use-ssl (or twindrill-use-ssl twindrill-oauth-use-ssl))
	 (entry (twindrill-lookup-connection-type use-ssl order table)))
    (cond
     (entry
      t)
     ((and (null entry) use-ssl
	   (yes-or-no-p "HTTPS(SSL) is unavailable. Use HTTP instead? "))
      ;; Fall back on connection without SSL.
      (setq twindrill-use-ssl nil)
      (setq twindrill-oauth-use-ssl nil)
      (twindrill-update-mode-line)
      (twindrill-ensure-connection-method order table))
     (t
      (message "No connection methods are available.")
      nil))))

(defun twindrill-make-http-request (method header-list host port path query-parameters post-body use-ssl)
  "Returns an alist specifying a HTTP request.
METHOD specifies HTTP method. It must be \"GET\" or \"POST\".
HEADER-LIST is a list of (field-name . field-value) specifying HTTP header
fields. The fields \"Host\", \"User-Agent\" and \"Content-Length\" are
automatically filled if necessary.
HOST specifies the host.
PORT specifies the port. This must be an integer.
PATH specifies the absolute path in URI (without query string).
QUERY-PARAMTERS is a string or an alist.
If QUERY-PARAMTERS is a string, it is treated as an encoded query string.
If QUERY-PARAMTERS is an alist, it represents a list of cons pairs of
string, (query-key . query-value).
POST-BODY specifies the post body sent when METHOD equals to \"POST\".
If POST-BODY is nil, no body is posted.
If USE-SSL is non-nil, the request is performed with SSL.

The result alist includes the following keys, where a key is a symbol.
  method: HTTP method such as \"GET\" or \"POST\".
  scheme: the scheme name. \"http\" or \"https\".
  host: the host to which the request is sent.
  port: the port to which the request is sent (integer).
  path: the absolute path string. Note that it does not include query string.
  query-string: the query string.
  encoded-query-alist: the alist consisting of pairs of encoded query-name and
    encoded query-value.
  uri: the URI. It includes the query string.
  uri-without-query: the URI without the query string.
  header-list: an alist specifying pairs of a parameter and its value in HTTP
    header field.
  post-body: the entity that will be posted."
  (let* ((scheme (if use-ssl "https" "http"))
	 (default-port (if use-ssl 443 80))
	 (port (if port port default-port))
	 (query-string
	  (cond
	   ((stringp query-parameters)
	    query-parameters)
	   ((consp query-parameters)
	    (mapconcat (lambda (pair)
			 (cond
			  ((stringp pair)
			   (twindrill-percent-encode pair))
			  ((consp pair)
			   (format
			    "%s=%s"
			    (twindrill-percent-encode (car pair))
			    (twindrill-percent-encode (cdr pair))))
			  (t
			   nil)))
		       query-parameters
		       "&"))
	   (t
	    nil)))
	 (encoded-query-alist
	  (cond
	   ((stringp query-parameters)
	    ;; Query name and its value must be already encoded.
	    (mapcar (lambda (str)
		      (if (string-match "=" str)
			  (let ((key (substring str 0 (match-beginning 0)))
				(value (substring str (match-end 0))))
			    `(,key . ,value))
			`(,str . nil)))
		    (split-string query-parameters "&")))
	   ((consp query-parameters)
	    (mapcar (lambda (pair)
		      (cond
		       ((stringp pair)
			(cons (twindrill-percent-encode pair) nil))
		       ((consp pair)
			(cons (twindrill-percent-encode (car pair))
			      (twindrill-percent-encode (cdr pair))))
		       (t
			nil)))
		    query-parameters))
	   (t
	    nil)))
	 (uri-without-query
	  (concat scheme "://"
		  host
		  (when (and port (not (= port default-port)))
		    (format ":%d" port))
		  path))
	 (uri
	  (if query-string
	      (concat uri-without-query "?" query-string)
	    uri-without-query))
	 (header-list
	  `(,@(when (and (string= method "POST")
			 (not (assoc "Content-Length" header-list)))
		`(("Content-Length" . ,(format "%d" (length post-body)))))
	    ,@(unless (assoc "Host" header-list)
		`(("Host" . ,host)))
	    ,@(unless (assoc "User-Agent" header-list)
		`(("User-Agent" . ,(twindrill-user-agent))))
	    ,@header-list)))
    (cond
     ((not (member method '("POST" "GET")))
      (error "Unknown HTTP method: %s" method)
      nil)
     ((not (string-match "^/" path))
      (error "Invalid HTTP path: %s" path)
      nil)
     (t
      `((method . ,method)
	(scheme . ,scheme)
	(host . ,host)
	(port . ,port)
	(path . ,path)
	(query-string . ,query-string)
	(encoded-query-alist . ,encoded-query-alist)
	(uri . ,uri)
	(uri-without-query . ,uri-without-query)
	(header-list . ,header-list)
	(post-body . ,post-body))))))

(defun twindrill-make-http-request-from-uri (method header-list uri &optional post-body)
  "Returns an alist specifying a HTTP request.
The result alist has the same form as an alist generated by
`twindrill-make-http-request'.

METHOD specifies HTTP method. It must be \"GET\" or \"POST\".
HEADER-LIST is a list of (field-name . field-value) specifying HTTP header
fields. The fields \"Host\" and \"User-Agent\" are automatically filled
if necessary.
URI specifies the URI including query string.
POST-BODY specifies the post body sent when METHOD equals to \"POST\".
If POST-BODY is nil, no body is posted."
  (let* ((parts-alist
	  (let ((parsed-url (url-generic-parse-url uri)))
	    ;; This is required for the difference of url library
	    ;; distributed with Emacs 22 and 23.
	    (cond
	     ((and (fboundp 'url-p) (url-p parsed-url))
	      ;; Emacs 23 and later.
	      `((scheme . ,(url-type parsed-url))
		(host . ,(url-host parsed-url))
		(port . ,(url-portspec parsed-url))
		(path . ,(url-filename parsed-url))))
	     ((vectorp parsed-url)
	      ;; Emacs 22.
	      `((scheme . ,(aref parsed-url 0))
		(host . ,(aref parsed-url 3))
		(port . ,(aref parsed-url 4))
		(path . ,(aref parsed-url 5))))
	     (t
	      nil))))
	 (path (let ((path (cdr (assq 'path parts-alist))))
		 (if (string-match "\\`\\(.*\\)\\?" path)
		     (match-string 1 path)
		   path)))
	 (query-string (let ((path (cdr (assq 'path parts-alist))))
			 (if (string-match "\\?\\(.*\\)\\'" path)
			     (match-string 1 path)
			   nil))))
    (twindrill-make-http-request method header-list
				  (cdr (assq 'host parts-alist))
				  (cdr (assq 'port parts-alist))
				  path
				  query-string
				  post-body
				  (string= "https"
					   (cdr (assq 'scheme parts-alist))))))

(defun twindrill-make-connection-info (request &optional additional order table)
  "Make an alist specifying the information of connection for REQUEST.
REQUEST must be an alist that has the same keys as that generated by
`twindrill-make-http-request'.

ADDITIONAL is appended to the tail of the result alist.
Following ADDITIONAL, an entry in TABLE is also appended to the result alist,
where `twindrill-lookup-connection-type' determines the entry according to
the priority order ORDER.
If ORDER is nil, `twindrill-connection-type-order' is used in place of ORDER.
If TABLE is nil, `twindrill-connection-type-table' is used in place of TABLE.

The parameter symbols are following:
  use-ssl: whether SSL is enabled or not.
  allow-insecure-server-cert: non-nil if an insecure server certificate is
    allowed on SSL.
  cacert-file-fullpath: the full-path of a file including the certificates
    authorizing a server certificate on SSL. The file must be in PEM format.
  use-proxy: non-nil if using a proxy.
  proxy-server: a proxy server or nil.
  proxy-port: a port for connecting the proxy (integer) or nil.
  proxy-user: a username for connecting the proxy or nil.
  proxy-password: a password for connecting the proxy or nil.
  request: an alist specifying a HTTP request."
  (let* ((order (or order twindrill-connection-type-order))
	 (table (or table twindrill-connection-type-table))
	 (scheme (cdr (assq 'scheme request)))
	 (use-ssl (string= "https" scheme))
	 (entry (twindrill-lookup-connection-type use-ssl order table)))
    `((use-ssl . ,use-ssl)
      (allow-insecure-server-cert
       . ,twindrill-allow-insecure-server-cert)
      (cacert-file-fullpath
       . ,(when use-ssl (twindrill-ensure-ca-cert)))
      (use-proxy . ,twindrill-proxy-use)
      ,@(when twindrill-proxy-use
	  `((proxy-server . ,(twindrill-proxy-info scheme 'server))
	    (proxy-port . ,(twindrill-proxy-info scheme 'port))
	    (proxy-user . ,(twindrill-proxy-info scheme 'user))
	    (proxy-password . ,(twindrill-proxy-info scheme 'password))))
      (request . ,request)
      ,@additional
      ,@entry)))

(defun twindrill-get-response-header (buffer)
  "Extract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer which contains the HTTP response."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp "\r?\n\r?\n" nil t)
	  (prog1
	      (buffer-substring (point-min) (match-end 0))
	    (when twindrill-debug-mode
	      (debug-printf "connection-info=%s\n" connection-info)
	      (debug-print "HTTP response header:\n--BEGIN\n")
	      (debug-print (buffer-substring (point-min) (match-end 0)))
	      (debug-print "--END\n")))
	nil))))

(defun twindrill-make-header-info-alist (header-str)
  "Make HTTP header alist from HEADER-STR.
The alist consists of pairs of field-name and field-value, such as
'((\"Content-Type\" . \"application/xml\; charset=utf-8\")
  (\"Content-Length\" . \"2075\"))."
  (let* ((lines (split-string header-str "\r?\n"))
	 (status-line (car lines))
	 (header-lines (cdr lines)))
    (when (string-match
	   "^\\(HTTP/1\.[01]\\) \\([0-9][0-9][0-9]\\) \\(.*\\)$"
	   status-line)
      (append `((status-line . ,status-line)
		(http-version . ,(match-string 1 status-line))
		(status-code . ,(match-string 2 status-line))
		(reason-phrase . ,(match-string 3 status-line)))
	      (remove nil
		      (mapcar
		       (lambda (line)
			 (when (string-match "^\\([^: ]*\\): *\\(.*\\)$" line)
			   (cons (match-string 1 line) (match-string 2 line))))
		       header-lines))))))

(defun twindrill-get-content-subtype-symbol-from-header-info (header-info)
  "Return a symbol corresponding to the subtype of content-type."
  (let* ((content-type
	  ;; According to RFC2616, field name of a HTTP header is
	  ;; case-insensitive.
	  (car
	   (remove
	    nil
	    (mapcar (lambda (entry)
		      (when (and (stringp (car entry))
				 (let ((case-fold-search t))
				   (string-match "\\`content-type\\'"
						 (car entry))))
			(cdr entry)))
		    header-info))))
	 (subtype (when (and (stringp content-type)
			     (string-match "\\` *[^/]*/\\([^ ;]*\\)"
					   content-type))
		    (downcase (match-string 1 content-type))))
	 (symbol-alist
	  '(("json" . json)
	    ("atom+xml" . atom)
	    ("xml" . xml))))
    (cdr (assoc subtype symbol-alist))))

(defun twindrill-decode-response-body (header-info)
  "Decode the current buffer according to the content-type in HEADER-INFO."
  (let* ((content-type
	  ;; According to RFC2616, field name of a HTTP header is
	  ;; case-insensitive.
	  (car
	   (remove
	    nil
	    (mapcar (lambda (entry)
		      (when (and (stringp (car entry))
				 (let ((case-fold-search t))
				   (string-match "\\`content-type\\'"
						 (car entry))))
			(cdr entry)))
		    header-info))))
	 (parameters (when (stringp content-type)
		       (cdr (split-string content-type ";"))))
	 (regexp "^[[:space:]]*charset=utf-8[[:space:]]*$")
	 (encoded-with-utf-8
	  (let ((case-fold-search t))
	    (remove nil
		    (mapcar (lambda (entry)
			      (string-match regexp entry))
			    parameters)))))
    (when encoded-with-utf-8
      (decode-coding-region (point-min) (point-max) 'utf-8))))

(defun twindrill-send-http-request-internal (request additional-info sentinel &optional order table)
  "Open a connection and return a subprocess object for the connection.
REQUEST must be an alist that has the same keys as that generated by
`twindrill-make-http-request'.
SENTINEL is called as a function when the process changes state.
It gets three arguments: the process, a string describing the change, and
the connection-info, which is generated by `twindrill-make-connection-info'
and also includes an alist ADDITIONAL-INFO.

How to perform the request is selected from TABLE according to the priority
order ORDER. ORDER and TABLE are directly sent to
`twindrill-make-connection-info'.
If ORDER is nil, `twindrill-connection-type-order' is used in place of ORDER.
If TABLE is nil, `twindrill-connection-type-table' is used in place of TABLE.
"
  (let* ((order (or order twindrill-connection-type-order))
	 (table (or table twindrill-connection-type-table))
	 (connection-info
	  (twindrill-make-connection-info request additional-info
					   order table))
	 (func (cdr (assq 'send-http-request connection-info)))
	 (temp-buffer (generate-new-buffer "*twmode-http-buffer*"))
	 ;; Bind `default-directory' to the temporary directory
	 ;; because it is possible that the directory pointed by
	 ;; `default-directory' has been already removed.
	 (default-directory temporary-file-directory))
    (cond
     ((and func (functionp func))
      (funcall func "*twmode-generic*" temp-buffer
	       connection-info
	       (when (and sentinel (functionp sentinel))
		 (lexical-let ((sentinel sentinel)
			       (connection-info connection-info))
		   (lambda (proc status)
		     (apply sentinel proc status connection-info nil))))))
     (t
      (error "No valid connection method is found")
      nil))))

(defun twindrill-send-http-request (request additional-info func &optional clean-up-func)
  "Send a HTTP request and return a subprocess object for the connection.
REQUEST must be an alist that has the same keys as that generated by
`twindrill-make-http-request'.

FUNC is called when a HTTP response has been received without errors.
It is called with the current buffer containing the HTTP response (without
HTTP headers). FUNC is called with four arguments: the process, a symbol
describing the status of the process, a connection-info generated by
`twindrill-make-connection-info', and a header-info generated by
`twindrill-get-response-header' and `twindrill-make-header-info-alist'.
The connection-info also includes an alist ADDITIONAL-INFO.
If FUNC returns non-nil and `twindrill-buffer-related-p' is non-nil, the
returned value is displayed as a message.
And also, if FUNC returns a string and it matches the regular expression
\"^\\\\(Failuare\\\\|Response\\\\): \", the returned value is displayed
as a message.

CLEAN-UP-FUNC is called whenever the sentinel of the subprocess for the
connection is called (as `set-process-sentinel').
It is called with three arguments: the process, a symbol describing the status
of the proess, and a connection-info generated by
`twindrill-make-connection-info'.
They are the same as arguments for FUNC.
When a HTTP response has been received, FUNC is called in advance of
CLEAN-UP-FUNC. CLEAN-UP-FUNC can overwrite the message displayed by FUNC.

If the subprocess has exited, the buffer bound to it is automatically killed
after calling CLEAN-UP-FUNC.

The method to perform the request is determined from
`twindrill-connection-type-table' according to the priority order
`twindrill-connection-type-order'."
  (lexical-let ((func func)
		(clean-up-func clean-up-func))
    (twindrill-send-http-request-internal
     request additional-info
     (lambda (proc status-str connection-info)
       (let ((status (cond
		      ((string= status-str "urllib-finished") 'exit)
		      ((processp proc) (process-status proc))
		      (t nil)))
	     (buffer (process-buffer proc))
	     (exit-status (cond
			   ((string= status-str "urllib-finished") 0)
			   ((processp proc) (process-exit-status proc))
			   (t 1)))
	     (command (process-command proc))
	     (pre-process-func
	      (cdr (assq 'pre-process-buffer connection-info)))
	     (mes nil))
	 (unwind-protect
	     (setq mes
		   (cond
		    ((null status)
		     (format "Failure: process %s does not exist" proc))
		    ((or (memq status '(run stop open listen connect))
			 (not (memq status '(exit signal closed failed))))
		     ;; If the process is running, FUNC is not called.
		     nil)
		    ((and command
			  (not (= 0 exit-status)))
		     ;; If the process abnormally exited,
		     (format "Failure: %s exited abnormally (exit-status=%s)"
			     (car command) exit-status))
		    ((not (buffer-live-p buffer))
		     (format "Failure: the buffer for %s is already killed"
			     proc))
		    (t
		     (when (functionp pre-process-func)
		       ;; Pre-process buffer.
		       (funcall pre-process-func proc buffer connection-info))
		     (let* ((header (twindrill-get-response-header buffer))
			    (header-info
			     (and header
				  (twindrill-make-header-info-alist header))))
		       (with-current-buffer buffer
			 (goto-char (point-min))
			 (when (search-forward-regexp "\r?\n\r?\n" nil t)
			   ;; delete HTTP headers.
			   (delete-region (point-min) (match-end 0)))
			 ;; It may be necessary to decode the contents of
			 ;; the buffer by UTF-8 because
			 ;; `twindrill-http-application-headers' specifies
			 ;; utf-8 as one of acceptable charset.
			 ;; For the present, only UTF-8 is taken into account.
			 (twindrill-decode-response-body header-info)
			 (apply func proc status connection-info
				header-info nil))))))
	   ;; unwind-forms
	   (setq mes
		 (cond
		  ((null mes)
		   nil)
		  ((string-match "^\\(Failure\\|Response\\): " mes)
		   (let* ((request (cdr (assq 'request connection-info)))
			  (method (cdr (assq 'method request)))
			  (uri (cdr (assq 'uri request))))
		     (concat mes " for " method " " uri)))
		  ((twindrill-buffer-related-p)
		   mes)))
	   (when mes
	     ;; CLEAN-UP-FUNC can overwrite a message from the return value
	     ;; of FUNC.
	     (message "%s" mes))
	   (when (functionp clean-up-func)
	     (funcall clean-up-func proc status connection-info))
	   (when (and (memq status '(exit signal closed failed))
		      (buffer-live-p buffer)
		      (not twindrill-debug-mode))
	     (kill-buffer buffer))))))))

;;;;
;;;; Basic HTTP functions with tls and Emacs builtins.
;;;;

(eval-when-compile (require 'tls nil t))
(defun twindrill-start-http-session-native-tls-p ()
  (when (and (not twindrill-proxy-use)
	     (require 'tls nil t))
    (unless twindrill-tls-program
      (let ((programs
	     (remove nil
		     (mapcar (lambda (cmd)
			       (when (string-match "\\`\\([^ ]+\\) " cmd)
				 (when (executable-find (match-string 1 cmd))
				   cmd)))
			     tls-program))))
	(setq twindrill-tls-program
	      (if twindrill-allow-insecure-server-cert
		  (mapcar
		   (lambda (str)
		     (cond
		      ((string-match "^\\([^ ]*/\\)?openssl s_client " str)
		       (concat (match-string 0 str) "-verify 0 "
			       (substring str (match-end 0))))
		      ((string-match "^\\([^ ]*/\\)?gnutls-cli " str)
		       (concat (match-string 0 str) "--insecure "
			       (substring str (match-end 0))))
		      (t
		       str)))
		   programs)
		programs))))
    (not (null twindrill-tls-program))))

;; TODO: proxy
(defun twindrill-send-http-request-native (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (uri (cdr (assq 'uri connection-info)))
	 (method (cdr (assq 'method request)))
	 (scheme (cdr (assq 'scheme request)))
	 (host (cdr (assq 'host request)))
	 (port (cdr (assq 'port request)))
	 (path (cdr (assq 'path request)))
	 (query-string (cdr (assq 'query-string request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (proxy-credentials
	  (when (and proxy-user proxy-password)
	    (concat "Basic "
		    (base64-encode-string
		     (concat proxy-user ":" proxy-password)))))
	 (header-list
	  (let ((original-header-list (cdr (assq 'header-list request))))
	    (if proxy-credentials
		(cons
		 `("Proxy-Authorization" ,proxy-credentials)
		 original-header-list)
	      original-header-list)))
	 (use-ssl (cdr (assq 'use-ssl connection-info)))
	 (allow-insecure-server-cert
	  (cdr (assq 'allow-insecure-server-cert connection-info)))
	 (connect-host (or proxy-server host))
	 (connect-port (or proxy-port port))
	 (request-str
	  (format "%s %s HTTP/1.1\r\n%s\r\n\r\n%s\r\n"
		  method
		  (if use-proxy
		      ;; As described in 5.1.2 of RFC2616, the
		      ;; absolute URI is required here if the connection
		      ;; uses a proxy.
		      uri
		    (concat path
			    (when query-string
			      (concat "?" query-string))))
		  (mapconcat (lambda (pair)
			       (format "%s: %s" (car pair) (cdr pair)))
			     header-list "\r\n")
		  (or post-body "")))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (tls-program twindrill-tls-program)
	 (proc
	  (funcall (if use-ssl
		       'open-tls-stream
		     'open-network-stream)
		   "network-connection-process"
		   nil connect-host connect-port)))
    (when proc
      (set-process-buffer proc buffer)
      (when (functionp sentinel)
	(if (twindrill-process-alive-p proc)
	    (set-process-sentinel proc sentinel)
	  (funcall sentinel proc "finished")))
      (process-send-string proc request-str)
      proc)))

(defun twindrill-pre-process-buffer-native (proc buffer connection-info)
  (let ((use-ssl (cdr (assq 'use-ssl connection-info)))
	(args (process-command proc)))
    (cond
     ((and use-ssl args
	   (car
	    (remove nil
		    (mapcar (lambda (cmd)
			      (string-match "^\\(.*/\\)?gnutls-cli\\b" cmd))
			    args))))
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (when (search-backward-regexp
		 "- Peer has closed the GNUTLS connection\r?\n\\'")
	    (let ((beg (match-beginning 0))
		  (end (match-end 0)))
	      (delete-region beg end))))))
     ((and use-ssl args
	   (car
	    (remove nil
		    (mapcar
		     (lambda (cmd)
		       (string-match "^\\(.*/\\)?openssl s_client\\b" cmd))
		     args))))
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (when (search-backward-regexp "closed\r?\n\\'")
	    (let ((beg (match-beginning 0))
		  (end (match-end 0)))
	      (delete-region beg end))))))
     (t
      nil))))

;;;;
;;;; Basic HTTP functions with curl
;;;;

(defun twindrill-find-curl-program ()
  "Returns an appropriate `curl' program pathname or nil if not found."
  (or (executable-find "curl")
      (let ((windows-p (memq system-type '(windows-nt cygwin)))
	    (curl.exe
	     (expand-file-name
	      "curl.exe"
	      (expand-file-name
	       "win-curl"
	       (file-name-directory (symbol-file 'twit))))))
	(and windows-p
	     (file-exists-p curl.exe) curl.exe))))

(defun twindrill-start-http-session-curl-p ()
  "Return t if curl was installed, otherwise nil."
  (unless twindrill-curl-program
    (setq twindrill-curl-program (twindrill-find-curl-program)))
  (not (null twindrill-curl-program)))

(defun twindrill-start-http-session-curl-https-p ()
  "Return t if curl was installed and the curl support HTTPS, otherwise nil."
  (when (twindrill-start-http-session-curl-p)
    (unless twindrill-curl-program-https-capability
      (with-temp-buffer
	(let ((coding-system-for-read 'iso-safe)
	      (coding-system-for-write 'iso-safe)
	      ;; Bind `default-directory' to the temporary directory
	      ;; because it is possible that the directory pointed by
	      ;; `default-directory' has been already removed.
	      (default-directory temporary-file-directory))
	  (call-process twindrill-curl-program
			nil (current-buffer) nil
			"--version")
	  (goto-char (point-min))
	  (setq twindrill-curl-program-https-capability
		(if (search-forward-regexp "^Protocols: .*https" nil t)
		    'capable
		  'incapable)))))
    (eq twindrill-curl-program-https-capability 'capable)))

(defun twindrill-send-http-request-curl (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
	 (uri (cdr (assq 'uri request)))
	 (header-list (cdr (assq 'header-list request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (use-ssl (cdr (assq 'use-ssl connection-info)))
	 (allow-insecure-server-cert
	  (cdr (assq 'allow-insecure-server-cert connection-info)))
	 (cacert-file-fullpath
	  (cdr (assq 'cacert-file-fullpath connection-info)))
	 (cacert-file-base-directory
	  (when cacert-file-fullpath
	    (file-name-directory cacert-file-fullpath)))
	 (cacert-file-body
	  (when cacert-file-fullpath
	    (file-name-nondirectory cacert-file-fullpath)))
	 (header-list
	  `(,@header-list
	    ;; Make `curl' remove the HTTP header field "Expect" for
	    ;; avoiding '417 Expectation Failed' HTTP response error.
	    ;; The header field is automatically added for a HTTP request
	    ;; exceeding 1024 byte. See
	    ;; http://d.hatena.ne.jp/imait/20091228/1262004813 and
	    ;; http://www.escafrace.co.jp/blog/09/10/16/1008
	    ("Expect" . "")))
	 (curl-args
	  `("--include" "--silent" "--compressed"
	    ,@(apply 'append
		     (mapcar
		      (lambda (pair)
			;; Do not overwrite internal headers `curl' would use.
			;; Thanks to William Xu.
			;; "cURL - How To Use"
			;; http://curl.haxx.se/docs/manpage.html
			(unless (string= (car pair) "Host")
			  `("-H" ,(format "%s: %s" (car pair) (cdr pair)))))
		      header-list))
	    ,@(when use-ssl `("--cacert" ,cacert-file-body))
	    ,@(when (and use-ssl allow-insecure-server-cert)
		`("--insecure"))
	    ,@(when (and use-proxy proxy-server proxy-port)
		(append
		 `("-x" ,(format "%s:%s" proxy-server proxy-port))
		 (when (and proxy-user proxy-password)
		   `("-U" ,(format "%s:%s" proxy-user proxy-password)))))
	    ,@(when (string= "POST" method)
		`("-d" ,(or post-body "")))
	    ,uri))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (default-directory
	   ;; If `use-ssl' is non-nil, the `curl' process
	   ;; is executed at the same directory as the temporary cert file.
	   ;; Without changing directory, `curl' misses the cert file if
	   ;; you use Emacs on Cygwin because the path on Emacs differs
	   ;; from Windows.
	   ;; With changing directory, `curl' on Windows can find the cert
	   ;; file if you use Emacs on Cygwin.
	   (if use-ssl
	       cacert-file-base-directory
	     default-directory)))
    (twindrill-start-process-with-sentinel name buffer
					    twindrill-curl-program
					    curl-args sentinel)))

(defun twindrill-pre-process-buffer-curl (proc buffer connection-info)
  (let ((use-ssl (cdr (assq 'use-ssl connection-info)))
	(use-proxy (cdr (assq 'use-proxy connection-info))))
    (when (and use-ssl use-proxy)
      ;; When using SSL via a proxy with CONNECT method,
      ;; omit a successful HTTP response and headers if they seem to be
      ;; sent from the proxy.
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-min))
	  (let ((first-regexp
		 ;; successful HTTP response
		 "\\`HTTP/1\.[01] 2[0-9][0-9] .*?\r?\n")
		(next-regexp
		 ;; following HTTP response
		 "^\\(\r?\n\\)HTTP/1\.[01] [0-9][0-9][0-9] .*?\r?\n"))
	    (when (and (search-forward-regexp first-regexp nil t)
		       (search-forward-regexp next-regexp nil t))
	      (let ((beg (point-min))
		    (end (match-end 1)))
		(delete-region beg end)))))))))

;;;;
;;;; Basic HTTP functions with wget
;;;;

(defun twindrill-find-wget-program ()
  "Returns an appropriate `wget' program pathname or nil if not found."
  (executable-find "wget"))

(defun twindrill-start-http-session-wget-p ()
  "Return t if `wget' was installed, otherwise nil."
  (unless twindrill-wget-program
    (setq twindrill-wget-program (twindrill-find-wget-program)))
  (not (null twindrill-wget-program)))

(defun twindrill-send-http-request-wget (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
	 (scheme (cdr (assq 'scheme request)))
	 (uri (cdr (assq 'uri request)))
	 (header-list (cdr (assq 'header-list request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (use-ssl (cdr (assq 'use-ssl connection-info)))
	 (allow-insecure-server-cert
	  (cdr (assq 'allow-insecure-server-cert connection-info)))
	 (cacert-file-fullpath
	  (cdr (assq 'cacert-file-fullpath connection-info)))
	 (cacert-file-base-directory
	  (when cacert-file-fullpath
	    (file-name-directory cacert-file-fullpath)))
	 (cacert-file-body
	  (when cacert-file-fullpath
	    (file-name-nondirectory cacert-file-fullpath)))
	 (args
	  `("--save-headers"
	    "--quiet"
	    "--output-document=-"
	    ,@(remove nil
		      (mapcar
		       (lambda (pair)
			 (unless (string= (car pair) "Host")
			   (format "--header=%s: %s" (car pair) (cdr pair))))
		       header-list))
	    ,@(when use-ssl
		`(,(format "--ca-certificate=%s" cacert-file-body)))
	    ,@(when (and use-ssl allow-insecure-server-cert)
		`("--no-check-certificate"))
	    ,@(cond
	       ((not use-proxy)
		'("--no-proxy"))
	       ((and use-proxy proxy-server proxy-port
		     proxy-user proxy-password)
		`(,(format "--proxy-user=%s" proxy-user)
		  ,(format "--proxy-password=%s" proxy-password)))
	       (t
		nil))
	    ,@(when (string= "POST" method)
		`(,(concat "--post-data=" (or post-body ""))))
	    ,uri))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (default-directory
	   ;; If `use-ssl' is non-nil, the `wget' process
	   ;; is executed at the same directory as the temporary cert file.
	   ;; Without changing directory, `wget' misses the cert file if
	   ;; you use Emacs on Cygwin because the path on Emacs differs
	   ;; from Windows.
	   ;; With changing directory, `wget' on Windows can find the cert
	   ;; file if you use Emacs on Cygwin.
	   (if use-ssl
	       cacert-file-base-directory
	     default-directory))
	 (process-environment
	  `(,@(when (and use-proxy proxy-server proxy-port)
		`(,(format "%s_proxy=%s://%s:%s/" scheme
			   scheme proxy-server proxy-port)))
	    ,@process-environment)))
    (twindrill-start-process-with-sentinel name buffer
					    twindrill-wget-program args
					    sentinel)))

(defun twindrill-pre-process-buffer-wget (proc buffer connection-info)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "\\`[^\n]*?\r\r\n" (point-max) t)
	;; When `wget.exe' writes HTTP response in text mode,
	;; CRLF may be converted into CRCRLF.
	(goto-char (point-min))
	(while (search-forward "\r\n" nil t)
	  (replace-match "\n" nil t)))
      (goto-char (point-max))
      (when (search-backward-regexp "\nProcess [^\n]* finished\n\\'"
				    (point-min) t)
	(replace-match "" nil t))
      )))

;;;;
;;;; Basic HTTP functions with url library
;;;;

(defun twindrill-start-http-session-urllib-p ()
  "Return t if url library is available, otherwise nil."
  (require 'url nil t))

(defun twindrill-start-http-session-urllib-https-p ()
  "Return t if url library can be used for HTTPS, otherwise nil."
  (and (not twindrill-proxy-use)
       (require 'url nil t)
       (cond
	((<= 22 emacs-major-version)
	 ;; On Emacs22 and later, `url' requires `tls'.
	 (twindrill-start-http-session-native-tls-p))
	((require 'ssl nil t)
	 ;; On Emacs21, `url' requires `ssl'.
	 t)
	((or (and (fboundp 'open-ssl-stream)
		  ;; Since `url-gw' (required by `url') defines autoload of
		  ;; `open-ssl-stream' from "ssl",
		  ;; (fboundp 'open-ssl-stream) will be non-nil even if
		  ;; "ssl" cannot be loaded and `open-ssl-stream' is
		  ;; unavailable.
		  ;; Here, the availability is confirmed by `documentation'.
		  (documentation 'open-ssl-stream))
	     ;; On Emacs21, `url' requires `ssl' in order to use
	     ;; `open-ssl-stream', which is included in `ssl.el'.
	     ;; Even if `ssl' cannot be loaded, `open-tls-stream' can be
	     ;; used as an alternative of the function.
	     (and (twindrill-start-http-session-native-tls-p)
		  (defalias 'open-ssl-stream 'open-tls-stream)))
	 (provide 'ssl)
	 t)
	(t
	 nil))))

(defun twindrill-send-http-request-urllib (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
	 (scheme (cdr (assq 'scheme request)))
	 (uri (cdr (assq 'uri request)))
	 (header-list (cdr (assq 'header-list request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (proxy-credentials
	  (when (and proxy-user proxy-password)
	    (concat "Basic "
		    (base64-encode-string
		     (concat proxy-user ":" proxy-password)))))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (url-proxy-services
	  (when use-proxy
	    `((,scheme . ,(format "%s:%s" proxy-server proxy-port)))))
	 (url-request-method method)
	 (url-request-extra-headers
	  ;; Remove some headers that should be configured by url library.
	  ;; They may break redirections by url library because
	  ;; `url-request-extra-headers' overwrites the new headers
	  ;; that are adapted to redirected connection.
	  (apply 'append
		 (mapcar (lambda (pair)
			   (if (member (car pair)
				       '("Host" "Content-Length"))
			       nil
			     `(,pair)))
			 (if proxy-credentials
			     (cons
			      `("Proxy-Authorization" ,proxy-credentials)
			      header-list)
			   header-list))))
	 (url-request-data post-body)
	 (url-show-status twindrill-url-show-status)
	 (url-http-attempt-keepalives nil)
	 (tls-program twindrill-tls-program)
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary))
    (lexical-let ((sentinel sentinel)
		  (buffer buffer))
      (let ((result-buffer
	     (url-retrieve
	      uri
	      (lambda (&rest args)
		(let ((proc url-http-process)
		      (url-buffer (current-buffer))
		      (status-str
		       (if (and (< emacs-major-version 22)
				(boundp 'url-http-end-of-headers)
				url-http-end-of-headers)
			   "urllib-finished"
			 "finished")))
		  ;; Callback may be called multiple times.
		  ;; (as filter and sentinel?)
		  (unless (local-variable-if-set-p 'twindrill-retrieved)
		    (set (make-local-variable 'twindrill-retrieved)
			 'not-completed)
		    (with-current-buffer buffer
		      (set-buffer-multibyte nil)
		      (insert-buffer-substring url-buffer))
		    (set-process-buffer proc buffer)
		    (unwind-protect
			(apply sentinel proc status-str nil)
		      (set-process-buffer proc url-buffer)
		      (if (eq twindrill-retrieved 'exited)
			  (url-mark-buffer-as-dead url-buffer)
			(setq twindrill-retrieved 'completed))))
		  (when (memq (process-status proc)
			      '(nil closed exit failed signal))
		    ;; Mark `url-buffer' as dead when the process exited
		    ;; and `sentinel' is completed.
		    ;; If this `lambda' is evaluated via a filter, the
		    ;; process may exit before it is finished to evaluate
		    ;; `(apply sentinel ...)'. In the case, `buffer' should
		    ;; not be killed. It should be killed after the
		    ;; evaluation of `sentinel'.
		    (if (eq twindrill-retrieved 'completed)
			(url-mark-buffer-as-dead url-buffer)
		      (setq twindrill-retrieved 'exited))))))))
	(when (buffer-live-p result-buffer)
	  (with-current-buffer result-buffer
	    (set (make-local-variable 'url-show-status)
		 twindrill-url-show-status)
	    ;; Make `url-http-attempt-keepalives' buffer-local
	    ;; in order to send the current value of the variable
	    ;; to the sentinel invoked for HTTP redirection,
	    (make-local-variable 'url-http-attempt-keepalives))
	  (get-buffer-process result-buffer))))))

(defun twindrill-pre-process-buffer-urllib (proc buffer connection-info)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (cond
       ((search-backward-regexp
	 "- Peer has closed the GNUTLS connection\r?\n\\'"
	 nil t)
	(let ((beg (match-beginning 0))
	      (end (match-end 0)))
	  (delete-region beg end)))
       ((search-backward-regexp "closed\r?\n\\'" nil t)
	(let ((beg (match-beginning 0))
	      (end (match-end 0)))
	  (delete-region beg end)))
       (t nil)))))

;;;;
;;;; HTTP functions for twitter-like serivce
;;;;

(defun twindrill-http-application-headers (&optional method headers)
  "Return an assoc list of HTTP headers for twindrill-mode."
  (unless method
    (setq method "GET"))

  (let ((headers headers))
    (push (cons "User-Agent" (twindrill-user-agent)) headers)
    (when (string= "GET" method)
      (push (cons "Accept"
		  (concat
		   "text/xml"
		   ",application/xml"
		   ",application/xhtml+xml"
		   ",application/html;q=0.9"
		   ",text/plain;q=0.8"
		   ",image/png,*/*;q=0.5"))
	    headers)
      (push (cons "Accept-Charset" "utf-8;q=0.7,*;q=0.7")
	    headers))
    (when (string= "POST" method)
      (push (cons "Content-Type" "text/plain") headers))
    headers
    ))

(defun twindrill-add-application-header-to-http-request (request account-info)
  "Make a new HTTP request based on REQUEST with the authorization header.
The authorization header is generated from ACCOUNT-INFO.
ACCOUNT-INFO must be an alist that includes the following keys;
  \"screen_name\" and \"password\" if `twindrill-auth-method' is 'basic,
  \"screen_name\", \"oauth_token\" and \"oauth_token_secret\" if
  `twindrill-auth-method' is 'oauth or 'xauth."
  (let* ((method (cdr (assq 'method request)))
	 (auth-str
	  (cond
	   ((eq twindrill-auth-method 'basic)
	    (twindrill-make-basic-authentication-string account-info))
	   ((memq twindrill-auth-method '(oauth xauth))
	    (twindrill-make-oauth-authentication-string account-info request))
	   (t
	    nil)))
	 (cookie-str (twindrill-make-cookie-string request account-info))
	 (application-headers
	  `(,@(twindrill-http-application-headers method)
	    ("Authorization" . ,auth-str)
	    ,@(when cookie-str
		`(("Cookie" . ,cookie-str))))))
    (mapcar (lambda (entry)
	      (if (eq (car entry) 'header-list)
		  `(header-list
		    . ,(append (cdr entry) application-headers))
		entry))
	    request)))

(defun twindrill-get-error-message (header-info connection-info &optional buffer)
  "Return an error message generated from the arguments.
HEADER-INFO must be an alist generated by `twindrill-get-response-header'.
CONNECTION-INFO must be an alist generated by
`twindrill-make-connection-info'. It may include some additional information
which is added by `twindrill-send-http-request'.
BUFFER must be nil or a HTTP response body, which includes error messages from
the server when the HTTP status code equals to 400 or 403.
If BUFFER is nil, the current buffer is used instead."
  (let ((buffer (or buffer (current-buffer)))
	(status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twindrill-get-content-subtype-symbol-from-header-info header-info)))
    (cond
     ((and (buffer-live-p buffer)
	   (member status-code '("400" "401" "403" "404")))
      ;; Twitter returns an error message as a HTTP response body if
      ;; HTTP status is "400 Bad Request" or "403 Forbidden".
      ;; See "HTTP Response Codes and Errors | dev.twitter.com"
      ;; http://dev.twitter.com/pages/responses_errors .
      ;;
      ;; However, Twitter seems to return an error message even when
      ;; the HTTP status is "401 Unauthorized" or "404 Not Found".
      (let* ((error-mes
	      (cond
	       ((eq format 'xml)
		(let ((xmltree
		       (with-current-buffer buffer
			 (twindrill-xml-parse-region (point-min)
						      (point-max)))))
		  (car (cddr (assq 'error (or (assq 'errors xmltree)
					      (assq 'hash xmltree)))))))
	       ((eq format 'json)
		(let ((json-object (with-current-buffer buffer
				     (twindrill-json-read))))
		  (cdr (assq 'error json-object))))
	       (t
		;; ATOM is not supported.
		nil))))
	(if error-mes
	    (format "%s (%s)" status-line error-mes)
	  status-line)))
     (t
      status-line))))

(defun twindrill-http-get (account-info-alist host method &optional parameters format additional-info sentinel clean-up-sentinel)
  "Send a HTTP GET request with application headers.
ACCOUNT-INFO-ALIST is an alist used by
`twindrill-add-application-header-to-http-request'.
The alist made by `((account-info . ,ACCOUNT-INFO-ALIST) ,@ADDITIONAL-INFO)'
is used as the argument `additional-info' of `twindrill-send-http-request'.
HOST is hostname of remote side, api.twitter.com (or search.twitter.com).
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6
FORMAT is a response data format (\"xml\", \"atom\", \"json\")"
  (let* ((format (or format "xml"))
	 (sentinel
	  (lexical-let ((sentinel (or sentinel
				      'twindrill-http-get-default-sentinel)))
	    (lambda (proc status connection-info header-info)
	      (twindrill-update-server-info connection-info header-info)
	      (apply sentinel proc status connection-info header-info nil))))
	 (path (concat "/" method "." format))
	 (headers nil)
	 (port nil)
	 (post-body "")
	 (request
	  (twindrill-add-application-header-to-http-request
	   (twindrill-make-http-request "GET" headers host port path
					 parameters post-body
					 twindrill-use-ssl)
	   account-info-alist))
	 (additional-info
	  `((account-info . ,account-info-alist)
	    ,@additional-info)))
    (twindrill-send-http-request request additional-info
				  sentinel clean-up-sentinel)))

(defun twindrill-http-get-default-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twindrill-get-content-subtype-symbol-from-header-info header-info)))
    (case-string
     status-code
     (("200")
      (debug-printf "connection-info=%s" connection-info)
      (let* ((spec (cdr (assq 'timeline-spec connection-info)))
	     (spec-string (cdr (assq 'timeline-spec-string connection-info)))
	     (service-method (cdr (assq 'service-method connection-info)))
	     (statuses
	      (cond
	       ((eq format 'json)
		(let ((json-array (twindrill-json-read)))
		  (cond
		   ((null json-array)
		    nil)
		   ((eq (car spec) 'search)
		    (cond
		     ((memq service-method '(twitter statusnet))
		      (mapcar 'twindrill-json-object-to-a-status-on-search
			      (cdr (assq 'results json-array))))
		     ((eq service-method 'twitter-api-v1.1)
		      (mapcar 'twindrill-json-object-to-a-status
			      (cdr (assq 'statuses json-array))))))
		   ((twindrill-timeline-spec-is-direct-messages-p spec)
		    (mapcar
		     'twindrill-json-object-to-a-status-on-direct-messages
		     json-array))
		   (t
		    (mapcar 'twindrill-json-object-to-a-status
			    json-array)))))
	       ((eq format 'xml)
		(let ((xmltree
		       (twindrill-xml-parse-region (point-min) (point-max))))
		  (when xmltree
		    (twindrill-xmltree-to-status xmltree))))
	       ((eq format 'atom)
		(let ((xmltree
		       (twindrill-xml-parse-region (point-min) (point-max))))
		  (when xmltree
		    (twindrill-atom-xmltree-to-status xmltree))))
	       (t
		nil)))
	     (rendered-tweets nil))
	(let ((updated-timeline-info
	       (twindrill-add-statuses-to-timeline-data statuses spec))
	      (buffer (twindrill-get-buffer-from-spec spec)))
	  ;; FIXME: We should retrieve un-retrieved statuses until
	  ;; statuses is nil. twitter server returns nil as
	  ;; xmltree with HTTP status-code is "200" when we
	  ;; retrieved all un-retrieved statuses.
	  (if twindrill-notify-successful-http-get
	      (if updated-timeline-info
		  (concat
		   (format "Fetching %s. Success. " spec-string)
		   (mapconcat
		    (lambda (info)
		      (let ((spec-string (nth 0 info))
			    (num (nth 1 info)))
			(format "%s: +%d" spec-string num)))
		    updated-timeline-info
		    ", "))
		(format "Fetching %s. Success. (No new tweets)"
			spec-string))
	    nil))))
     (("404")
      ;; The requested resource does not exist.
      (let ((spec (cdr (assq 'timeline-spec connection-info)))
	    (spec-string (cdr (assq 'timeline-spec-string connection-info))))
	;; Remove specs related to the invalid spec from history.
	(mapc
	 (lambda (buffer)
	   (let ((other-spec (twindrill-get-timeline-spec-for-buffer buffer))
		 (other-spec-string
		  (twindrill-get-timeline-spec-string-for-buffer buffer)))
	     (when (twindrill-timeline-spec-depending-on-p other-spec spec)
	       (twindrill-remove-timeline-spec-string-from-history
		other-spec-string))))
	 (twindrill-get-buffer-list)))
      (format "Response: %s"
	      (twindrill-get-error-message header-info connection-info)))
     (t
      (format "Response: %s"
	      (twindrill-get-error-message header-info connection-info))))))

(defun twindrill-retrieve-single-tweet-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twindrill-get-content-subtype-symbol-from-header-info header-info)))
    (case-string
     status-code
     (("200" "403" "404")
      (debug-printf "connection-info=%s" connection-info)
      (let* ((id (cdr (assq 'id connection-info)))
	     (user-screen-name (cdr (assq 'user-screen-name connection-info)))
	     (status
	      (cond
	       ((string= status-code "403")
		;; Forbidden. Maybe a protected tweet?
		(twindrill-make-alist-of-forbidden-tweet id
							  user-screen-name))
	       ((string= status-code "404")
		;; The requested resource does not exist.
		(twindrill-make-alist-of-non-existent-tweet id
							     user-screen-name))
	       ((eq format 'json)
		(let ((json-object (twindrill-json-read)))
		  (twindrill-json-object-to-a-status json-object)))
	       ((eq format 'xml)
		(let ((xmltree
		       (twindrill-xml-parse-region (point-min) (point-max))))
		  (when xmltree
		    (car
		     (twindrill-xmltree-to-status
		      `((statuses nil ,@xmltree)))))))
	       (t
		nil))))
	(when status
	  (twindrill-add-statuses-to-timeline-data `(,status) '(:single))
	  (let ((buffer (cdr (assq 'buffer connection-info)))
		(spec (cdr (assq 'timeline-spec connection-info)))
		(prop
		 (cdr (assq 'property-to-be-redisplayed connection-info))))
	    (cond
	     ((null prop)
	      ;; The process has been invoked via `twindrill-call-api' with
	      ;; the command `retrieve-timeline', not the command
	      ;; `retrieve-single-tweet' for rendering a replied tweet.
	      ;; No special property that specifies regions being re-rendered
	      ;; is given.
	      (let ((new-statuses `(,status))
		    (buffer (twindrill-get-buffer-from-spec spec)))
		(when (and new-statuses buffer)
		  (twindrill-render-timeline buffer new-statuses t))))
	     ((and buffer prop (buffer-live-p buffer))
	      (twindrill-redisplay-status-on-each-buffer buffer prop)
	      (with-current-buffer buffer
		(save-excursion
		  (let ((buffer-read-only nil))
		    (lexical-let ((prop prop))
		      (twindrill-for-each-property-region
		       prop
		       (lambda (beg end value)
			 ;; Remove the property required no longer.
			 (remove-text-properties beg end `(,prop nil))
			 (goto-char beg)
			 (twindrill-render-replied-statuses)))))))))))
	(cond
	 ((string= status-code "403")
	  (format "You are not authorized to see this tweet (ID %s)." id))
	 ((string= status-code "404")
	  (format "The tweet with ID %s does not exist." id))
	 (twindrill-notify-successful-http-get
	  (format "Fetching %s.  Success." id))
	 (t
	  nil))))
     (t
      (format "Response: %s"
	      (twindrill-get-error-message header-info connection-info))))))

(defmacro twindrill-http-get-list-sentinel-base (what)
  `(let ((status-line (cdr (assq 'status-line header-info)))
	 (status-code (cdr (assq 'status-code header-info)))
	 (format
	  (twindrill-get-content-subtype-symbol-from-header-info header-info))
	 (indexes nil)
	 (mes nil))
     (case-string
      status-code
      (("200")
       (cond
	((eq format 'xml)
	 (let ((xmltree (twindrill-xml-parse-region (point-min) (point-max))))
	   (when xmltree
	     (setq indexes
		   (mapcar
		    (lambda (c-node)
		      (caddr (assq ,what c-node)))
		    (remove nil
			    (mapcar
			     (lambda (node)
			       (and (consp node) (eq 'list (car node))
				    node))
			     (cdr-safe
			      (assq 'lists (assq 'lists_list xmltree))))
			    ))
		   ))))
	((eq format 'json)
	 (let* ((json-object (twindrill-json-read))
		(json-list
		 (cond
		  ((arrayp json-object)
		   ;; GET lists/list in the Twitter REST API v1.1 returns
		   ;; an array.
		   json-object)
		  (t
		   ;; GET lists/subscriptions in the Twitter REST API v1.1
		   ;; returns an alist.
		   (cdr (assq 'lists json-object))))))
	   (when json-object
	     (setq indexes
		   (mapcar (lambda (entry)
			     (cdr (assq ,what entry)))
			   json-list)))))
	(t
	 (error "Format \"%s\" is not supported" format)
	 nil)))
      (t
       (setq mes (format "Response: %s"
			 (twindrill-get-error-message header-info
						       connection-info)))))
     (setq twindrill-list-index-retrieved
	   (or indexes
	       mes
	       "")) ;; set "" explicitly if user does not have a list.
     mes))

(defun twindrill-http-get-list-index-sentinel (proc status connection-info header-info)
  (twindrill-http-get-list-sentinel-base 'slug))

(defun twindrill-http-get-list-subscriptions-sentinel (proc status connection-info header-info)
  (let ((result (twindrill-http-get-list-sentinel-base 'full_name)))
    (when (listp twindrill-list-index-retrieved)
      (setq twindrill-list-index-retrieved
	    (mapcar (lambda (str)
		      (and (string-match "\\`@\\(.*\\)\\'" str)
			   (match-string 1 str)))
		    twindrill-list-index-retrieved)))
    result))

(defun twindrill-http-post (account-info-alist host method &optional parameters format additional-info sentinel clean-up-sentinel)
  "Send HTTP POST request to api.twitter.com (or search.twitter.com)
ACCOUNT-INFO-ALIST is an alist used by
`twindrill-add-application-header-to-http-request'.
The alist made by `((account-info . ,ACCOUNT-INFO-ALIST) ,@ADDITIONAL-INFO)'
is used as the argument `additional-info' of `twindrill-send-http-request'.
HOST is hostname of remote side, api.twitter.com (or search.twitter.com).
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6
FORMAT is a response data format (\"xml\", \"atom\", \"json\")"
  (let* ((format (or format "xml"))
	 (sentinel
	  (lexical-let ((sentinel (or sentinel
				      'twindrill-http-post-default-sentinel)))
	    (lambda (proc status connection-info header-info)
	      (twindrill-update-server-info connection-info header-info)
	      (apply sentinel proc status connection-info header-info nil))))
	 (path (concat "/" method "." format))
	 (headers nil)
	 (port nil)
	 (post-body "")
	 (request
	  (twindrill-add-application-header-to-http-request
	   (twindrill-make-http-request "POST" headers host port path
					 parameters post-body
					 twindrill-use-ssl)
	   account-info-alist))
	 (additional-info `((account-info . ,account-info-alist)
			    ,@additional-info)))
    (twindrill-send-http-request request additional-info
				  sentinel clean-up-sentinel)))

(defun twindrill-http-post-default-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info))))
    (case-string
     status-code
     (("200")
      "Success: Post.")
     (t
      (format "Response: %s"
	      (twindrill-get-error-message header-info connection-info))))))

(defun twindrill-http-post-destroy-status-sentinel (proc status connection-info header-info)
  "A sentinel for deleting a status invoked via `twindrill-call-api'."
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twindrill-get-content-subtype-symbol-from-header-info header-info)))
    (case-string
     status-code
     (("200")
      (let* ((params
	      (cond
	       ((eq format 'xml)
		(let ((xml
		       (twindrill-xml-parse-region (point-min) (point-max))))
		  `((id . ,(elt (assq 'id (assq 'status xml)) 2))
		    (text . ,(elt (assq 'text (assq 'status xml)) 2)))))
	       ((eq format 'json)
		(let ((json-object (twindrill-json-read)))
		  `((id . ,(cdr (assq 'id_str json-object)))
		    (text . ,(cdr (assq 'text json-object))))))
	       (t
		(error "Format \"%s\" is not supported" format)
		nil)))
	     (id (cdr (assq 'id params)))
	     (text (cdr (assq 'text params))))
	(cond
	 (id
	  (twindrill-delete-status-from-data-table id)
	  (format "Deleting \"%s\". Success." text))
	 (t
	  "Failure: the response for deletion could not be parsed."))))
     (t
      (format "Response: %s"
	      (twindrill-get-error-message header-info connection-info))))))

;;;;
;;;; OAuth
;;;;

(defun twindrill-oauth-url-encode (str &optional coding-system)
  "Encode string according to Percent-Encoding defined in RFC 3986."
  (let ((coding-system (or (when (and coding-system
				      (coding-system-p coding-system))
			     coding-system)
			   'utf-8)))
    (mapconcat
     (lambda (c)
       (cond
	((or (and (<= ?A c) (<= c ?Z))
	     (and (<= ?a c) (<= c ?z))
	     (and (<= ?0 c) (<= c ?9))
	     (eq ?. c)
	     (eq ?- c)
	     (eq ?_ c)
	     (eq ?~ c))
	 (char-to-string c))
	(t (format "%%%02X" c))))
     (encode-coding-string str coding-system)
     "")))

(defun twindrill-oauth-unhex (c)
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (- c ?0))
   ((and (<= ?A c) (<= c ?F))
    (+ 10 (- c ?A)))
   ((and (<= ?a c) (<= c ?f))
    (+ 10 (- c ?a)))
   ))

(defun twindrill-oauth-url-decode (str &optional coding-system)
  (let* ((coding-system (or (when (and coding-system
				       (coding-system-p coding-system))
			      coding-system)
			    'utf-8))
	 (substr-list (split-string str "%"))
	 (head (car substr-list))
	 (tail (cdr substr-list)))
    (decode-coding-string
     (concat
      head
      (mapconcat
       (lambda (substr)
	 (if (string-match "\\`\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\(.*\\)\\'"
			   substr)
	     (let* ((c1 (string-to-char (match-string 1 substr)))
		    (c0 (string-to-char (match-string 2 substr)))
		    (tail (match-string 3 substr))
		    (ch (+ (* 16 (twindrill-oauth-unhex c1))
			   (twindrill-oauth-unhex c0))))
	       (concat (char-to-string ch) tail))
	   substr))
       tail
       ""))
     coding-system)))

(defun twindrill-oauth-make-signature-base-string (method base-url parameters)
  ;; "OAuth Core 1.0a"
  ;; http://oauth.net/core/1.0a/#anchor13
  (let* ((sorted-parameters (copy-sequence parameters))
	 (sorted-parameters
	  (sort sorted-parameters
		(lambda (entry1 entry2)
		  (string< (car entry1) (car entry2))))))
    (concat
     method
     "&"
     (twindrill-oauth-url-encode base-url)
     "&"
     (mapconcat
      (lambda (entry)
	(let ((key (car entry))
	      (value (cdr entry)))
	  (concat (twindrill-oauth-url-encode key)
		  "%3D"
		  (twindrill-oauth-url-encode value))))
      sorted-parameters
      "%26"))))

(defun twindrill-oauth-make-random-string (len)
  (let* ((table
	  (concat
	   "0123456789"
	   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	   "abcdefghijklmnopqrstuvwxyz"))
	 (n (length table))
	 (l 0)
	 (result (make-string len ?0)))
    (while (< l len)
      (aset result l (aref table (random n)))
      (setq l (1+ l)))
    result))

(defun twindrill-sha1 (&rest args)
  "Return the SHA1 (Secure Hash Algorithm) of an object.

This is equivalent to the function `sha1' except that
`coding-system-for-read' and `coding-system-for-write' are bound to the
symbol `binary'.

The function `sha1' uses an external program for large object. However,
the coding system for transferring data from/to the program is not fixed,
at least in the implementation distributed with GNU Emacs 21.4.1, 22.2.1
and 23.2.1.
Therefore, the result from the function `sha1' may depend on the current
coding system.

This function avoid the dependency by binding `coding-system-for-read' and
`coding-system-for-write' to the symbol `binary'."
  (require 'sha1)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	;; Bind `default-directory' to the temporary directory
	;; because it is possible that the directory pointed by
	;; `default-directory' has been already removed.
	(default-directory temporary-file-directory))
    (apply 'sha1 args)))

;;;
;;; The below function is derived from `hmac-sha1' retrieved
;;; from http://www.emacswiki.org/emacs/HmacShaOne.
;;;
(defun twindrill-hmac-sha1 (key message)
  "Return an HMAC-SHA1 authentication code for KEY and MESSAGE.

KEY and MESSAGE must be unibyte strings.  The result is a unibyte
string.  Use the function `encode-hex-string' or the function
`base64-encode-string' to produce human-readable output.

See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
on the HMAC-SHA1 algorithm.

The Emacs multibyte representation actually uses a series of
8-bit values under the hood, so we could have allowed multibyte
strings as arguments.  However, internal 8-bit values don't
correspond to any external representation \(at least for major
version 22).  This makes multibyte strings useless for generating
hashes.

Instead, callers must explicitly pick and use an encoding for
their multibyte data.  Most callers will want to use UTF-8
encoding, which we can generate as follows:

  (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
        (unibyte-value (encode-coding-string value 'utf-8 t)))
    (twindrill-hmac-sha1 unibyte-key unibyte-value))

For keys and values that are already unibyte, the
`encode-coding-string' calls just return the same string."
;;; Return an HMAC-SHA1 authentication code for KEY and MESSAGE.
;;;
;;; KEY and MESSAGE must be unibyte strings.  The result is a unibyte
;;; string.  Use the function `encode-hex-string' or the function
;;; `base64-encode-string' to produce human-readable output.
;;;
;;; See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
;;; on the HMAC-SHA1 algorithm.
;;;
;;; The Emacs multibyte representation actually uses a series of
;;; 8-bit values under the hood, so we could have allowed multibyte
;;; strings as arguments.  However, internal 8-bit values don't
;;; correspond to any external representation \(at least for major
;;; version 22).  This makes multibyte strings useless for generating
;;; hashes.
;;;
;;; Instead, callers must explicitly pick and use an encoding for
;;; their multibyte data.  Most callers will want to use UTF-8
;;; encoding, which we can generate as follows:
;;;
;;; (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
;;;       (unibyte-value (encode-coding-string value 'utf-8 t)))
;;; (hmac-sha1 unibyte-key unibyte-value))
;;;
;;; For keys and values that are already unibyte, the
;;; `encode-coding-string' calls just return the same string.
;;;
;;; Author: Derek Upham - sand (at) blarg.net
;;;
;;; Copyright: This code is in the public domain.
  (require 'sha1)
  (when (multibyte-string-p key)
    (error "key must be unibyte"))
  (when (multibyte-string-p message)
    (error "message must be unibyte"))

  ;; The key block is always exactly the block size of the hash
  ;; algorithm.  If the key is too small, we pad it with zeroes (or
  ;; instead, we initialize the key block with zeroes and copy the
  ;; key onto the nulls).  If the key is too large, we run it
  ;; through the hash algorithm and use the hashed value (strange
  ;; but true).

  (let ((+hmac-sha1-block-size-bytes+ 64)) ; SHA-1 uses 512-bit blocks
    (when (< +hmac-sha1-block-size-bytes+ (length key))
      (setq key (twindrill-sha1 key nil nil t)))

    (let ((key-block (make-vector +hmac-sha1-block-size-bytes+ 0)))
      (dotimes (i (length key))
	(aset key-block i (aref key i)))

      (let ((opad (make-vector +hmac-sha1-block-size-bytes+ #x5c))
	    (ipad (make-vector +hmac-sha1-block-size-bytes+ #x36)))

	(dotimes (i +hmac-sha1-block-size-bytes+)
	  (aset ipad i (logxor (aref ipad i) (aref key-block i)))
	  (aset opad i (logxor (aref opad i) (aref key-block i))))

	(when (fboundp 'unibyte-string)
	  ;; `concat' of Emacs23 (and later?) generates a multi-byte
	  ;; string from a vector of characters with eight bit.
	  ;; Since `opad' and `ipad' must be unibyte, we have to
	  ;; convert them by using `unibyte-string'.
	  ;; We cannot use `string-as-unibyte' here because it encodes
	  ;; bytes with the manner of UTF-8.
	  (setq opad (apply 'unibyte-string (mapcar 'identity opad)))
	  (setq ipad (apply 'unibyte-string (mapcar 'identity ipad))))

	(twindrill-sha1 (concat opad
				 (twindrill-sha1 (concat ipad message)
						  nil nil t))
			 nil nil t)))))

(defun twindrill-oauth-auth-str (method base-url query-parameters oauth-parameters key)
  "Generate the value for HTTP Authorization header on OAuth.
QUERY-PARAMETERS is an alist for query parameters, where name and value
must be encoded into the same as they will be sent."
  (let* ((parameters (append query-parameters oauth-parameters))
	 (base-string
	  (twindrill-oauth-make-signature-base-string method base-url parameters))
	 (key (if (multibyte-string-p key)
		  (string-make-unibyte key)
		key))
	 (base-string (if (multibyte-string-p base-string)
			  (string-make-unibyte base-string)
			base-string))
	 (signature
	  (base64-encode-string (twindrill-hmac-sha1 key base-string))))
    (concat
     "OAuth "
     (mapconcat
      (lambda (entry)
	(concat (car entry) "=\"" (cdr entry) "\""))
      oauth-parameters
      ",")
     ",oauth_signature=\"" (twindrill-oauth-url-encode signature) "\"")))

(defun twindrill-oauth-auth-str-request-token (url query-parameters consumer-key consumer-secret &optional oauth-parameters)
  (let ((key (concat consumer-secret "&"))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_nonce" . ,(twindrill-oauth-make-random-string 43))
	       ("oauth_callback" . "oob")
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_consumer_key" . ,consumer-key)
	       ("oauth_version" . "1.0")))))
    (twindrill-oauth-auth-str "POST" url query-parameters oauth-params key)))

(defun twindrill-oauth-auth-str-exchange-token (url query-parameters consumer-key consumer-secret request-token request-token-secret verifier &optional oauth-parameters)
  (let ((key (concat consumer-secret "&" request-token-secret))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_consumer_key" . ,consumer-key)
	       ("oauth_nonce" . ,(twindrill-oauth-make-random-string 43))
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_version" . "1.0")
	       ("oauth_token" . ,request-token)
	       ("oauth_verifier" . ,verifier)))))
    (twindrill-oauth-auth-str "POST" url query-parameters oauth-params key)))

(defun twindrill-oauth-auth-str-access (method url query-parameters consumer-key consumer-secret access-token access-token-secret &optional oauth-parameters)
  "Generate a string for Authorization in HTTP header on OAuth.
METHOD means HTTP method such as \"GET\", \"POST\", etc. URL means a simple
URL without port number and query parameters.
QUERY-PARAMETERS means an alist of query parameters such as
'((\"status\" . \"test%20tweet\")
  (\"in_reply_to_status_id\" . \"12345678\")),
where name and value must be encoded into the same as they will be sent.
CONSUMER-KEY and CONSUMER-SECRET specifies the consumer.
ACCESS-TOKEN and ACCESS-TOKEN-SECRET must be authorized before calling this
function."
  (let ((key (concat consumer-secret "&" access-token-secret))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_consumer_key" . ,consumer-key)
	       ("oauth_nonce" . ,(twindrill-oauth-make-random-string 43))
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_version" . "1.0")
	       ("oauth_token" . ,access-token)))))
    (twindrill-oauth-auth-str method url query-parameters oauth-params key)))

;; "Using xAuth | dev.twitter.com"
;; http://dev.twitter.com/pages/xauth
(defun twindrill-xauth-auth-str-access-token (url query-parameters consumer-key consumer-secret username password &optional oauth-parameters)
  (let ((key (concat consumer-secret "&"))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_nonce" . ,(twindrill-oauth-make-random-string 43))
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_consumer_key" . ,consumer-key)
	       ("oauth_version" . "1.0"))))
	(query-params
	 (append query-parameters
		 `(("x_auth_mode" . "client_auth")
		   ("x_auth_password"
		    . ,(twindrill-oauth-url-encode password))
		   ("x_auth_username"
		    . ,(twindrill-oauth-url-encode username))))))
    (twindrill-oauth-auth-str "POST" url query-params oauth-params key)))

;; "OAuth Core 1.0a"
;; http://oauth.net/core/1.0a/#response_parameters
(defun twindrill-oauth-make-response-alist (str)
  (mapcar
   (lambda (entry)
     (let* ((pair (split-string entry "="))
	    (name-entry (car pair))
	    (value-entry (cadr pair))
	    (name (and name-entry (twindrill-oauth-url-decode name-entry)))
	    (value (and value-entry
			(twindrill-oauth-url-decode value-entry))))
       `(,name . ,value)))
   (split-string str "&")))

(defun twindrill-oauth-get-response-alist (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (search-forward-regexp
	   "\\`\\(\\(HTTP/1\.[01]\\) \\([0-9][0-9][0-9]\\) \\(.*?\\)\\)\r?\n"
	   nil t)
      (let ((status-line (match-string 1))
	    (http-version (match-string 2))
	    (status-code (match-string 3))
	    (reason-phrase (match-string 4)))
	(cond
	 ((not (string-match "2[0-9][0-9]" status-code))
	  (message "Response: %s" status-line)
	  nil)
	 ((search-forward-regexp "\r?\n\r?\n" nil t)
	  (let ((beg (match-end 0))
		(end (point-max)))
	    (twindrill-oauth-make-response-alist (buffer-substring beg end))))
	 (t
	  (message "Response: %s" status-line)
	  nil))))))

(defun twindrill-oauth-get-token-alist-url (url auth-str post-body)
  (let* ((url-request-method "POST")
	 (url-request-extra-headers
	  `(("Authorization" . ,auth-str)
	    ("Accept-Charset" . "us-ascii")
	    ("Content-Type" . "application/x-www-form-urlencoded")
	    ("Content-Length" . ,(format "%d" (length post-body)))))
	 (url-request-data post-body)
	 (coding-system-for-read 'utf-8-unix))
    (lexical-let ((result 'queried))
      (let ((buffer
	     (url-retrieve
	      url
	      (lambda (&rest args)
		(let* ((status (if (< 21 emacs-major-version)
				   (car args)
				 nil))
		       (callback-args (if (< 21 emacs-major-version)
					  (cdr args)
					args))
		       (response-buffer (current-buffer)))
		  (setq result
			(twindrill-oauth-get-response-alist response-buffer))
		  )))))
	(while (eq result 'queried)
	  (sleep-for 0.1))
	(unless twindrill-debug-mode
	  (kill-buffer buffer))
	result))))

(defun twindrill-oauth-get-token-alist (url auth-str &optional post-body)
  (let ((request
	 (twindrill-make-http-request-from-uri
	  "POST"
	  `(("Authorization" . ,auth-str)
	    ("Accept-Charset" . "us-ascii")
	    ("Content-Type" . "application/x-www-form-urlencoded"))
	  url post-body)))
    (lexical-let ((result 'queried))
      (let ((proc
	     (twindrill-send-http-request
	      request nil
	      (lambda (proc status connection-info header-info)
		(let ((status-line (cdr (assq 'status-line header-info)))
		      (status-code (cdr (assq 'status-code header-info))))
		  (case-string
		   status-code
		   (("200")
		    (when twindrill-debug-mode
		      (let ((buffer (current-buffer)))
			(with-current-buffer (twindrill-debug-buffer)
			  (insert-buffer-substring buffer))))
		    (setq result
			  (twindrill-oauth-make-response-alist
			   (buffer-string)))
		    nil)
		   (t
		    (setq result nil)
		    (format "Response: %s" status-line)))))
	      (lambda (proc status connection-info)
		(when (and (not (twindrill-process-alive-p proc))
			   (eq result 'queried))
		  (setq result nil))))))
	(twindrill-wait-while nil 0.1
			       (and (eq result 'queried)
				    (twindrill-process-alive-p proc)))
	(when (and (eq result 'queried)
		   (not (twindrill-process-alive-p proc)))
	  ;; If the process has been dead, wait a moment because
	  ;; Emacs may be in the middle of evaluating the sentinel.
	  (twindrill-wait-while 10 0.1
				 (eq result 'queried)
				 nil
				 (setq result nil)))
	result))))

(defun twindrill-oauth-get-request-token (url consumer-key consumer-secret)
  (let ((auth-str
	 (twindrill-oauth-auth-str-request-token
	  url nil consumer-key consumer-secret)))
    (twindrill-oauth-get-token-alist url auth-str)))

(defun twindrill-oauth-exchange-request-token (url consumer-key consumer-secret request-token request-token-secret verifier)
  (let ((auth-str
	 (twindrill-oauth-auth-str-exchange-token
	  url nil
	  consumer-key consumer-secret
	  request-token request-token-secret verifier)))
    (twindrill-oauth-get-token-alist url auth-str)))

(defun twindrill-oauth-get-access-token (request-token-url authorize-url-func access-token-url consumer-key consumer-secret consumer-name)
  "Return an alist of authorized access token.
The function retrieves a request token from the site specified by
REQUEST-TOKEN-URL. Then, The function asks a WWW browser to authorize the
token by calling `browse-url'. The URL for authorization is calculated by
calling AUTHORIZE-URL-FUNC with the request token as an argument.
AUTHORIZE-URL-FUNC is called as `(funcal AUTHORIZE-URL-FUNC request-token)',
where the request-token is a string.
After calling `browse-url', the function waits for user to input the PIN code
that is displayed in the browser. The request token is authorized by the
PIN code, and then it is exchanged for the access token on the site
specified by ACCESS-TOKEN-URL.
CONSUMER-KEY and CONSUMER-SECRET specify the consumer.
CONSUMER-NAME is displayed at the guide of authorization.

The access token is returned as a list of a cons pair of name and value
like following:
 ((\"oauth_token\"
  . \"819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw\")
  (\"oauth_token_secret\"
   . \"J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA\")
  (\"user_id\" . \"819797\")
  (\"screen_name\" . \"episod\"))
."
  (let* ((request-token-alist
	  (twindrill-oauth-get-request-token
	   request-token-url consumer-key consumer-secret))
	 (request-token (cdr (assoc "oauth_token" request-token-alist)))
	 (request-token-secret
	  (cdr (assoc "oauth_token_secret" request-token-alist)))
	 (authorize-url (funcall authorize-url-func request-token))
	 (str
	  (concat
	   (propertize "Authorization via OAuth\n" 'face 'bold)
	   "\n"
	   "1.Allow access by " consumer-name " on the below site.\n"
	   "\n  "
	   (propertize authorize-url 'url authorize-url 'face 'bold)
	   "\n"
	   "\n"
	   (when twindrill-oauth-invoke-browser
	     (concat
	      "  Emacs invokes your browser by the function `browse-url'.\n"
	      "  If the site is not opened automatically, you have to open\n"
	      "  the site manually.\n"
	      "\n"))
	   "2.After allowing access, the site will display the PIN code."
	   "\n"
	   "  Input the PIN code "
	   (propertize "at the below minibuffer." 'face 'bold))))
    (cond
     (request-token-alist
      (with-temp-buffer
	(switch-to-buffer (current-buffer))
	(let* ((str-height (length (split-string str "\n")))
	       (height (max 0 (- (/ (- (window-text-height) 1) 2)
				 (/ str-height 2)))))
	  (insert (make-string height ?\n) str)
	  (if twindrill-oauth-invoke-browser
	      (browse-url authorize-url)
	    (when (y-or-n-p "Open authorization URL with browser? (using `browse-url')")
	      (browse-url authorize-url)))
	  (let* ((pin
		  (block pin-input-block
		    (while t
		      (let ((pin-input (read-string "Input PIN code: ")))
			(when (string-match "^\\s-*\\([0-9]+\\)\\s-*$" pin-input)
			  (return-from pin-input-block
			    (match-string 1 pin-input)))))))
		 (verifier pin))
	    (twindrill-oauth-exchange-request-token
	     access-token-url
	     consumer-key consumer-secret
	     request-token request-token-secret verifier)))))
     (t
      (error "Failed to retrieve a request token")
      nil))))

(defun twindrill-xauth-get-access-token (access-token-url consumer-key consumer-secret username password)
  (let ((auth-str
	 (twindrill-xauth-auth-str-access-token
	  access-token-url nil consumer-key consumer-secret
	  username password))
	(post-body
	 (mapconcat (lambda (pair)
		      (format "%s=%s" (car pair)
			      (twindrill-oauth-url-encode (cdr pair))))
		    `(("x_auth_mode" . "client_auth")
		      ("x_auth_password" . ,password)
		      ("x_auth_username" . ,username))
		    "&")))
    (twindrill-oauth-get-token-alist access-token-url auth-str post-body)))

;;;;
;;;; Private storage
;;;;

(defun twindrill-private-info-loaded-p ()
  twindrill-private-info-file-loaded)

(defun twindrill-load-private-info ()
  (let* ((file twindrill-private-info-file)
	 (decrypted-str (twindrill-read-from-encrypted-file file))
	 (loaded-alist
	  (when decrypted-str
	    (condition-case nil
		(read decrypted-str)
	      (error
	       nil)))))
    (when loaded-alist
      (remove
       nil
       (mapcar
	(lambda (pair)
	  (when (consp pair)
	    (let ((sym (car pair))
		  (value (cdr pair)))
	      (cond
	       ((memq sym twindrill-variables-stored-with-encryption)
		(set sym value)
		sym)
	       (t
		nil)))))
	loaded-alist)))))

(defun twindrill-load-private-info-with-guide ()
  (let ((str (concat
	      "Loading authorized access token for OAuth from\n"
	      (format "%s.\n" twindrill-private-info-file)
	      "\n"
	      (propertize "Please input the master password.\n" 'face 'bold)
	      "\n"
	      "To cancel it, you may need to press C-g multiple times.\n"
	      )))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((str-height (length (split-string str "\n")))
	     (height (max 0 (- (/ (- (window-text-height) 1) 2)
			       (/ str-height 2)))))
	(insert (make-string height ?\n) str)
	(set-buffer-modified-p nil)
	(twindrill-load-private-info)))))

(defun twindrill-save-private-info ()
  (let* ((obj (mapcar (lambda (sym)
			`(,sym . ,(symbol-value sym)))
		      twindrill-variables-stored-with-encryption))
	 (str (with-output-to-string (pp obj)))
	 (file twindrill-private-info-file))
    (when (twindrill-write-and-encrypt file str)
      (set-file-modes file #o600)
      (setq twindrill-private-info-file-loaded t))))

(defun twindrill-save-private-info-with-guide ()
  (let ((str (concat
	      "Saving authorized access token for OAuth to "
	      (format "%s.\n" twindrill-private-info-file)
	      "\n"
	      (propertize "Please input a master password twice."
			  'face 'bold))))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((str-height (length (split-string str "\n")))
	     (height (max 0 (- (/ (- (window-text-height) 1) 2)
			       (/ str-height 2)))))
	(insert (make-string height ?\n) str)
	(set-buffer-modified-p nil)
	(twindrill-save-private-info)))))

(defun twindrill-capable-of-encryption-p ()
  (and (or (require 'epa nil t) (require 'alpaca nil t))
       (executable-find "gpg")))

(eval-when-compile
  (require 'epa nil t)
  (require 'alpaca nil t))
(defun twindrill-read-from-encrypted-file (file)
  "Decrypt contents from FILE and return them.
Read encrypted contents from FILE and return the decrypted contents.
This function requires `epa' or `alpaca' library."
  (cond
   ((not (file-readable-p file))
    (error "Failed to read %s" file)
    nil)
   ((require 'epa nil t)
    (let ((context (epg-make-context epa-protocol))
	  ;; Bind `default-directory' to the temporary directory
	  ;; because it is possible that the directory pointed by
	  ;; `default-directory' has been already removed.
	  (default-directory temporary-file-directory))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context
       (cons #'epa-progress-callback-function
	     (format "Decrypting %s..." (file-name-nondirectory file))))
      (message "Decrypting %s..." (file-name-nondirectory file))
      (condition-case err
	  (let ((full-path (expand-file-name file)))
	    ;; `epg-decrypt-file' included in EasyPG 1.0.0, which is
	    ;; distributed with Emacs 23.2, requires the expanded full path
	    ;; as the argument CIPHER. This is because CIPHER is directly
	    ;; used as an argument of the command `gpg'.
	    (epg-decrypt-file context full-path nil))
	(error
	 (message "%s" (cdr err))
	 nil))))
   ((require 'alpaca nil t)
    (with-temp-buffer
      (let ((buffer-file-name (expand-file-name file))
	    (alpaca-regex-suffix ".*")
	    (coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    (temp-buffer (current-buffer))
	    ;; Bind `default-directory' to the temporary directory
	    ;; because it is possible that the directory pointed by
	    ;; `default-directory' has been already removed.
	    (default-directory temporary-file-directory))
	(insert-file-contents-literally file)
	(set-buffer-modified-p nil)
	(condition-case nil
	    (progn
	      (alpaca-after-find-file)
	      (if (eq temp-buffer (current-buffer))
		  (buffer-string)
		;; `alpaca-after-find-file' kills the current buffer
		;; if the decryption is failed.
		nil))
	  (error
	   (when (eq temp-buffer (current-buffer))
	     (delete-region (point-min) (point-max)))
	   nil)))))
   (t
    nil)))

(defun twindrill-write-and-encrypt (file str)
  (cond
   ((require 'epg nil t)
    (let ((context (epg-make-context epa-protocol))
	  ;; Bind `default-directory' to the temporary directory
	  ;; because it is possible that the directory pointed by
	  ;; `default-directory' has been already removed.
	  (default-directory temporary-file-directory))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context (cons #'epa-progress-callback-function "Encrypting..."))
      (message "Encrypting...")
      (condition-case err
	  (unwind-protect
	      ;; In order to prevent `epa-file' to encrypt the file double,
	      ;; `epa-file-name-regexp' is temorarily changed into the null
	      ;; regexp that never matches any string.
	      (let ((epa-file-name-regexp "\\`\\'")
		    (coding-system-for-read 'binary)
		    (coding-system-for-write 'binary))
		(when (fboundp 'epa-file-name-regexp-update)
		  (epa-file-name-regexp-update))
		(with-temp-file file
		  (set-buffer-multibyte nil)
		  (delete-region (point-min) (point-max))
		  (insert (epg-encrypt-string context str nil))
		  (message "Encrypting...wrote %s" file)
		  t))
	    (when (fboundp 'epa-file-name-regexp-update)
	      (epa-file-name-regexp-update)))
	(error
	 (message "%s" (cdr err))
	 nil))))
   ((require 'alpaca nil t)
    ;; Create the file.
    ;; This is required because `alpaca-save-buffer' checks its timestamp.
    (with-temp-file file)
    (with-temp-buffer
      (let ((buffer-file-name file)
	    (coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    ;; Bind `default-directory' to the temporary directory
	    ;; because it is possible that the directory pointed by
	    ;; `default-directory' has been already removed.
	    (default-directory temporary-file-directory))
	(insert str)
	(condition-case nil
	    (if (alpaca-save-buffer)
		t
	      (delete-file file)
	      nil)
	  (error
	   (when (file-exists-p file)
	     (delete-file file))
	   nil)))))
   (t
    nil)))

(defun twindrill-ensure-private-info ()
  "Ensure that private information is loaded if necessary.
Return non-nil if `twindrill-use-master-password' is nil or private
information has been already loaded. Also, return non-nil
if `twindrill-use-master-password' is non-nil and this function succeeded
in loading private information.
Return nil if private information cannot be loaded."
  (if (or (not twindrill-use-master-password)
	  (twindrill-private-info-loaded-p))
      ;; The private information is unnecessary or already loaded.
      t
    (cond
     ((not (twindrill-capable-of-encryption-p))
      (message "You need GnuPG and (EasyPG or alpaca.el) for master password!")
      nil)
     ((and (memq twindrill-auth-method '(oauth xauth))
	   (file-exists-p twindrill-private-info-file))
      (cond
       ((twindrill-load-private-info-with-guide)
	(setq twindrill-private-info-file-loaded t)
	(message "The authorized token is loaded.")
	t)
       (t
	(message "Failed to load an authorized token from \"%s\"."
		 twindrill-private-info-file)
	nil)))
     (t
      ;; The file for private infomation does not exist now.
      t))))

;;;;
;;;; Asynchronous retrieval
;;;;

(defvar twindrill-url-data-hash (make-hash-table :test 'equal))
(defvar twindrill-url-request-list nil)
(defvar twindrill-url-request-sentinel-hash (make-hash-table :test 'equal))
(defvar twindrill-internal-url-queue nil)
(defvar twindrill-url-request-resolving-p nil)
(defvar twindrill-url-request-retry-limit 3)
(defvar twindrill-url-request-sentinel-delay 1.0
  "*Delay from completing retrieval to invoking associated sentinels.
Sentinels registered by `twindrill-url-retrieve-async' will be invoked
after retrieval is completed and Emacs remains idle a certain time, which
this variable specifies. The unit is second.")

(defun twindrill-remove-redundant-queries (queue)
  (remove nil
	  (mapcar
	   (lambda (url)
	     (let ((current (gethash url twindrill-url-data-hash)))
	       (when (or (null current)
			 (and (integerp current)
			      (< current twindrill-url-request-retry-limit)))
		 url)))
	   (twindrill-remove-duplicates queue))))

(defun twindrill-resolve-url-request ()
  "Resolve requests of asynchronous URL retrieval."
  (when (null twindrill-url-request-resolving-p)
    (setq twindrill-url-request-resolving-p t)
    ;; It is assumed that the following part is not processed
    ;; in parallel.
    (setq twindrill-internal-url-queue
	  (append twindrill-internal-url-queue twindrill-url-request-list))
    (setq twindrill-url-request-list nil)
    (setq twindrill-internal-url-queue
	  (twindrill-remove-redundant-queries twindrill-internal-url-queue))
    (if (null twindrill-internal-url-queue)
	(setq twindrill-url-request-resolving-p nil)
      (let* ((url (car twindrill-internal-url-queue))
	     (request (twindrill-make-http-request-from-uri "GET" nil url))
	     (additional-info `((uri . ,url))))
	(twindrill-send-http-request
	 request additional-info
	 'twindrill-url-retrieve-async-sentinel
	 'twindrill-url-retrieve-async-clean-up-sentinel)))))

(defun twindrill-url-retrieve-async-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(uri (cdr (assq 'uri (assq 'request connection-info)))))
    (when (string= status-code "200")
      (let ((body (string-as-unibyte (buffer-string))))
	(puthash uri body twindrill-url-data-hash)
	(setq twindrill-internal-url-queue
	      (remove uri twindrill-internal-url-queue))
	(let ((sentinels (gethash uri twindrill-url-request-sentinel-hash)))
	  (when sentinels
	    (remhash uri twindrill-url-request-sentinel-hash))
	  (twindrill-run-on-idle twindrill-url-request-sentinel-delay
				  (lambda (sentinels uri body)
				    (mapc (lambda (func)
					    (funcall func uri body))
					  sentinels)
				    ;; Resolve the rest of requests.
				    (setq twindrill-url-request-resolving-p
					  nil)
				    (twindrill-resolve-url-request))
				  sentinels uri body)
	  ;;  Without the following nil, it seems that the value of
	  ;; `sentinels' is displayed.
	  nil)))))

(defun twindrill-url-retrieve-async-clean-up-sentinel (proc status connection-info)
  (when (memq status '(exit signal closed failed))
    (let* ((uri (cdr (assq 'uri connection-info)))
	   (current (gethash uri twindrill-url-data-hash)))
      (when (or (null current) (integerp current))
	;; Increment the counter on failure and then retry retrieval.
	(puthash uri (1+ (or current 0)) twindrill-url-data-hash)
	(setq twindrill-url-request-resolving-p nil)
	(twindrill-resolve-url-request)))))

(defun twindrill-url-retrieve-async (url &optional sentinel)
  "Retrieve URL asynchronously and call SENTINEL with the retrieved data.
The request is placed at the last of queries queue. When the data has been
retrieved and Emacs remains idle a certain time specified by
`twindrill-url-request-sentinel-delay', SENTINEL will be called as
 (funcall SENTINEL URL url-data).
The retrieved data can be referred as (gethash URL twindrill-url-data-hash)."
  (let ((data (gethash url twindrill-url-data-hash)))
    (cond
     ((or (null data) (integerp data))
      (add-to-list 'twindrill-url-request-list url t)
      (when sentinel
	(let ((current (gethash url twindrill-url-request-sentinel-hash)))
	  (unless (member sentinel current)
	    (puthash url (cons sentinel current)
		     twindrill-url-request-sentinel-hash))))
      (twindrill-resolve-url-request)
      nil)
     (t
      ;; URL has been already retrieved.
      (twindrill-run-on-idle twindrill-url-request-sentinel-delay
			      sentinel url data)
      data))))

;;;;
;;;; XML parser
;;;;

(defun twindrill-ucs-to-char-internal (code-point)
  ;; Check (featurep 'unicode) is a workaround with navi2ch to avoid
  ;; error "error in process sentinel: Cannot open load file:
  ;; unicode".
  ;; 
  ;; Details: navi2ch prior to 1.8.3 (which is currently last release
  ;; version as of 2010-01-18) always define `ucs-to-char' as autoload
  ;; file "unicode(.el)" (which came from Mule-UCS), hence it breaks
  ;; `ucs-to-char' under non Mule-UCS environment. The problem is
  ;; fixed in navi2ch dated 2010-01-16 or later, but not released yet.
  (if (and (featurep 'unicode) (functionp 'ucs-to-char))
      (ucs-to-char code-point)
    ;; Emacs21 have a partial support for UTF-8 text, so it can decode
    ;; only parts of a text with Japanese.
    (decode-char 'ucs code-point)))

(defvar twindrill-unicode-replacement-char
  ;; "Unicode Character 'REPLACEMENT CHARACTER' (U+FFFD)"
  (or (twindrill-ucs-to-char-internal #xFFFD)
      ??)
  "*Replacement character returned by `twindrill-ucs-to-char' when it fails
to decode a code.")

(defun twindrill-ucs-to-char (code-point)
  "Return a character specified by CODE-POINT in Unicode.
If it fails to decode the code, return `twindrill-unicode-replacement-char'."
  (or (twindrill-ucs-to-char-internal code-point)
      twindrill-unicode-replacement-char))

(defadvice decode-char (after twindrill-add-fail-over-to-decode-char)
  (when (null ad-return-value)
    (setq ad-return-value twindrill-unicode-replacement-char)))

(defun twindrill-xml-parse-region (&rest args)
  "Wrapped `xml-parse-region' in order to avoid decoding errors.
After activating the advice `twindrill-add-fail-over-to-decode-char',
`xml-parse-region' is called. This prevents `xml-parse-region' from
exiting abnormally by decoding unknown numeric character reference."
  (let ((activated (ad-is-active 'decode-char)))
    (ad-enable-advice
     'decode-char 'after 'twindrill-add-fail-over-to-decode-char)
    (ad-activate 'decode-char)
    (unwind-protect
	(condition-case err
	    (apply 'xml-parse-region args)
	  (error
	   (message "Failed to parse the retrieved XML.")
	   nil))
      (ad-disable-advice 'decode-char 'after
			 'twindrill-add-fail-over-to-decode-char)
      (if activated
	  (ad-activate 'decode-char)
	(ad-deactivate 'decode-char)))))

;;;;
;;;; JSON parser with a fallback character
;;;;

(defconst twindrill-surrogate-pair-regexp
  (if (<= 23 emacs-major-version)
      ;; Literal strings such as "\uXXXX" is not allowed in Emacs 21
      ;; and earlier. A character of invalid code point such as U+D800
      ;; is not allowed in Emacs 22.
      ;; To avoid errors caused by literal strings invalid in Emacs 22
      ;; and earlier, the regexp is generated indirectly.
      (format "[%c-%c][%c-%c]"
	      (decode-char 'ucs #xd800)
	      (decode-char 'ucs #xdbff)
	      (decode-char 'ucs #xdc00)
	      (decode-char 'ucs #xdfff))
    ;; A regexp that never matches any strings.
    "\\'\\`")
  "Regexp to match a surrogate pair for CESU-8.
In Emacs 22 and earlier, this variable is initialized by a regexp
that never matches any string because code points for a surrogate pair,
from U+D800 to U+DFFF, are invalid.")

(defun twindrill-decode-surrogate-pairs-as-cesu-8 (str)
  "Decode surrogate pairs in STR similarly to CESU-8.
If STR includes surrogate pairs represented by code points from U+D800 to
U+DFFF, decode them with CESU-8 and return the result.

A character not in the Basic Multilingual Plane is represented by a surrogate
pair in JSON (RFC4627). This is similar to CESU-8. But the function
`json-read' in `json.el' does not correctly decode surrogate pairs. Therefore,
`json-read' may return a string including invalid code points from U+D800 to
U+DFFF. This function decodes such invalid code points."
  (let ((str str)
	(prev 0)
	(current 0)
	(result ""))
    (while (setq current
		 (string-match twindrill-surrogate-pair-regexp str prev))
      (let* ((next (match-end 0))
	     (decoded-str
	      (decode-coding-string
	       (mapconcat
		(lambda (c)
		  (let* ((code-point (encode-char c 'ucs))
			 (b1 (/ code-point #x100))
			 (b2 (% code-point #x100)))
		    (unibyte-string b1 b2)))
		(match-string 0 str)
		"")
	       'utf-16)))
	(setq result
	      (concat result
		      (substring str prev current)
		      decoded-str))
	(setq prev next)))
    (setq result (concat result (substring str prev)))
    result))

(defadvice json-read-string (after twindrill-decode-surrogate-pairs-as-cesu-8)
  (when (<= 23 emacs-major-version)
    (setq ad-return-value
	  (twindrill-decode-surrogate-pairs-as-cesu-8 ad-return-value))))

(defun twindrill-json-read (&rest args)
  "Wrapped `json-read' in order to avoid decoding errors.
`json-read' is called after activating the advice
`twindrill-add-fail-over-to-decode-char'.
This prevents `json-read' from exiting abnormally by decoding an unknown
numeric character reference."
  (let ((activated (ad-is-active 'decode-char))
	(json-activated (ad-is-active 'json-read-string)))
    (ad-enable-advice
     'decode-char 'after 'twindrill-add-fail-over-to-decode-char)
    (ad-activate 'decode-char)
    (ad-enable-advice 'json-read-string 'after
		      'twindrill-decode-surrogate-pairs-as-cesu-8)
    (ad-activate 'json-read-string)
    (unwind-protect
	(condition-case err
	    (apply 'json-read args)
	  (error
	   (message "Failed to parse the retrieved JSON.")
	   nil))
      (ad-disable-advice 'decode-char 'after
			 'twindrill-add-fail-over-to-decode-char)
      (ad-disable-advice 'json-read-string 'after
			 'twindrill-decode-surrogate-pairs-as-cesu-8)
      (if activated
	  (ad-activate 'decode-char)
	(ad-deactivate 'decode-char))
      (if json-activated
	  (ad-activate 'json-read-string)
	(ad-deactivate 'json-read-string))
      )))

;;;;
;;;; Window configuration
;;;;

(defun twindrill-set-window-end (window pos)
  (let* ((height (window-text-height window))
	 (n (- (- height 1))))
    (while (progn (setq n (1+ n))
		  (set-window-start
		   window
		   (with-current-buffer (window-buffer window)
		     (save-excursion
		       (goto-char pos)
		       (line-beginning-position n))))
		  (not (pos-visible-in-window-p pos window))))))

(defun twindrill-current-window-config (window-list)
  "Return window parameters of WINDOW-LIST."
  (mapcar (lambda (win)
	    (let ((start (window-start win))
		  (point (window-point win)))
	      `(,win ,start ,point)))
	  window-list))

(defun twindrill-restore-window-config-after-modification (config beg end)
  "Restore window parameters changed by modification on given region.
CONFIG is window parameters made by `twindrill-current-window-config'.
BEG and END mean a region that had been modified."
  (mapc (lambda (entry)
	  (let ((win (elt entry 0))
		(start (elt entry 1))
		(point (elt entry 2)))
	    (when (and (< beg start) (< start end))
	      (set-window-start win start))
	    (when (and (< beg point) (< point end))
	      (set-window-point win point))))
	config))

(defun twindrill-pop-to-buffer (buf)
  "Select the buffer BUF in some window.
The behavior is determined by the function specified by
`twindrill-pop-to-buffer-function'."
  (funcall twindrill-pop-to-buffer-function buf))

(defun twindrill-pop-to-buffer-simple (buf)
  "Select the buffer BUF by using `pop-to-buffer'."
  (let ((win (selected-window)))
    (pop-to-buffer buf)
    ;; This is required because the new window generated by `pop-to-buffer'
    ;; may hide the region following the current position.
    (twindrill-ensure-whole-of-status-is-visible win)))

(defun twindrill-pop-to-buffer-in-current-window (buf &optional win)
  "Select the buffer BUF in the window WIN by splitting it.
If WIN is nil, the selected window is splitted."
  (let* ((win (or win (selected-window)))
	 (size
	  (let ((rest (- (window-height win) 15)))
	    (if (<= rest 3)
		;; To avoid an error due to a too small window.
		nil
	      rest)))
	 (new-win (split-window win size)))
    (select-window new-win)
    (switch-to-buffer buf)))

(defun twindrill-pop-to-buffer-in-largest-window (buf)
  "Select the buffer BUF in the largest window by splitting it."
  (let ((win
	 (lexical-let ((max-area 0)
		       (largest-win nil))
	   (walk-windows
	    (lambda (win)
	      (let ((area (* (window-height win) (window-width win))))
		(when (< max-area area)
		  (setq max-area area)
		  (setq largest-win win)))))
	   largest-win)))
    (twindrill-pop-to-buffer-in-current-window buf win)))

(defun twindrill-pop-to-buffer-in-bottom-largest-window (buf)
  "Select the buffer BUF in the window largest on bottom by splitting it."
  (let* ((bottom-win-list
	  (lexical-let ((win-list '())
			(max-bottom 0))
	    (walk-windows
	     (lambda (win)
	       (let ((bottom (nth 3 (window-edges win))))
		 (cond
		  ((< max-bottom bottom)
		   (setq max-bottom bottom)
		   (setq win-list `(,win)))
		  ((= max-bottom bottom)
		   (setq win-list (cons win win-list)))
		  (t
		   nil)))))
	    win-list))
	 (win
	  (lexical-let ((max-area 0)
			(largest-win nil))
	    (mapc (lambda (win)
		    (let ((area (* (window-height win) (window-width win))))
		      (when (< max-area area)
			(setq largest-win win)
			(setq max-area area))))
		  bottom-win-list)
	    largest-win)))
    (twindrill-pop-to-buffer-in-current-window buf win)))

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

;;;;
;;;; Process info
;;;;

(defun twindrill-register-process (proc spec &optional str)
  (let ((str (or str (twindrill-timeline-spec-to-string spec))))
    (add-to-list 'twindrill-process-info-alist `(,proc ,spec ,str))))

(defun twindrill-release-process (proc)
  (let ((pair (assoc proc twindrill-process-info-alist)))
    (when pair
      (setq twindrill-process-info-alist
	    (delq pair twindrill-process-info-alist)))))

(defun twindrill-get-timeline-spec-from-process (proc)
  (let ((entry (assoc proc twindrill-process-info-alist)))
    (if entry
	(elt entry 1)
      nil)))

(defun twindrill-get-timeline-spec-string-from-process (proc)
  (let ((entry (assoc proc twindrill-process-info-alist)))
    (if entry
	(elt entry 2)
      nil)))

(defun twindrill-find-processes-for-timeline-spec (spec)
  (apply 'append
	 (mapcar
	  (lambda (pair)
	    (let ((proc (car pair))
		  (spec-info (cadr pair)))
	      (if (equal spec-info spec)
		  `(,proc)
		nil)))
	  twindrill-process-info-alist)))

(defun twindrill-remove-inactive-processes ()
  (let ((inactive-statuses '(nil closed exit failed signal)))
    (setq twindrill-process-info-alist
	  (apply 'append
		 (mapcar
		  (lambda (pair)
		    (let* ((proc (car pair))
			   (info (cdr pair))
			   (status (process-status proc)))
		      (if (memq status inactive-statuses)
			  nil
			`((,proc ,@info)))))
		  twindrill-process-info-alist)))))

(defun twindrill-process-active-p (&optional spec)
  (twindrill-remove-inactive-processes)
  (if spec
      (twindrill-find-processes-for-timeline-spec spec)
    twindrill-process-info-alist))

;;;;
;;;; Server info
;;;;

(defun twindrill-update-api-table (spec api-string)
  "Register a pair of a timeline spec and an API for retrieving the timeline.
SPEC is a timeline spec. API-STRING is an identifier of an API for retrieving
the timeline."
  (let ((current (assoc spec twindrill-timeline-spec-to-api-table)))
    (if (null current)
	(add-to-list 'twindrill-timeline-spec-to-api-table
		     `(,spec . ,api-string))
      (setcdr current api-string))))

(defun twindrill-make-rate-limit-alist (header-info)
  "Make a rate-limit information alist from HEADER-INFO.
Key symbols of a returned alist are following; limit, remaining, reset-time.
Values bound to limit and remaining is a positive integer and
one bound to reset-time is an Emacs time (result of `seconds-to-time')."
  (let ((symbol-table
	 '(("X-Rate-Limit-Limit" . limit)
	   ("X-Rate-Limit-Remaining" . remaining)
	   ("X-Rate-Limit-Reset" . reset-time)
	   ;; For Twitter API v1.0.
	   ("X-RateLimit-Limit" . limit)
	   ("X-RateLimit-Remaining" . remaining)
	   ("X-RateLimit-Reset" . reset-time))))
    (remove
     nil
     (mapcar (lambda (entry)
	       (let ((sym
		      (cdr
		       (twindrill-assoc-string (car entry) symbol-table t))))
		 (cond
		  ((memq sym '(limit remaining))
		   `(,sym . ,(string-to-number (cdr entry))))
		  ((eq sym 'reset-time)
		   `(,sym
		     . ,(seconds-to-time (string-to-number (cdr entry)))))
		  (t
		   nil))))
	     header-info))))

(defun twindrill-update-rate-limit-info (api-string spec header-info)
  "Register rate-limit information.
API-STRING is an identifier of an API. SPEC is a timeline spec that had been
retrieved by the API. HEADER-INFO is an alist generated from the HTTP response
header of the API."
  (let* ((api-string
	  (if (eq twindrill-service-method 'twitter)
	      ;; The key for Twitter API v1.0 is nil.
	      nil
	    api-string))
	 (current (assoc api-string twindrill-api-limit-info-alist))
	 (rate-limit-alist (twindrill-make-rate-limit-alist header-info)))
    (twindrill-update-api-table spec api-string)
    (if (null current)
	(add-to-list 'twindrill-api-limit-info-alist
		     `(,api-string . ,rate-limit-alist))
      (setcdr current rate-limit-alist))))

(defun twindrill-update-server-info (connection-info header-info)
  (let* ((new-entry-list (mapcar 'car header-info))
	 (account-info (cdr (assq 'account-info connection-info)))
	 (account
	  (twindrill-get-from-account-info "screen_name" account-info))
	 (spec (cdr (assq 'timeline-spec connection-info)))
	 (api-string
	  (cdr (assq 'uri-without-query (assq 'request connection-info)))))
    (twindrill-update-rate-limit-info api-string spec header-info)
    (when (remove t (mapcar
		     (lambda (entry)
		       (equal (assoc entry header-info)
			      (assoc entry twindrill-server-info-alist)))
		     new-entry-list))
      (setq twindrill-server-info-alist
	    (append header-info
		    (remove nil (mapcar
				 (lambda (entry)
				   (if (member (car entry) new-entry-list)
				       nil
				     entry))
				 twindrill-server-info-alist))))
      (when twindrill-display-remaining
	(mapc (lambda (buffer)
		(with-current-buffer buffer
		  (twindrill-update-mode-line)))
	      (twindrill-get-buffer-list))))
    ;; cookie
    (let* ((new-cookies
	    (twindrill-extract-cookie connection-info header-info))
	   (old-cookies (cdr (assoc account twindrill-cookie-alist)))
	   (updated-cookies
	    (append new-cookies
		    (remove nil
			    (mapcar (lambda (cookie)
				      (unless (assoc (car cookie) new-cookies)
					cookie))
				    old-cookies)))))
      (setq twindrill-cookie-alist
	    (cons (cons account updated-cookies)
		  (remove nil
			  (mapcar (lambda (entry)
				    (unless (equal account (car entry))
				      entry))
				  twindrill-cookie-alist)))))
    header-info))

(defun twindrill-extract-cookie (connection-info header-info)
  (remove
   nil
   (mapcar
    (lambda (entry)
      (let ((header-item (car entry))
	    (header-value (cdr entry)))
	(when (and (string= header-item "Set-Cookie")
		   (string-match "\\([^= ]*\\) *= *\\([^; ]*\\) *;? *"
				 header-value))
	  ;; For ease of implementation, the followings are assumed.
	  ;; 1. Each response header includes only one cookie.
	  ;; 2. `value' of cookie is a token, not a quoted string.
	  ;; 3. Attributes except `domain', `expires' and `path' are ignored.
	  (let* ((name (downcase (match-string 1 header-value)))
		 (value (match-string 2 header-value))
		 (attributes
		  (mapcar
		   (lambda (str)
		     (when (string-match "\\` *\\([^ ]*\\) *= *\\(.*\\)\\'"
					 str)
		       (let ((attr (downcase (match-string 1 str)))
			     (value (match-string 2 str)))
			 (cond
			  ((string= attr "domain")
			   `(domain . ,value))
			  ((string= attr "expires")
			   `(expires
			     . ,(apply 'encode-time
				       (parse-time-string
					(replace-regexp-in-string
					 "-" " " value)))))
			  ((string= attr "path")
			   `(path . ,value))
			  (t
			   nil)))))
		   (split-string (substring header-value (match-end 0))
				 " *; *")))
		 (additional-attributes
		  `(,@(let* ((domain (cdr (assq 'domain attributes)))
			     (request (cdr (assq 'request connection-info)))
			     (host (cdr (assq 'host request)))
			     (prefix
			      (if domain
				  (regexp-quote domain)
				(concat "\\`" (regexp-quote host)))))
			`((domain-regexp . ,(concat prefix "\\'")))))))
	    `(,name
	      (value . ,value)
	      ,@attributes
	      ,@additional-attributes)))))
    header-info)))

(defun twindrill-make-cookie-string (request account-info)
  (let ((account
	 (twindrill-get-from-account-info "screen_name" account-info))
	(current-time (current-time))
	(host (cdr (assq 'host request))))
    (when account
      (mapconcat
       'identity
       (remove nil
	       (mapcar
		(lambda (entry)
		  (let* ((expires (cdr (assq 'expires entry)))
			 (not-expired (or (null expires)
					  (time-less-p current-time expires)))
			 (domain-regexp (cdr (assq 'domain-regexp entry))))
		    (when (and not-expired
			       (string-match domain-regexp host))
		      (format "%s=%s" (car entry) (cdr (assq 'value entry))))))
		(cdr (assoc account twindrill-cookie-alist))))
       ";"))))

(defun twindrill-get-ratelimit-alist (&optional spec)
  (let ((api-string
	 (cdr (assoc spec twindrill-timeline-spec-to-api-table))))
    (cdr (assoc api-string twindrill-api-limit-info-alist))))

(defun twindrill-get-ratelimit-remaining (&optional spec)
  (or (cdr (assq 'remaining (twindrill-get-ratelimit-alist spec)))
      0))

(defun twindrill-get-ratelimit-limit (&optional spec)
  (or (cdr (assq 'limit (twindrill-get-ratelimit-alist spec)))
      0))

(defun twindrill-get-ratelimit-indicator-string (&optional spec)
  "Make an indicator string of rate-limit information of SPEC."
  (cond
   ((eq twindrill-service-method 'twitter)
    ;; Twitter API v1.0.
    (format "%d/%d"
	    (twindrill-get-ratelimit-remaining)
	    (twindrill-get-ratelimit-limit)))
   (t
    (mapconcat
     (lambda (api-string)
       (let* ((alist (cdr (assoc api-string twindrill-api-limit-info-alist)))
	      (remaining (cdr (assq 'remaining alist)))
	      (limit (cdr (assq 'limit alist))))
	 (format "%s/%s"
		 (if remaining (number-to-string remaining) "?")
		 (if limit (number-to-string limit) "?"))))
     (twindrill-remove-duplicates
      (mapcar (lambda (spec)
		(cdr (assoc spec twindrill-timeline-spec-to-api-table)))
	      (twindrill-get-primary-base-timeline-specs spec)))
     "+"))))

;;;;
;;;; Abstract layer for Twitter API
;;;;

(defun twindrill-api-path (&rest params)
  (mapconcat 'identity `(,twindrill-api-prefix ,@params) ""))

(defun twindrill-call-api (command args-alist &optional additional-info)
  "Call Twitter API and return the process object for the request.
Invoke `twindrill-call-api-with-account' with the main account specified
by `twindrill-get-main-account-info'.
For details of arguments, see `twindrill-call-api-with-account'."
  (let ((account-info-alist (twindrill-get-main-account-info)))
    (twindrill-call-api-with-account account-info-alist command args-alist
				      additional-info)))

(defun twindrill-call-api-with-account (account-info-alist command args-alist &optional additional-info)
  "Call Twitter API and return the process object for the request.
COMMAND is a symbol specifying API. ARGS-ALIST is an alist specifying
arguments for the API corresponding to COMMAND. Each key of ARGS-ALIST is a
symbol.
ACCOUNT-INFO-ALIST is an alist storing account information, which has
the following key;
\"screen_name\", \"oauth_token\" and \"oauth_token_secret\" for OAuth/xAuth,
\"screen_name\" and \"password\" for basic authentication.
ADDITIONAL-INFO is used as an argument ADDITIONAL-INFO of
`twindrill-send-http-request'. Sentinels associated to the returned process
receives it as the fourth argument. See also the function
`twindrill-send-http-request'.

The valid symbols as COMMAND follows:
retrieve-timeline -- Retrieve a timeline.
  Valid key symbols in ARGS-ALIST:
    timeline-spec -- the timeline spec to be retrieved.
    timeline-spec-string -- the string representation of the timeline spec.
    format -- (optional) the symbol specifying the format.
    number -- (optional) how many tweets are retrieved. It must be an integer.
      If nil, `twindrill-number-of-tweets-on-retrieval' is used instead.
      The maximum for search timeline is 100, and that for other timelines is
      `twindrill-max-number-of-tweets-on-retrieval'.
      If the given number exceeds the maximum, the maximum is used instead.
    max_id -- (optional) the maximum ID of retrieved tweets.
    since_id -- (optional) the minimum ID of retrieved tweets.
    sentinel -- (optional) the sentinel that processes the buffer consisting
      of retrieved data.. This is used as an argument SENTINEL of
      `twindrill-send-http-request' via `twindrill-http-get'.
      If nil, `twindrill-http-get-default-sentinel' is used.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twindrill-send-http-request' via
      `twindrill-http-get'.
    page -- (optional and valid only for favorites timeline) which page will
      be retrieved.
retrieve-single-tweet -- Retrieve a single tweet.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the tweet to be retrieved.
    username -- (optional) the screen name of the author of the tweet.
    format -- (optional) the symbol specifying the format.
    sentinel -- (optional) the sentinel that processes the buffer consisting
      of retrieved data.. This is used as an argument SENTINEL of
      `twindrill-send-http-request' via `twindrill-http-get'.
      If nil, `twindrill-http-get-default-sentinel' is used.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twindrill-send-http-request' via
      `twindrill-http-get'.
get-list-index -- Retrieve list names owned by a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username.
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twindrill-send-http-request'
      via `twindrill-http-get'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twindrill-send-http-request' via
      `twindrill-http-get'.
get-list-subscriptions -- Retrieve list names followed by a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username.
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twindrill-send-http-request'
      via `twindrill-http-get'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twindrill-send-http-request' via
      `twindrill-http-get'.
create-friendships -- Follow a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username which will be followed.
destroy-friendships -- Unfollow a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username which will be unfollowed.
create-favorites -- Mark a tweet as a favorite.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
destroy-favorites -- Remove a mark of a tweet as a favorite.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
update-status -- Post a tweet.
  Valid key symbols in ARGS-ALIST:
    status -- the string to be posted.
    in-reply-to-status-id -- (optional) the ID of a status that this post is
      in reply to.
destroy-status -- Destroy a tweet posted by the authenticated user itself.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
retweet -- Retweet a tweet.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
verify-credentials -- Verify the current credentials.
  Valid key symbols in ARGS-ALIST:
    sentinel -- the sentinel that processes returned information. This is used
      as an argument SENTINEL of `twindrill-send-http-request'
      via `twindrill-http-get'.
    clean-up-sentinel -- the clean-up sentinel that post-processes the buffer
      associated to the process. This is used as an argument CLEAN-UP-SENTINEL
      of `twindrill-send-http-request' via `twindrill-http-get'.
send-direct-message -- Send a direct message.
  Valid key symbols in ARGS-ALIST:
    username -- the username who the message is sent to.
    status -- the sent message.
mute -- Mute a user.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be muted.
    username -- the username who will be muted.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
unmute -- Un-mute a user.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be un-muted.
    username -- the username who will be un-muted.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
block -- Block a user.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be blocked.
    username -- the username who will be blocked.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
block-and-report-as-spammer -- Block a user and report him or her as a spammer.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be blocked.
    username -- the username who will be blocked.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
get-service-configuration -- Get the configuration of the server.
  Valid key symbols in ARGS-ALIST:
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twindrill-send-http-request'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twindrill-send-http-request'."
  (let* ((additional-info
	  `(,@additional-info
	    (service-method . ,twindrill-service-method))))
    (cond
     ((memq twindrill-service-method '(twitter statusnet))
      (twindrill-call-api-with-account-in-api1.0
       account-info-alist command args-alist additional-info))
     ((eq twindrill-service-method 'twitter-api-v1.1)
      (cond
       ((not (require 'json nil t))
	(error "`json.el' is required to use the Twitter REST API v1.1")
	nil)
       ((not twindrill-use-ssl)
	(error "SSL is required to use the Twitter REST API v1.1")
	nil)
       (t
	(twindrill-call-api-with-account-in-api1.1
	 account-info-alist command args-alist additional-info))))
     (t
      (error "`twindrill-service-method' is an invalid service method")
      ))))

(defun twindrill-call-api-with-account-in-api1.0 (account-info-alist command args-alist &optional additional-info)
  "Call the Twitter REST API v1.0 and return the process object for the request."
  (cond
   ((eq command 'retrieve-timeline)
    ;; Retrieve a timeline.
    (let* ((spec (cdr (assq 'timeline-spec args-alist)))
	   (spec-string (cdr (assq 'timeline-spec-string args-alist)))
	   (spec-type (car-safe spec))
	   (max-number (if (eq 'search spec-type)
			   100 ;; FIXME: refer to defconst.
			 twindrill-max-number-of-tweets-on-retrieval))
	   (number
	    (let ((number
		   (or (cdr (assq 'number args-alist))
		       (let* ((default-number 20)
			      (n twindrill-number-of-tweets-on-retrieval))
			 (cond
			  ((integerp n) n)
			  ((string-match "^[0-9]+$" n) (string-to-number n 10))
			  (t default-number))))))
	      (min (max 1 number) max-number)))
	   (number-str (number-to-string number))
	   (max_id (cdr (assq 'max_id args-alist)))
	   (page (cdr (assq 'page args-alist)))
	   (since_id (cdr (assq 'since_id args-alist)))
	   (word (when (eq 'search spec-type)
		   (cadr spec)))
	   (parameters
	    (cond
	     ((eq spec-type 'favorites)
	      `(("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when page `(("page" . ,page)))))
	     ((eq spec-type 'retweeted_by_user)
	      (let ((username (elt spec 1)))
		`(("count" . ,number-str)
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("include_entities" . "true")
		  ("screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     ((eq spec-type 'retweeted_to_user)
	      (let ((username (elt spec 1)))
		`(("count" . ,number-str)
		  ("include_entities" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     (t
	      `(,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))
		,@(cond
		   ((eq spec-type 'search)
		    `(("include_entities" . "true")
		      ("q" . ,word)
		      ("rpp" . ,number-str)
		      ("with_twitter_user_id". "true")))
		   ((eq spec-type 'list)
		    (let ((username (elt spec 1))
			  (list-name (elt spec 2)))
		      `(("include_entities" . "true")
			("include_rts" . "true")
			("owner_screen_name" . ,username)
			("per_page" . ,number-str)
			("slug" . ,list-name))))
		   ((eq spec-type 'user)
		    (let ((username (elt spec 1)))
		      `(("count" . ,number-str)
			("include_entities" . "true")
			("include_rts" . "true")
			("screen_name" . ,username))))
		   ((memq spec-type '(friends mentions public))
		    `(("include_entities" . "true")
		      ("count" . ,number-str)
		      ("include_rts" . "true")))
		   (t
		    ;; direct_messages
		    ;; direct_messages_sent
		    ;; home
		    ;; replies
		    ;; retweeted_by_me
		    ;; retweeted_to_me
		    ;; retweets_of_me
		    `(("include_entities" . "true")
		      ("count" . ,number-str))))))))
	   (format
	    (let ((format (cdr (assq 'format args-alist))))
	      (cond
	       ((and format (symbolp format))
		format)
	       ((eq spec-type 'search)
		'atom)
	       (t
		'xml))))
	   (format-str (symbol-name format))
	   (simple-spec-list
	    '((direct_messages . "direct_messages")
	      (direct_messages_sent . "direct_messages/sent")
	      (friends . "statuses/friends_timeline")
	      (home . "statuses/home_timeline")
	      (mentions . "statuses/mentions")
	      (public . "statuses/public_timeline")
	      (replies . "statuses/replies")
	      (retweeted_by_me . "statuses/retweeted_by_me")
	      (retweeted_to_me . "statuses/retweeted_to_me")
	      (retweets_of_me . "statuses/retweets_of_me")
	      (user . "statuses/user_timeline")))
	   (host (cond ((eq spec-type 'search) twindrill-api-search-host)
		       (t twindrill-api-host)))
	   (method
	    (cond
	     ((eq spec-type 'list)
	      (twindrill-api-path "lists/statuses"))
	     ((eq spec-type 'favorites)
	      (let ((user (elt spec 1)))
		(if user
		    (twindrill-api-path "favorites/" user)
		  (twindrill-api-path "favorites"))))
	     ((eq spec-type 'retweeted_by_user)
	      (twindrill-api-path "statuses/retweeted_by_user"))
	     ((eq spec-type 'retweeted_to_user)
	      (twindrill-api-path "statuses/retweeted_to_user"))
	     ((eq spec-type 'search)
	      twindrill-search-api-method)
	     ((assq spec-type simple-spec-list)
	      (twindrill-api-path (cdr (assq spec-type simple-spec-list))))
	     (t nil)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info (format . ,format))))
      (cond
       ((eq spec-type 'single)
	(let ((id (cadr spec))
	      (sentinel (or sentinel
			    'twindrill-retrieve-single-tweet-sentinel)))
	  (if (twindrill-find-status id)
	      ;; If the status has already retrieved, do nothing.
	      nil
	    (twindrill-call-api 'retrieve-single-tweet
				 `((id . ,id)
				   (format . ,format)
				   (sentinel . ,sentinel)
				   (clean-up-sentinel . ,clean-up-sentinel))
				 additional-info))))
       ((and host method)
	(twindrill-http-get account-info-alist host method parameters
			     format-str
			     additional-info sentinel clean-up-sentinel))
       (t
	(error "Invalid timeline spec")))))
   ((eq command 'retrieve-single-tweet)
    (let* ((id (cdr (assq 'id args-alist)))
	   (user-screen-name (cdr (assq 'username args-alist)))
	   (format
	    (let ((format (cdr (assq 'format args-alist))))
	      (cond
	       ((and format (symbolp format))
		format)
	       (t
		'xml))))
	   (format-str (symbol-name format))
	   (parameters '(("include_entities" . "true")))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info
			      (id . ,id)
			      (user-screen-name . ,user-screen-name)
			      (format . ,format))))
      (twindrill-http-get account-info-alist twindrill-api-host
			   (twindrill-api-path "statuses/show/" id)
			   parameters format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'get-list-index)
    ;; Get list names.
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twindrill-http-get account-info-alist twindrill-api-host
			   (twindrill-api-path username "/lists")
			   nil format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'get-list-subscriptions)
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twindrill-http-get account-info-alist twindrill-api-host
			   (twindrill-api-path username "/lists/subscriptions")
			   nil format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'create-friendships)
    ;; Create a friendship.
    (let ((username (cdr (assq 'username args-alist))))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "friendships/create")
			    `(("screen_name" . ,username))
			    nil additional-info)))
   ((eq command 'destroy-friendships)
    ;; Destroy a friendship
    (let ((username (cdr (assq 'username args-alist))))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "friendships/destroy")
			    `(("screen_name" . ,username))
			    nil additional-info)))
   ((eq command 'create-favorites)
    ;; Create a favorite.
    (let ((id (cdr (assq 'id args-alist))))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "favorites/create/" id)
			    nil nil additional-info)))
   ((eq command 'destroy-favorites)
    ;; Destroy a favorite.
    (let ((id (cdr (assq 'id args-alist))))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "favorites/destroy/" id)
			    nil nil additional-info)))
   ((eq command 'update-status)
    ;; Post a tweet.
    (let* ((status (cdr (assq 'status args-alist)))
	   (id (cdr (assq 'in-reply-to-status-id args-alist)))
	   (parameters
	    `(("status" . ,status)
	      ,@(when (eq twindrill-auth-method 'basic)
		  '(("source" . "twmode")))
	      ,@(when id `(("in_reply_to_status_id" . ,id))))))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "statuses/update")
			    parameters nil additional-info)))
   ((eq command 'destroy-status)
    ;; Destroy a status.
    (let* ((id (cdr (assq 'id args-alist)))
	   (format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format)))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "statuses/destroy/" id)
			    nil format-str additional-info
			    'twindrill-http-post-destroy-status-sentinel)))
   ((eq command 'retweet)
    ;; Post a retweet.
    (let ((id (cdr (assq 'id args-alist))))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "statuses/retweet/" id)
			    nil nil additional-info)))
   ((eq command 'verify-credentials)
    ;; Verify the account.
    (let ((sentinel (cdr (assq 'sentinel args-alist)))
	  (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twindrill-http-get account-info-alist twindrill-api-host
			   (twindrill-api-path "account/verify_credentials")
			   nil nil additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'send-direct-message)
    ;; Send a direct message.
    (let ((parameters
	   `(("screen_name" . ,(cdr (assq 'username args-alist)))
	     ("text" . ,(cdr (assq 'status args-alist))))))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "direct_messages/new")
			    parameters nil additional-info)))
   ((eq command 'block)
    ;; Block a user.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (parameters (if user-id
			   `(("user_id" . ,user-id))
			 `(("screen_name" . ,username)))))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "blocks/create")
			    parameters nil additional-info)))
   ((eq command 'block-and-report-as-spammer)
    ;; Report a user as a spammer and block him or her.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (parameters (if user-id
			   `(("user_id" . ,user-id))
			 `(("screen_name" . ,username)))))
      (twindrill-http-post account-info-alist twindrill-api-host
			    (twindrill-api-path "report_spam")
			    parameters nil additional-info)))
   ((eq command 'get-service-configuration)
    (let* ((format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format))
	   (request
	    (twindrill-make-http-request-from-uri
	     "GET" nil
	     (concat (if twindrill-use-ssl
			 "https"
		       "http")
		     "://" twindrill-api-host
		     "/"
		     (twindrill-api-path "help/configuration." format-str))))
	   (additional-info nil)
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twindrill-send-http-request request additional-info
				    sentinel clean-up-sentinel)))
   (t
    nil)))

(defun twindrill-call-api-with-account-in-api1.1 (account-info-alist command args-alist &optional additional-info)
  "Call the Twitter REST API v1.1 and return the process object for the request."
  (cond
   ((eq command 'retrieve-timeline)
    ;; Retrieve a timeline.
    (let* ((args-alist
	    (let* ((spec (cdr (assq 'timeline-spec args-alist)))
		   (spec-type (car-safe spec))
		   (table '((friends . (home))
			    (replies . (mentions)))))
	      (cond
	       ((memq spec-type '(friends replies))
		(let* ((alternative (cdr (assq spec-type table)))
		       (alternative-str
			(twindrill-timeline-spec-to-string alternative)))
		  (message
		   "Timeline spec %s is not supported in the Twitter REST API v1.1"
		   spec)
		  `((timeline-spec . ,alternative)
		    (timeline-spec-string . ,alternative-str)
		    ,@args-alist)))
	       (t
		args-alist))))
	   (spec (cdr (assq 'timeline-spec args-alist)))
	   (spec-string (cdr (assq 'timeline-spec-string args-alist)))
	   (spec-type (car-safe spec))
	   (max-number (if (eq 'search spec-type)
			   100 ;; FIXME: refer to defconst.
			 twindrill-max-number-of-tweets-on-retrieval))
	   (number
	    (let ((number
		   (or (cdr (assq 'number args-alist))
		       (let* ((default-number 20)
			      (n twindrill-number-of-tweets-on-retrieval))
			 (cond
			  ((integerp n) n)
			  ((string-match "^[0-9]+$" n) (string-to-number n 10))
			  (t default-number))))))
	      (min (max 1 number) max-number)))
	   (number-str (number-to-string number))
	   (max_id (cdr (assq 'max_id args-alist)))
	   (since_id (cdr (assq 'since_id args-alist)))
	   (word (when (eq 'search spec-type)
		   (cadr spec)))
	   (parameters
	    (cond
	     ((eq spec-type 'user)
	      (let ((username (elt spec 1)))
		`("api.twitter.com"
		  "1.1/statuses/user_timeline"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ("include_rts" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id)))
		  )))
	     ((eq spec-type 'list)
	      (let ((username (elt spec 1))
		    (list-name (elt spec 2)))
		`("api.twitter.com"
		  "1.1/lists/statuses"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ("include_rts" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("owner_screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id)))
		  ("slug" . ,list-name))))
	     ((eq spec-type 'direct_messages)
	      `("api.twitter.com"
		"1.1/direct_messages"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'direct_messages_sent)
	      `("api.twitter.com"
		"1.1/direct_messages/sent"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'favorites)
	      (let ((user (elt spec 1)))
		`("api.twitter.com"
		  "1.1/favorites/list"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ,@(when user `(("screen_name" . ,user)))
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     ((eq spec-type 'home)
	      `("api.twitter.com"
		"1.1/statuses/home_timeline"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'mentions)
	      `("api.twitter.com"
		"1.1/statuses/mentions_timeline"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'public)
	      (error
	       "Timeline spec %s is not supported in the Twitter REST API v1.1"
	       spec)
	      nil)
	     ((memq spec-type '(retweeted_by_me
				retweeted_by_user
				retweeted_to_me
				retweeted_to_user))
	      (error
	       "Timeline spec %s is not supported in the Twitter REST API v1.1"
	       spec)
	      nil)
	     ((eq spec-type 'retweets_of_me)
	      `("api.twitter.com"
		"1.1/statuses/retweets_of_me"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'single)
	      (let ((id (elt spec 1)))
		`("api.twitter.com"
		  "1.1/statuses/show"
		  ("id" . ,id)
		  ("include_entities" . "true"))))
	     ((eq spec-type 'search)
	      (let ((word (elt spec 1)))
		`("api.twitter.com"
		  "1.1/search/tweets"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("q" . ,word)
		  ("result_type" . "recent")
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     (t
	      (error
	       "Timeline spec %s is unknown"
	       spec-string)
	      nil)))
	   (format 'json)
	   (format-str (symbol-name format))
	   (host (elt parameters 0))
	   (method (elt parameters 1))
	   (http-parameters (nthcdr 2 parameters))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info (format . ,format)))
	   ;; special treatment for single timeline.
	   (id (cdr (assoc "id" http-parameters)))
	   (sentinel (or sentinel
			 (when (eq spec-type 'single)
			   'twindrill-retrieve-single-tweet-sentinel))))
      (cond
       ((null parameters)
	nil)
       ((not (and (string= format-str "json")
		  (require 'json nil t)))
	(error "The Twitter REST API v1.1 supports only JSON")
	nil)
       ((and (eq spec-type 'single)
	     (twindrill-find-status id))
	;; If the status has already retrieved, do nothing.
	nil)
       ((and host method)
	(twindrill-http-get account-info-alist host method http-parameters
			     format-str
			     additional-info sentinel clean-up-sentinel))
       (t
	(error "Invalid timeline spec")
	nil))))
   ((eq command 'retrieve-single-tweet)
    (let* ((id (cdr (assq 'id args-alist)))
	   (user-screen-name (cdr (assq 'username args-alist)))
	   (format
	    (let ((format (cdr (assq 'format args-alist))))
	      (cond
	       ((and format (symbolp format))
		format)
	       (t
		'json))))
	   (format-str (symbol-name format))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info
			      (id . ,id)
			      (user-screen-name . ,user-screen-name)
			      (format . ,format))))
      (twindrill-call-api-with-account-in-api1.1
       account-info-alist 'retrieve-timeline
       `((timeline-spec . (single ,id))
	 (format . ,format)
	 (sentinel . ,sentinel)
	 (clean-up-sentinel . ,clean-up-sentinel))
       additional-info)))
   ((eq command 'get-list-index)
    ;; Get list names.
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/lists/list")
	   (http-parameters `(("screen_name" . ,username)))
	   (format-str "json")
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twindrill-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'get-list-subscriptions)
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/lists/subscriptions")
	   (http-parameters
	    `(("count" . "20")
	      ("screen_name" . ,username)))
	   (format-str "json")
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twindrill-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'create-friendships)
    ;; Create a friendship.
    (let* ((username (cdr (assq 'username args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/friendships/create")
	   (http-parameters
	    `(("screen_name" . ,username)))
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'destroy-friendships)
    ;; Destroy a friendship
    (let* ((username (cdr (assq 'username args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/friendships/destroy")
	   (http-parameters
	    `(("screen_name" . ,username)))
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'create-favorites)
    ;; Create a favorite.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/favorites/create")
	   (http-parameters `(("id" . ,id)))
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'destroy-favorites)
    ;; Destroy a favorite.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/favorites/destroy")
	   (http-parameters `(("id" . ,id)))
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'update-status)
    ;; Post a tweet.
    (let* ((status (cdr (assq 'status args-alist)))
	   (id (cdr (assq 'in-reply-to-status-id args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/statuses/update")
	   (http-parameters
	    `(("status" . ,status)
	      ,@(when id `(("in_reply_to_status_id" . ,id)))))
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'destroy-status)
    ;; Destroy a status.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host "api.twitter.com")
	   (method (format "1.1/statuses/destroy/%s" id))
	   (http-parameters nil)
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info
			    'twindrill-http-post-destroy-status-sentinel)))
   ((eq command 'retweet)
    ;; Post a retweet.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host "api.twitter.com")
	   (method (format "1.1/statuses/retweet/%s" id))
	   (http-parameters nil)
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'verify-credentials)
    ;; Verify the account.
    (let* ((host "api.twitter.com")
	   (method "1.1/account/verify_credentials")
	   (http-parameters nil)
	   (format-str "json")
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twindrill-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'send-direct-message)
    ;; Send a direct message.
    (let* ((host "api.twitter.com")
	   (method "1.1/direct_messages/new")
	   (http-parameters
	    `(("screen_name" . ,(cdr (assq 'username args-alist)))
	      ("text" . ,(cdr (assq 'status args-alist)))))
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((memq command '(mute unmute))
    ;; Mute a user.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (host "api.twitter.com")
	   (method
	    (cdr (assq command '((mute . "1.1/mutes/users/create")
				 (unmute . "1.1/mutes/users/destroy")))))
	   (http-parameters (if user-id
				`(("user_id" . ,user-id))
			      `(("screen_name" . ,username))))
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'block)
    ;; Block a user.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/blocks/create")
	   (http-parameters (if user-id
				`(("user_id" . ,user-id))
			      `(("screen_name" . ,username))))
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'block-and-report-as-spammer)
    ;; Report a user as a spammer and block him or her.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/users/report_spam")
	   (http-parameters (if user-id
				`(("user_id" . ,user-id))
			      `(("screen_name" . ,username))))
	   (format-str "json"))
      (twindrill-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'get-service-configuration)
    (let* ((host "api.twitter.com")
	   (method "1.1/help/configuration")
	   (http-parameters nil)
	   (format-str "json")
	   (additional-info nil)
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twindrill-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   (t
    nil)))

;;;;
;;;; Service configuration
;;;;

(defconst twindrill-service-configuration-default
  '((short_url_length . 19)
    (short_url_length_https . 20))
  "Default value of `twindrill-service-configuration'.")
(defvar twindrill-service-configuration nil
  "Current server configuration.")
(defvar twindrill-service-configuration-queried nil)
(defvar twindrill-service-configuration-update-interval 86400
  "*Interval of updating `twindrill-service-configuration'.")

(defun twindrill-get-service-configuration (entry)
  (let ((pair (assq entry twindrill-service-configuration)))
    (if (null pair)
	(cdr (assq entry twindrill-service-configuration-default))
      (cdr pair))))

(defun twindrill-update-service-configuration (&optional ignore-time)
  "Update `twindrill-service-configuration' if necessary."
  (when (and
	 (memq twindrill-service-method '(twitter twitter-api-v1.1))
	 (null twindrill-service-configuration-queried)
	 (or ignore-time
	     (let ((current (twindrill-get-service-configuration 'time))
		   (interval
		    (seconds-to-time
		     twindrill-service-configuration-update-interval)))
	       (if (null current)
		   t
		 ;; If time passed more than `interval',
		 ;; update the configuration.
		 (time-less-p interval (time-since current))))))
    (setq twindrill-service-configuration-queried t)
    (twindrill-call-api
     'get-service-configuration
     '((sentinel . twindrill-update-service-configuration-sentinel)
       (clean-up-sentinel
	. twindrill-update-service-configuration-clean-up-sentinel)))))

(defun twindrill-update-service-configuration-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twindrill-get-content-subtype-symbol-from-header-info header-info)))
    (case-string
     status-code
     (("200")
      (let* ((conf-alist
	      (cond
	       ((eq format 'xml)
		(let ((xml
		       (twindrill-xml-parse-region (point-min) (point-max))))
		  (mapcar
		   (lambda (entry)
		     `(,(car entry) . ,(elt entry 2)))
		   (cddr (assq 'configuration xml)))))
	       ((eq format 'json)
		(twindrill-json-read))
	       (t
		(error "Format \"%s\" is not supported" format)
		nil)))
	     (entries '(short_url_length short_url_length_https)))
	(setq twindrill-service-configuration
	      `((time . ,(current-time))
		,@(mapcar (lambda (entry)
			    (let ((value (cdr (assq entry conf-alist))))
			      (cons
			       entry
			       (cond
				((stringp value)
				 (string-to-number value))
				(t
				 value)))))
			  entries)))
	(setq twindrill-service-configuration-queried nil)
	nil))
     (("400")
      ;; Rate limit exceeded.
      (setq twindrill-service-configuration-queried nil)
      (format "Response: %s"
	      (twindrill-get-error-message header-info connection-info)))
     (t
      (setq twindrill-service-configuration-queried nil)
      (format "Response: %s"
	      (twindrill-get-error-message header-info connection-info))))))

(defun twindrill-update-service-configuration-clean-up-sentinel (proc status connection-info)
  (when (not (twindrill-process-alive-p proc))
    (setq twindrill-service-configuration-queried nil)))

;;;;
;;;; Account authorization
;;;;

(defun twindrill-register-account-info (account-info)
  (setq twindrill-oauth-access-token-alist account-info))

(defun twindrill-get-main-account-info ()
  (cond
   ((eq twindrill-auth-method 'basic)
    `(("screen_name" . ,twindrill-username)
      ("password" . ,twindrill-password)))
   ((memq twindrill-auth-method '(oauth xauth))
    twindrill-oauth-access-token-alist)))

(defun twindrill-get-from-account-info (param account-info)
  (cdr (assoc param account-info)))

(defun twindrill-get-username ()
  (let ((account-info (twindrill-get-main-account-info)))
    (twindrill-get-from-account-info "screen_name" account-info)))

(defun twindrill-get-password ()
  (let ((account-info (twindrill-get-main-account-info)))
    (twindrill-get-from-account-info "password" account-info)))

(defun twindrill-make-basic-authentication-string (account-info)
  (concat "Basic "
	  (base64-encode-string
	   (concat (cdr (assoc "screen_name" account-info))
		   ":" (cdr (assoc "password" account-info))))))

(defun twindrill-make-oauth-authentication-string (account-info request)
  (let ((method (cdr (assq 'method request)))
	(access-token (cdr (assoc "oauth_token" account-info)))
	(access-token-secret (cdr (assoc "oauth_token_secret" account-info))))
    (unless (and (stringp access-token)
		 (stringp access-token-secret))
      (error "`account-info' has no valid OAuth token"))
    (twindrill-oauth-auth-str-access
     method
     (cdr (assq 'uri-without-query request))
     (cdr (assq 'encoded-query-alist request))
     twindrill-oauth-consumer-key twindrill-oauth-consumer-secret
     access-token access-token-secret)))

(defun twindrill-account-authorized-p ()
  (eq twindrill-account-authorization 'authorized))
(defun twindrill-account-authorization-queried-p ()
  (eq twindrill-account-authorization 'queried))

(defun twindrill-prepare-account-info ()
  "Return a pair of username and password.
If `twindrill-username' is nil, read it from the minibuffer.
If `twindrill-password' is nil, read it from the minibuffer."
  (let* ((username (or twindrill-username
		       (read-string "your twitter username: ")))
	 (password (or twindrill-password
		       (read-passwd (format "%s's twitter password: "
					    username)))))
    `(,username . ,password)))

(defun twindrill-has-oauth-access-token-p ()
  (let* ((required-entries '("oauth_token"
			     "oauth_token_secret"
			     "user_id"
			     "screen_name"))
	 (value-list
	  (mapcar
	   (lambda (key)
	     (cdr (assoc key twindrill-oauth-access-token-alist)))
	   required-entries)))
    (null (remove t (mapcar 'stringp value-list)))))

(defun twindrill-verify-credentials ()
  "Verify the account.

This function is an internal function, which should be called from
`twindrill-ensure-account-verification'.

If the account has been authorized already, return t.
Otherwise, this function tries to authorize the account.
If the authorization succeeded, return t.
If the authorization failed, return nil."
  (cond
   ((twindrill-account-authorized-p)
    ;; The account has been authorized already.
    t)
   ((not (twindrill-account-authorization-queried-p))
    ;; This function must be invoked from
    ;; `twindrill-ensure-account-verification', which updates the variable
    ;; `twindrill-account-authorization' into the symbol `queried'.
    (error "`twindrill-verify-credentials' is invoked multiple times.")
    nil)
   ((and (memq twindrill-auth-method '(oauth xauth))
	 (or (null twindrill-oauth-consumer-key)
	     (null twindrill-oauth-consumer-secret)))
    (message "Consumer for OAuth is not specified.")
    nil)
   ((twindrill-has-oauth-access-token-p)
    (let* ((username (cdr (assoc "screen_name"
				 (twindrill-get-main-account-info))))
	   (proc
	    (twindrill-call-api-with-account
	     (twindrill-get-main-account-info)
	     'verify-credentials
	     `((sentinel
		. twindrill-http-get-verify-credentials-sentinel)
	       (clean-up-sentinel
		. twindrill-http-get-verify-credentials-clean-up-sentinel)))))
      (cond
       ((null proc)
	(message "Process invocation for authorizing \"%s\" failed." username)
	;; Failed to authorize the account.
	nil)
       (t
	;; wait for verification to finish.
	(twindrill-wait-while nil 0.1
			       (and
				(twindrill-account-authorization-queried-p)
				(twindrill-process-alive-p proc)))
	(if (not (twindrill-account-authorization-queried-p))
	    ;; The query is completed.
	    (twindrill-account-authorized-p)
	  ;; If the process has been dead, wait a moment because
	  ;; Emacs may be in the middle of evaluating the sentinel.
	  (twindrill-wait-while
	   10 0.1
	   (twindrill-account-authorization-queried-p)
	   ;; Succeeded in authorizing the account.
	   t
	   ;; Display a message.
	   (message
	    "Status of Authorization process is `%s'. Type M-x twit to retry."
	    (process-status proc))
	   ;; Failed to authorize the account.
	   nil))))))
   ((eq twindrill-auth-method 'oauth)
    (let* ((scheme (if twindrill-oauth-use-ssl
		       "https"
		     "http"))
	   (request-token-url
	    (concat scheme twindrill-oauth-request-token-url-without-scheme))
	   (access-token-url
	    (concat scheme twindrill-oauth-access-token-url-without-scheme))
	   (token-alist
	    (twindrill-oauth-get-access-token
	     request-token-url
	     (lambda (token)
	       (concat scheme
		       twindrill-oauth-authorization-url-base-without-scheme
		       token))
	     access-token-url
	     twindrill-oauth-consumer-key twindrill-oauth-consumer-secret
	     "twindrill-mode")))
      (cond
       ((and (assoc "oauth_token" token-alist)
	     (assoc "oauth_token_secret" token-alist)
	     (assoc "screen_name" token-alist))
	(let ((username (cdr (assoc "screen_name" token-alist))))
	  (twindrill-register-account-info token-alist)
	  (message "Authorization for the account \"%s\" succeeded."
		   username)
	  (when (and twindrill-use-master-password
		     (twindrill-capable-of-encryption-p)
		     (not (file-exists-p twindrill-private-info-file)))
	    (twindrill-save-private-info-with-guide))
	  ;; Succeeded in authorizing the account.
	  t))
       (t
	;; There is no global account info that should be invalidated.
	;; Failed to authorize the account.
	(message "Authorization via OAuth failed. Type M-x twit to retry.")
	nil))))
   ((eq twindrill-auth-method 'xauth)
    (let* ((account-info (twindrill-prepare-account-info))
	   (scheme (if twindrill-oauth-use-ssl
		       "https"
		     "http"))
	   (access-token-url
	    (concat scheme twindrill-oauth-access-token-url-without-scheme))
	   (token-alist
	    (twindrill-xauth-get-access-token
	     access-token-url
	     twindrill-oauth-consumer-key twindrill-oauth-consumer-secret
	     (car account-info)
	     (cdr account-info))))
      ;; Dispose of password as recommended by Twitter.
      ;; http://dev.twitter.com/pages/xauth
      (setcdr account-info nil)
      (cond
       ((and token-alist
	     (assoc "oauth_token" token-alist)
	     (assoc "oauth_token_secret" token-alist))
	(twindrill-register-account-info token-alist)
	(message "Authorization for the account \"%s\" succeeded."
		 (twindrill-get-username))
	(when (and twindrill-use-master-password
		   (twindrill-capable-of-encryption-p)
		   (not (file-exists-p twindrill-private-info-file)))
	  (twindrill-save-private-info-with-guide))
	;; Succeeded in authorizing the account.
	t)
       (t
	;; Failed to authorize the account.
	(message "Authorization via xAuth failed. Type M-x twit to retry.")
	nil))))
   ((eq twindrill-auth-method 'basic)
    (let* ((account-info
	    (let ((pair (twindrill-prepare-account-info)))
	      `(("screen_name" . ,(car pair))
		("password" . ,(cdr pair)))))
	   ;; Bind account information locally to ensure that
	   ;; the variables are reset when the verification fails.
	   (twindrill-username (car account-info))
	   (twindrill-password (cdr account-info))
	   (proc
	    (twindrill-call-api-with-account
	     account-info
	     'verify-credentials
	     `((sentinel . twindrill-http-get-verify-credentials-sentinel)
	       (clean-up-sentinel
		. twindrill-http-get-verify-credentials-clean-up-sentinel)))))
      (cond
       ((null proc)
	(message "Process invocation for authorizing \"%s\" failed."
		 (twindrill-get-from-account-info "screen_name" account-info))
	;; Failed to authorize the account.
	nil)
       (t
	;; wait for verification to finish.
	(twindrill-wait-while nil 0.1
			       (and
				(twindrill-account-authorization-queried-p)
				(twindrill-process-alive-p proc)))
	(if (not (twindrill-account-authorization-queried-p))
	    ;; The query is finished.
	    (twindrill-account-authorized-p)
	  ;; If the process has been dead, wait a moment because
	  ;; Emacs may be in the middle of evaluating the sentinel.
	  (twindrill-wait-while
	   10 0.1
	   (twindrill-account-authorization-queried-p)
	   ;; Succeeded in authorizing the account.
	   t
	   ;; Display a message.
	   (message
	    "Status of Authorization process is `%s'. Type M-x twit to retry."
	    (process-status proc))
	   ;; Failed to authorize the account.
	   nil))))))
   (t
    (message "%s is invalid as an authorization method."
	     twindrill-auth-method)
    nil)))

(defun twindrill-http-get-verify-credentials-sentinel (proc status connection-info header-info)
  (let* ((status-line (cdr (assq 'status-line header-info)))
	 (status-code (cdr (assq 'status-code header-info)))
	 (account-info (cdr (assq 'account-info connection-info)))
	 (username
	  (twindrill-get-from-account-info "screen_name" account-info))
	 (password
	  (twindrill-get-from-account-info "password" account-info)))
    (case-string
     status-code
     (("200")
      (twindrill-register-account-info account-info)
      (setq twindrill-account-authorization 'authorized)
      (message "Authorization for the account \"%s\" succeeded." username)
      nil)
     (("401")
      (setq twindrill-account-authorization nil)
      (let ((error-mes
	     (format "Authorization for the account \"%s\" failed. Type M-x twit to retry with correct information."
		     username)))
	;; Invalidate the account info.
	(twindrill-register-account-info nil)
	(message "%s" error-mes)
	nil))
     (t
      (setq twindrill-account-authorization nil)
      (let ((error-mes
	     (format "Authorization for the account \"%s\" failed due to \"%s\"."
		     username status-line)))
	(message "%s" error-mes)
	nil)))))

(defun twindrill-http-get-verify-credentials-clean-up-sentinel (proc status connection-info)
  (when (and (memq status '(exit signal closed failed))
	     (eq twindrill-account-authorization 'queried))
    (setq twindrill-account-authorization nil)
    (let ((exit-status (cond
			((processp proc) (process-exit-status proc))
			(t 0)))
	  (command (process-command proc)))
      (if (= 0 exit-status)
	  (message "Authorization failed. Type M-x twit to retry.")
	(message "Authorization failed: %s exited abnormally (exit-status=%s)."
		 (car command) exit-status)))
    (setq twindrill-username nil)
    (setq twindrill-password nil)))

(defun twindrill-ensure-account-verification ()
  "Ensure verification of an account.

If an account has been already authorized, return t.
If a query of authorization is being processed, return nil.

Otherwise, this function tries to authorize an account by calling
`twindrill-verify-credentials'.
If the authorization succeeded, return t.
If the authorization failed, return nil."
  (cond
   ((twindrill-account-authorized-p)
    ;; The account has been already authorized.
    t)
   ((twindrill-account-authorization-queried-p)
    ;; The account has not been authorized yet.
    nil)
   (t
    (setq twindrill-account-authorization 'queried)
    (let ((result nil))
      (unwind-protect
	  (setq result (twindrill-verify-credentials))
	(if result
	    (setq twindrill-account-authorization 'authorized)
	  (setq twindrill-account-authorization nil)))
      result))))

;;;;
;;;; Status retrieval
;;;;

(defun twindrill-add-timeline-history (spec-string)
  (when (or (null twindrill-timeline-history)
	    (not (string= spec-string (car twindrill-timeline-history))))
    (twindrill-add-to-history 'twindrill-timeline-history spec-string))
  (let ((spec (twindrill-string-to-timeline-spec spec-string)))
    (when (and (twindrill-timeline-spec-is-user-p spec)
	       (or (null twindrill-user-history)
		   (not (string= spec-string (car twindrill-user-history)))))
      (twindrill-add-to-history 'twindrill-user-history (cadr spec)))))

(defun twindrill-remove-timeline-spec-string-from-history (spec-string)
  (setq twindrill-timeline-history
	(remove nil
		(mapcar
		 (lambda (str)
		   (if (twindrill-equal-string-as-timeline spec-string str)
		       nil
		     str))
		 twindrill-timeline-history))))

(defun twindrill-make-alist-of-forbidden-tweet (id &optional user-screen-name)
  (let ((created-at
	 (or
	  (twindrill-id-to-time id)
	  (apply 'encode-time
		 (parse-time-string "Jan 01 00:00:00 +0000 2012")))))
  `((forbidden . t)
    (id . ,id)
    (created-at . ,created-at)
    (user-name . nil)
    (user-screen-name . ,user-screen-name)
    (text . "SORRY, YOU ARE NOT AUTHORIZED TO SEE THIS TWEET.")
    )))

(defun twindrill-make-alist-of-non-existent-tweet (id &optional user-screen-name)
  (let ((created-at
	 (or
	  (twindrill-id-to-time id)
	  (apply 'encode-time
		 (parse-time-string "Jan 01 00:00:00 +0000 2012")))))
  `((forbidden . t)
    (id . ,id)
    (created-at . ,created-at)
    (user-name . nil)
    (user-screen-name . ,user-screen-name)
    (text . ,(format "THE TWEET WITH ID %s DOES NOT EXIST." id))
    )))

(defun twindrill-atom-xmltree-to-status-datum (atom-xml-entry)
  (let* ((id-str (car (cddr (assq 'id atom-xml-entry))))
	 (time-str (car (cddr (assq 'updated atom-xml-entry))))
	 (author-str (car (cddr (assq 'name (assq 'author atom-xml-entry)))))
	 (formatted-time-str
	  ;; ISO 8601
	  ;; Twitter -> "2010-05-08T05:59:41Z"
	  ;; StatusNet -> "2010-05-08T08:44:39+00:00"
	  (cond
	   ((string-match
	     "\\(.*\\)T\\(.*\\)\\(Z\\|\\([-+][0-2][0-9]\\):?\\([0-5][0-9]\\)\\)"
	     time-str)
	    ;; time-str is formatted as
	    ;; "Combined date and time in UTC:" in ISO 8601.
	    (let ((timezone (match-string 3 time-str)))
	      (format "%s %s %s"
		      (match-string 1 time-str) (match-string 2 time-str)
		      (if (string= "Z" timezone)
			  "+0000"
			(concat (match-string 4 time-str)
				(match-string 5 time-str))))))
	   (t
	    ;; unknown format?
	    time-str))))
    `((created-at . ,(date-to-time formatted-time-str))
      (id . ,(progn
	       (string-match ":\\([0-9]+\\)$" id-str)
	       (match-string 1 id-str)))
      ,@(let ((source (twindrill-decode-html-entities
		       (car (cddr (assq 'twitter:source atom-xml-entry))))))
	  `(,@(if (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				source)
		  (let ((uri (match-string-no-properties 1 source))
			(caption (match-string-no-properties 2 source)))
		    `((source . ,caption)
		      (source-uri . ,uri)))
		`((source . ,source)
		  (source-uri . "")))))
      (text . ,(twindrill-decode-html-entities
		(car (cddr (assq 'title atom-xml-entry)))))
      ,@(cond
	 ((and (eq twindrill-service-method 'statusnet)
	       (string-match "^\\([^ ]+\\)\\( (\\(.*\\))\\)?$" author-str))
	  ;; StatusNet
	  `((user-screen-name . ,(match-string 1 author-str))
	    (user-name . ,(or (match-string 3 author-str) ""))))
	 ((string-match "^\\([^ ]+\\) (\\(.*\\))$" author-str)
	  ;; Twitter (default)
	  `((user-screen-name . ,(match-string 1 author-str))
	    (user-name . ,(match-string 2 author-str))))
	 (t
	  '((user-screen-name . "PARSING FAILED!!")
	    (user-name . ""))))
      (user-profile-image-url
       . ,(let* ((link-items
		  (mapcar
		   (lambda (item)
		     (when (eq 'link (car-safe item))
		       (cadr item)))
		   atom-xml-entry))
		 (image-urls
		  (mapcar
		   (lambda (item)
		     (cond
		      ((and (eq twindrill-service-method 'statusnet)
			    (member '(rel . "related") item))
		       ;; StatusNet
		       (cdr (assq 'href item)))
		      ((member '(rel . "image") item)
		       ;; Twitter (default)
		       (cdr (assq 'href item)))
		      (t
		       nil)))
		   link-items)))
	    (car-safe (remq nil image-urls)))))))

(defun twindrill-atom-xmltree-to-status (atom-xmltree)
  (let ((entry-list
	 (apply 'append
		(mapcar (lambda (x)
			  (if (eq (car-safe x) 'entry) `(,x) nil))
			(cdar atom-xmltree)))))
    (mapcar 'twindrill-atom-xmltree-to-status-datum
	    entry-list)))

(eval-and-compile
  (defsubst twindrill-make-gap-list (text)
    "Return a list representing index gaps between TEXT and the decoded and normalized text.
Indices included in entities in a response from Twitter are calculated
with the assumption that \"<\" and \">\" are encoded as \"&lt;\" and \"&gt;\"
respectively and a Unicode combining character is considered as a character.
On rendering a tweet, twindrill-mode decode \"&lt;\" and \"&gt;\".
And also twindrill-mode normalize its text into canonically equivalent text
without combining characters.
Therefore, the indices in entities differ from the indices of the corresponding
positions in the decoded text.
In addition, the normalization to NFC also makes additional gaps between
the indices in entities and the corresponding positions.

This function assumes that TEXT is already decoded but not normalized.
From TEXT, the function calculates the gaps between the encoded text and the
decoded and normalized text.
This function returns a list of pairs representing the gaps.
For each pair, the car means the position in the original TEXT and the cdr
means the gap. The (car pair)-th character in the original TEXT corresponds
to the (- (car pair) (cdr pair))-th character in the decoded and normalized
text."
    (let ((result nil)
	  (regexp
	   (if (require 'ucs-normalize nil t)
	       (concat "\\(?:\\([<>]\\)\\|\\("
		       ucs-normalize-combining-chars-regexp "\\)\\)")
	     "\\([<>]\\)"))
	  (pos 0)
	  (gap 0))
      (while (string-match regexp text pos)
	(let ((shift (if (match-beginning 1)
			 3
		       1)))
	  (setq result
		(cons `(,(+ gap (match-end 0)) . ,(+ gap shift)) result))
	  (setq gap (+ shift gap)))
	(setq pos (match-end 0)))
      (reverse result)))

  (defun twindrill-get-gap (pos gap-list)
    "Return the gap at the specific position.
GAP-LIST must be generated by `twindrill-make-gap-list'."
    (let ((rest-gaps gap-list)
	  (gap 0))
      (while (and rest-gaps (< (caar rest-gaps) pos))
	(setq gap (cdar rest-gaps))
	(setq rest-gaps (cdr rest-gaps)))
      gap)))

(defun twindrill-normalize-raw-status (raw-status &optional ignore-retweet)
  (let* ((status-data (cddr raw-status))
	 (raw-retweeted-status (assq 'retweeted_status status-data)))
    (cond
     ((and raw-retweeted-status
	   (not ignore-retweet))
      (let ((retweeted-status
	     (twindrill-normalize-raw-status raw-retweeted-status t))
	    (retweeting-status
	     (twindrill-normalize-raw-status raw-status t))
	    (items-overwritten-by-retweet
	     '(id)))
	`(,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 (if (memq sym items-overwritten-by-retweet)
		     (let ((value-on-retweet
			    (cdr (assq sym retweeting-status))))
		       ;; Replace the value in `retweeted-status' with
		       ;; that in `retweeting-status'.
		       `(,sym . ,value-on-retweet))
		   `(,sym . ,value))))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeted-" (symbol-name sym)))
		   . ,value)))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeting-" (symbol-name sym)))
		   . ,value)))
	     retweeting-status))))
     (t
      (let ((assq-get (lambda (item seq)
			(car (cddr (assq item seq))))))
	`(,@(mapcar
	     (lambda (entry)
	       (let* ((sym (elt entry 0))
		      (sym-in-data (elt entry 1))
		      (encoded (elt entry 2))
		      (data (funcall assq-get sym-in-data status-data)))
		 `(,sym . ,(if encoded
			       (twindrill-decode-entities-after-parsing-xml
				data)
			     data))))
	     '(;; Raw entries.
	       (id id)
	       (in-reply-to-screen-name in_reply_to_screen_name)
	       (in-reply-to-status-id in_reply_to_status_id)
	       (recipient-screen-name recipient_screen_name)
	       ;; Encoded entries.
	       (text text t)
	       ))
	  ;; created_at
	  (created-at
	   . ,(date-to-time (funcall assq-get 'created_at status-data)))
	  ;; Replace "true" and "false" into t and nil.
	  ,@(mapcar (lambda (sym)
		      `(,sym . ,(string= "true"
					 (funcall assq-get sym status-data))))
		    '(favorited truncated))
	  ;; Entities.
	  ,(let* ((entity-data (cddr (assq 'entities status-data)))
		  (encoded-text (funcall assq-get 'text status-data))
		  (text
		   (twindrill-decode-entities-after-parsing-xml encoded-text))
		  (gap-list (twindrill-make-gap-list text)))
	     (list
	      'entity
	      ;; hashtags
	      (cons
	       'hashtags
	       (remove nil
		       (mapcar
			(lambda (entry)
			  (when (and (consp entry)
				     (eq 'hashtag (car entry)))
			    (let* ((data (cdr entry))
				   (start-str (cdr (assq 'start (car data))))
				   (end-str (cdr (assq 'end (car data))))
				   (start (if (stringp start-str)
					      (string-to-number start-str)
					    0))
				   (end (if (stringp end-str)
					    (string-to-number end-str)
					  0))
				   (gap (twindrill-get-gap start gap-list)))
			      `((start . ,(- start gap))
				(end . ,(- end gap))
				(text . ,(elt (assq 'text data) 2))))))
			(assq 'hashtags entity-data))))
	      ;; mentions
	      (cons
	       'mentions
	       (remove nil
		       (mapcar
			(lambda (entry)
			  (when (and (consp entry)
				     (eq 'user_mention (car entry)))
			    (let* ((data (cdr entry))
				   (start-str (cdr (assq 'start (car data))))
				   (end-str (cdr (assq 'end (car data))))
				   (start (if (stringp start-str)
					      (string-to-number start-str)
					    0))
				   (end (if (stringp end-str)
					    (string-to-number end-str)
					  0))
				   (gap (twindrill-get-gap start gap-list)))
			      `((start . ,(- start gap))
				(end . ,(- end gap))
				(id . ,(elt (assq 'id data) 2))
				(screen-name
				 . ,(elt (assq 'screen_name data) 2))
				(name
				 . ,(elt (assq 'name data) 2))))))
			(assq 'user_mentions entity-data))))
	      ;; urls
	      (cons
	       'urls
	       (remove nil
		       (mapcar
			(lambda (entry)
			  (when (and (consp entry)
				     (eq 'url (car entry)))
			    (let* ((data (cdr entry))
				   (start-str (cdr (assq 'start (car data))))
				   (end-str (cdr (assq 'end (car data))))
				   (start (if (stringp start-str)
					      (string-to-number start-str)
					    0))
				   (end (if (stringp end-str)
					    (string-to-number end-str)
					  0))
				   (gap (twindrill-get-gap start gap-list)))
			      `((start . ,(- start gap))
				(end . ,(- end gap))
				(url . ,(elt (assq 'url data) 2))
				(display-url
				 . ,(elt (assq 'display_url data) 2))
				(expanded-url
				 . ,(elt (assq 'expanded_url data) 2))))))
			(assq 'urls entity-data))))))
	  ;; Source.
	  ,@(let ((source (twindrill-decode-html-entities
			   (funcall assq-get 'source status-data))))
	      (if (and source
		       (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				     source))
		  (let ((uri (match-string-no-properties 1 source))
			(caption (match-string-no-properties 2 source)))
		    `((source . ,caption)
		      (source-uri . ,uri)))
		`((source . ,source)
		  (source-uri . ""))))
	  ;; Items related to the user that posted the tweet.
	  ,@(let ((user-data (cddr (assq 'user status-data))))
	      (mapcar
	       (lambda (entry)
		 (let* ((sym (elt entry 0))
			(sym-in-user-data (elt entry 1))
			(encoded (elt entry 2))
			(value (funcall assq-get sym-in-user-data user-data)))
		   `(,sym . ,(if encoded
				 (twindrill-decode-html-entities value)
			       value))))
	       '(;; Raw entries.
		 (user-id id)
		 (user-profile-image-url profile_image_url)
		 (user-url url)
		 ;; Encoded entries.
		 (user-name name t)
		 (user-screen-name screen_name t)
		 (user-location location t)
		 (user-description description t))))
	  ,@(let ((user-data (cddr (assq 'user status-data))))
	      (mapcar (lambda (entry)
			`(,(car entry)
			  . ,(string=
			      "true"
			      (funcall assq-get (cdr entry) user-data))))
		      '((user-protected . protected))))))))))

(defun twindrill-xmltree-to-status (xmltree)
  (setq xmltree
	(cond
	 ((eq 'direct-messages (caar xmltree))
	  `(,@(mapcar
	       (lambda (c-node)
		 `(status nil
			  (created_at
			   nil ,(caddr (assq 'created_at c-node)))
			  (id nil ,(caddr (assq 'id c-node)))
			  (text nil ,(caddr (assq 'text c-node)))
			  (source nil ,(format "%s" (car c-node))) ;; fake
			  (truncated nil "false")
			  (in_reply_to_status_id nil)
			  (in_reply_to_user_id
			   nil ,(caddr (assq 'recipient_id c-node)))
			  (favorited nil "false")
			  (recipient_screen_name
			   nil ,(caddr (assq 'recipient_screen_name c-node)))
			  (user nil ,@(cdddr (assq 'sender c-node)))
			  (entities nil ,@(cdddr (assq 'entities c-node)))))
	       (remove nil
		       (mapcar
			(lambda (node)
			  (and (consp node) (eq 'direct_message (car node))
			       node))
			(cdr-safe (assq 'direct-messages xmltree))))
	       )))
	 ((eq 'statuses (caar xmltree))
	  (cddr (car xmltree)))
	 (t ;; unknown format?
	  nil)))

  (mapcar #'twindrill-normalize-raw-status
 	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
 	  ;; On Emacs22, there may be blank strings
	  (remove nil (mapcar (lambda (x)
				(if (consp x) x))
			      xmltree))))

(defun twindrill-decode-entities-after-parsing-xml (encoded-str)
  "Decode ENCODED-STR retrieved by parsing XML and return the result.
On Emacs 22 and later, `xml-parse-region' resolves numeric character
references. It is redundant to resolve numeric character references
again. However, in a XML response from Twitter, the two characters,
\"<\" and \">\", are doubly escaped as \"&amp;lt;\" and \"&amp;gt;\",
respectively. Then, they are represented as \"&lt;\" and \"&gt;\" in
the result of `xml-parse-region'. This function decodes them.

On Emacs 21, `xml-parse-region' does not resolve numeric character
references. This function decodes them."
  (cond
   ((null encoded-str)
    "")
   ((> 22 emacs-major-version)
    (replace-regexp-in-string
     "&#\\([0-9]+\\);"
     (lambda (str)
       (let ((number-entity
	      (progn
		(string-match "&#\\([0-9]+\\);" str)
		(match-string 1 str))))
	 (char-to-string
	  (twindrill-ucs-to-char (string-to-number number-entity)))))
     encoded-str))
   (t
    (replace-regexp-in-string
     "&\\(?:\\(gt\\)\\|\\(lt\\)\\);"
     (lambda (str)
       (if (match-beginning 1)
	   ">"
	 "<"))
     encoded-str))))

(defun twindrill-decode-html-entities (encoded-str)
  (if encoded-str
      (let ((cursor 0)
	    (found-at nil)
	    (result '()))
	(while (setq found-at
		     (string-match "&\\(#\\([0-9]+\\)\\|\\([a-zA-Z]+\\)\\);"
				   encoded-str cursor))
	  (when (> found-at cursor)
	    (list-push (substring encoded-str cursor found-at) result))
	  (let ((number-entity (match-string-no-properties 2 encoded-str))
		(letter-entity (match-string-no-properties 3 encoded-str)))
	    (cond (number-entity
		   (list-push
		    (char-to-string
		     (twindrill-ucs-to-char
		      (string-to-number number-entity))) result))
		  (letter-entity
		   (cond ((string= "gt" letter-entity) (list-push ">" result))
			 ((string= "lt" letter-entity) (list-push "<" result))
			 ((string= "quot" letter-entity) (list-push "\"" result))
			 (t (list-push "?" result))))
		  (t (list-push "?" result)))
	    (setq cursor (match-end 0))))
	(list-push (substring encoded-str cursor) result)
	(apply 'concat (nreverse result)))
    ""))

;; JSON
(defun twindrill-extract-common-element-from-json (json-object)
  "Extract common parameters of a tweet from JSON-OBJECT.
Return an alist including text, created_at and entities, which are common
to JSON objects from ordinary timeline and search timeline."
  (let* ((encoded-text (cdr (assq 'text json-object)))
	 (text
	  (twindrill-decode-entities-after-parsing-xml encoded-text))
	 (gap-list (twindrill-make-gap-list text))
	 (entities (cdr (assq 'entities json-object)))
	 (urls (cdr (assq 'urls entities)))
	 (hashtags (cdr (assq 'hashtags entities)))
	 (mentions (cdr (assq 'user_mentions entities)))
	 (media (cdr (assq 'media entities)))
	 (func
	  (lambda (entry sym-table)
	    (mapcar (lambda (sym-entry)
		      (let ((sym (car sym-entry))
			    (target (cdr sym-entry)))
			`(,sym . ,(cdr (assq target entry)))))
		    sym-table))))
    `((text . ,(twindrill-normalize-string text))
      (created-at
       . ,(apply 'encode-time
		 (parse-time-string (cdr (assq 'created_at json-object)))))
      (entity
       (hashtags . ,(mapcar (lambda (entry)
			      (let* ((indices (cdr (assq 'indices entry)))
				     (start (elt indices 0))
				     (end (elt indices 1))
				     (gap
				      (twindrill-get-gap start gap-list)))
				`((start . ,(- start gap))
				  (end . ,(- end gap))
				  (text
				   . ,(twindrill-normalize-string
				       (cdr (assq 'text entry)))))))
			    hashtags))
       (mentions . ,(mapcar (lambda (entry)
			      (let* ((indices (cdr (assq 'indices entry)))
				     (start (elt indices 0))
				     (end (elt indices 1))
				     (gap
				      (twindrill-get-gap start gap-list)))
				`((start . ,(- start gap))
				  (end . ,(- end gap))
				  (id . ,(cdr (assq 'id_str entry)))
				  (name
				   . ,(twindrill-normalize-string
				       (cdr (assq 'name entry))))
				  (screen-name
				   . ,(cdr (assq 'screen_name entry))))))
			    mentions))
       (urls . ,(mapcar (lambda (entry)
			  (let* ((indices (cdr (assq 'indices entry)))
				 (start (elt indices 0))
				 (end (elt indices 1))
				 (gap (twindrill-get-gap start gap-list)))
			    `((start . ,(- start gap))
			      (end . ,(- end gap))
			      (url . ,(cdr (assq 'url entry)))
			      (display-url
			       . ,(cdr (assq 'display_url entry)))
			      (expanded-url
			       . ,(cdr (assq 'expanded_url entry))))))
			urls))
       (media . ,(mapcar (lambda (entry)
			  (let* ((indices (cdr (assq 'indices entry)))
				 (start (elt indices 0))
				 (end (elt indices 1))
				 (gap (twindrill-get-gap start gap-list)))
			    `((start . ,(- start gap))
			      (end . ,(- end gap))
			      (url . ,(cdr (assq 'url entry)))
			      (raw-entry . ,entry)
			      ,@(funcall func entry
					 '((media-url . media_url)
					   (display-url . display_url)
					   (expanded-url . expanded_url))))))
			 media)))
      (retweet-count . ,(cdr (assq 'retweet_count json-object)))
      (favorite-count . ,(cdr (assq 'favorite_count json-object)))
      )))

(defun twindrill-json-object-to-a-status (json-object)
  "Convert JSON-OBJECT representing a tweet into an alist representation.
JSON-OBJECT must originate in an ordinary timeline, not a search timeline.
To convert a JSON object from a search timeline, use
`twindrill-json-object-to-a-status-on-search'."
  (let* ((raw-retweeted-status (cdr (assq 'retweeted_status json-object))))
    (cond
     (raw-retweeted-status
      (let ((retweeted-status
	     (twindrill-json-object-to-a-status-base raw-retweeted-status))
	    (retweeting-status
	     (twindrill-json-object-to-a-status-base json-object))
	    (items-overwritten-by-retweet
	     '(id)))
	`(,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 (if (memq sym items-overwritten-by-retweet)
		     (let ((value-on-retweet
			    (cdr (assq sym retweeting-status))))
		       ;; Replace the value in `retweeted-status' with
		       ;; that in `retweeting-status'.
		       `(,sym . ,value-on-retweet))
		   `(,sym . ,value))))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeted-" (symbol-name sym)))
		   . ,value)))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeting-" (symbol-name sym)))
		   . ,value)))
	     retweeting-status))))
     (t
      (twindrill-json-object-to-a-status-base json-object)))))

(defun twindrill-json-object-to-a-status-base (json-object)
  (let ((user-data (cdr (assq 'user json-object))))
    `(,@(twindrill-extract-common-element-from-json json-object)
      ,@(let ((symbol-table
	       '((favorited . favorited)
		 (id_str . id)
		 (in_reply_to_screen_name . in-reply-to-screen-name)
		 (in_reply_to_status_id_str . in-reply-to-status-id)
		 (recipient_screen_name . recipient-screen-name)
		 (truncated . truncated))))
	  (remove nil
		  (mapcar
		   (lambda (entry)
		     (let* ((sym (car entry))
			    (value (cdr entry))
			    (value
			     (if (and (memq sym '(favorited truncated))
				      (eq value :json-false))
				 nil
			       value))
			    (dest (cdr (assq sym symbol-table))))
		       (when (and dest value)
			 `(,dest . ,value))))
		   json-object)))
      ;; source
      ,@(let ((source (cdr (assq 'source json-object))))
	  (if (and source
		   (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				 source))
	      (let ((uri (match-string-no-properties 1 source))
		    (caption (match-string-no-properties 2 source)))
		`((source . ,(twindrill-normalize-string caption))
		  (source-uri . ,uri)))
	    `((source . ,(twindrill-normalize-string source))
	      (source-uri . ""))))
      ;; user data
      ,@(let ((symbol-table
	       '((id_str . user-id)
		 (profile_image_url . user-profile-image-url)
		 (url . user-url)
		 (protected . user-protected)
		 (name . user-name)
		 (screen_name . user-screen-name)
		 (location . user-location)
		 (description . user-description))))
	  (remove nil
		  (mapcar (lambda (entry)
			    (let* ((sym (car entry))
				   (value (cdr entry))
				   (value
				    (cond
				     ((and (eq sym 'protected)
					   (eq value :json-false))
				      nil)
				     ((memq sym '(name location description))
				      (twindrill-normalize-string value))
				     (t
				      value))))
			      (when value
				(let ((dest (cdr (assq sym symbol-table))))
				  (when dest
				    `(,dest . ,value))))))
			  user-data))))))

(defun twindrill-json-object-to-a-status-on-search (json-object)
  "Convert JSON-OBJECT representing a tweet into an alist representation.
JSON-OBJECT must originate in a search timeline.
To convert a JSON object from other timelines, use
`twindrill-json-object-to-a-status'."
  `(,@(twindrill-extract-common-element-from-json json-object)
    ,@(let ((symbol-table
	     '((id_str . id)
	       (to_user . in-reply-to-screen-name)
	       (in_reply_to_status_id_str . in-reply-to-status-id)
	       ;; user data
	       (from_user_id_str . user-id)
	       (profile_image_url . user-profile-image-url)
	       (from_user_name . user-name)
	       (from_user . user-screen-name))))
	  (remove nil
		  (mapcar
		   (lambda (entry)
		     (let* ((sym (car entry))
			    (value (cdr entry))
			    (dest (cdr (assq sym symbol-table))))
		       (when (and dest value)
			 `(,dest . ,value))))
		   json-object)))
    ;; source
    ,@(let ((source
	       (twindrill-decode-html-entities
		(cdr (assq 'source json-object)))))
	  (if (and source
		   (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				 source))
	      (let ((uri (match-string-no-properties 1 source))
		    (caption (match-string-no-properties 2 source)))
		`((source . ,caption)
		  (source-uri . ,uri)))
	    `((source . ,source)
	      (source-uri . ""))))))

(defun twindrill-json-object-to-a-status-on-direct-messages (json-object)
  "Convert JSON-OBJECT representing a tweet into an alist representation.
JSON-OBJECT must originate in timelines related to direct messages.
To convert a JSON object from other timelines, use
`twindrill-json-object-to-a-status'."
  `(,@(twindrill-extract-common-element-from-json json-object)
    ,@(let ((symbol-table
	     '((id_str . id)
	       (recipient_screen_name . recipient-screen-name))))
	(remove nil
		  (mapcar
		   (lambda (entry)
		     (let* ((sym (car entry))
			    (value (cdr entry))
			    (dest (cdr (assq sym symbol-table))))
		       (when (and dest value)
			 `(,dest . ,value))))
		   json-object)))
    ;; sender
    ,@(let ((symbol-table
	     '((id_str . user-id)
	       (name . user-name)
	       (profile_image_url . user-profile-image-url)
	       (protected . user-protected)
	       (screen_name . user-screen-name))))
	(remove nil
		(mapcar
		 (lambda (entry)
		   (let* ((sym (car entry))
			  (value (cdr entry))
			  (value
			   (cond
			    ((eq sym 'protected)
			     (if (eq value :json-false)
				 nil
			       t))
			    ((eq value :json-false)
			     nil)
			    (t
			     value))))
		     (when value
		       (let ((dest (cdr (assq sym symbol-table))))
			 (when dest
			   `(,dest . ,value))))))
		 (cdr (assq 'sender json-object)))))))

;;;;
;;;; List info retrieval
;;;;

(defun twindrill-get-list-index (username)
  (twindrill-call-api
   'get-list-index
   `((username . ,username)
     (sentinel . twindrill-http-get-list-index-sentinel))))

(defun twindrill-get-list-subscriptions (username)
  (twindrill-call-api
   'get-list-subscriptions
   `((username . ,username)
     (sentinel . twindrill-http-get-list-subscriptions-sentinel))))

(defun twindrill-get-list-sync (username function)
  (setq twindrill-list-index-retrieved nil)
  (let ((proc (funcall function username)))
    (when proc
      (twindrill-wait-while nil 0.1
			     (and (not twindrill-list-index-retrieved)
				  (twindrill-process-alive-p proc)))
      (when (and (not twindrill-list-index-retrieved)
		 (not (twindrill-process-alive-p proc)))
	;; If the process has been dead, wait a moment because
	;; Emacs may be in the middle of evaluating the sentinel.
	(twindrill-wait-while 10 0.1
			       (not twindrill-list-index-retrieved)))))
  (cond
   ((null twindrill-list-index-retrieved)
    (message "Failed to retrieve %s's lists." username)
    nil)
   ((stringp twindrill-list-index-retrieved)
    (if (string= "" twindrill-list-index-retrieved)
	(message "%s does not have a list." username)
      (message "%s" twindrill-list-index-retrieved))
    nil)
   ((listp twindrill-list-index-retrieved)
    twindrill-list-index-retrieved)))

(defun twindrill-get-list-index-sync (username)
  (twindrill-get-list-sync username 'twindrill-get-list-index))

(defun twindrill-get-list-subscriptions-sync (username)
  (twindrill-get-list-sync username 'twindrill-get-list-subscriptions))

;;;;
;;;; Buffer info
;;;;

(defvar twindrill-buffer-info-list nil
  "List of buffers managed by `twindrill-mode'.")

(defun twindrill-get-buffer-list ()
  "Return buffers managed by `twindrill-mode'."
  (twindrill-unregister-killed-buffer)
  twindrill-buffer-info-list)

(defun twindrill-get-active-buffer-list ()
  "Return active buffers managed by `twindrill-mode', where statuses are
retrieved periodically."
  (twindrill-unregister-killed-buffer)
  (remove nil
	  (mapcar (lambda (buffer)
		    (if (twindrill-buffer-active-p buffer)
			buffer
		      nil))
		  twindrill-buffer-info-list)))

(defun twindrill-buffer-p (&optional buffer)
  "Return t if BUFFER is managed by `twindrill-mode'.
BUFFER defaults to the the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
	 (memq buffer twindrill-buffer-info-list))))

(defun twindrill-buffer-related-p ()
  "Return t if current buffer relates to `twindrill-mode'."
  (or (twindrill-buffer-p)
      (eq major-mode 'twindrill-edit-mode)
      (string= (buffer-name (current-buffer))
	       twindrill-debug-buffer)))

(defun twindrill-buffer-active-p (&optional buffer)
  "Return t if BUFFER is an active buffer managed by `twindrill-mode'.
BUFFER defaults to the the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (twindrill-buffer-p buffer)
	 (with-current-buffer buffer
	   twindrill-active-mode))))

(defun twindrill-get-buffer-from-spec (spec)
  "Return the buffer bound to SPEC. If no buffers are bound to SPEC,
return nil."
  (let* ((spec-string (twindrill-timeline-spec-to-string spec))
	 (buffers
	  (remove
	   nil
	   (mapcar
	    (lambda (buffer)
	      (if (twindrill-equal-string-as-timeline
		   spec-string
		   (twindrill-get-timeline-spec-string-for-buffer buffer))
		  buffer
		nil))
	    (twindrill-get-buffer-list)))))
    (if buffers
	;; We assume that the buffer with the same spec is unique.
	(car buffers)
      nil)))

(defun twindrill-get-buffer-from-spec-string (spec-string)
  "Return the buffer bound to SPEC-STRING. If no buffers are bound to it,
return nil."
  (let ((spec (twindrill-string-to-timeline-spec spec-string)))
    (and spec (twindrill-get-buffer-from-spec spec))))

(defun twindrill-get-timeline-spec-for-buffer (buffer)
  "Return the timeline spec bound to BUFFER. If BUFFER is not managed by
`twindrill-mode', return nil."
  (when (twindrill-buffer-p buffer)
    (with-current-buffer buffer
      twindrill-timeline-spec)))

(defun twindrill-get-timeline-spec-string-for-buffer (buffer)
  "Return the timeline spec string bound to BUFFER. If BUFFER is not managed
by `twindrill-mode', return nil."
  (when (twindrill-buffer-p buffer)
    (with-current-buffer buffer
      twindrill-timeline-spec-string)))

(defun twindrill-current-timeline-spec ()
  "Return the timeline spec bound to the current buffer. If it is not managed
by `twindrill-mode', return nil."
  (twindrill-get-timeline-spec-for-buffer (current-buffer)))

(defun twindrill-current-timeline-spec-string ()
  "Return the timeline spec string bound to the current buffer. If it is not
managed by `twindrill-mode', return nil."
  (twindrill-get-timeline-spec-string-for-buffer (current-buffer)))

(defun twindrill-unregister-buffer (buffer &optional keep-timer)
  "Unregister BUFFER from `twindrill-buffer-info-list'.
If BUFFER is the last managed buffer and KEEP-TIMER is nil, call
`twindrill-stop' to stop timers."
  (when (memq buffer twindrill-buffer-info-list)
    (setq twindrill-buffer-info-list
	  (delq buffer twindrill-buffer-info-list))
    (when (and (null twindrill-buffer-info-list)
	       (not keep-timer))
      (twindrill-stop))))

(defun twindrill-unregister-killed-buffer ()
  "Unregister buffers which has been killed."
  (mapc (lambda (buffer)
	  (unless (buffer-live-p buffer)
	    (twindrill-unregister-buffer buffer)))
	twindrill-buffer-info-list))

(defun twindrill-replace-spec-string-for-buffer (buffer spec-string)
  "Replace the timeline spec string for BUFFER with SPEC-STRING when
BUFFER is managed by `twindrill-mode' and SPEC-STRING is equivalent
to the current one."
  (when (twindrill-buffer-p buffer)
    (let ((current (twindrill-get-timeline-spec-string-for-buffer buffer)))
      (when (and (not (string= current spec-string))
		 (twindrill-equal-string-as-timeline current spec-string))
	(with-current-buffer buffer
	  (rename-buffer spec-string t)
	  (setq twindrill-timeline-spec-string spec-string))))))

(defun twindrill-set-active-flag-for-buffer (buffer active)
  "Set ACTIVE to active-flag for BUFFER."
  (when (twindrill-buffer-p buffer)
    (let ((current (twindrill-buffer-active-p buffer)))
      (when (or (and active (not current))
		(and (not active) current))
	(twindrill-toggle-activate-buffer buffer)))))

(defun twindrill-toggle-activate-buffer (&optional buffer)
  "Toggle whether to retrieve timeline for the current buffer periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (twindrill-buffer-p buffer)
      (with-current-buffer buffer
	(let* ((new-mode (not twindrill-active-mode))
	       (active-buffer-list (twindrill-get-active-buffer-list))
	       (start-timer (and new-mode (null active-buffer-list))))
	  (setq twindrill-active-mode new-mode)
	  (when start-timer
	    (twindrill-start))
	  (twindrill-update-mode-line))))))

(defun twindrill-activate-buffer (&optional buffer)
  "Activate BUFFER to retrieve timeline for it periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (twindrill-set-active-flag-for-buffer buffer t)))

(defun twindrill-deactivate-buffer (&optional buffer)
  "Deactivate BUFFER not to retrieve timeline for it periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (twindrill-set-active-flag-for-buffer buffer nil)))

(defun twindrill-kill-buffer (&optional buffer)
  "Kill BUFFER managed by `twindrill-mode'."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (twindrill-buffer-p buffer)
      (twindrill-deactivate-buffer buffer)
      (kill-buffer buffer)
      (twindrill-unregister-killed-buffer))))

(defun twindrill-get-managed-buffer (spec)
  "Return the buffer bound to SPEC. If no buffers are bound to SPEC, return
newly generated buffer.
SPEC may be a timeline spec or a timeline spec string."
  (let* ((original-spec spec)
	 (spec-string (if (stringp spec)
			  spec
			(twindrill-timeline-spec-to-string spec)))
	 ;; `spec-string' without text properties is required because
	 ;; Emacs21 displays `spec-string' with its properties on mode-line.
	 ;; In addition, copying `spec-string' keeps timeline-data from
	 ;; being modified by `minibuf-isearch.el'.
	 (spec-string (copy-sequence spec-string))
	 (spec (if (stringp spec-string)
		   (twindrill-string-to-timeline-spec spec-string)
		 nil)))
    (when (null spec)
      (error "\"%s\" is invalid as a timeline spec"
	     (or spec-string original-spec)))
    (set-text-properties 0 (length spec-string) nil spec-string)
    (twindrill-add-timeline-history spec-string)
    (let ((buffer (twindrill-get-buffer-from-spec spec)))
      (if buffer
	  (progn
	    (twindrill-replace-spec-string-for-buffer buffer spec-string)
	    (twindrill-update-mode-line)
	    buffer)
	(let ((buffer (generate-new-buffer spec-string))
	      (start-timer (null twindrill-buffer-info-list)))
	  (add-to-list 'twindrill-buffer-info-list buffer t)
	  (with-current-buffer buffer
	    (twindrill-mode-setup spec-string)
	    (twindrill-rerender-timeline-all buffer)
	    (when (twindrill-account-authorized-p)
	      (when start-timer
		;; If `buffer' is the first managed buffer,
		;; call `twindrill-start' to start timers.
		(twindrill-start))
	      (unless (and start-timer twindrill-active-mode)
		;; If `buffer' is active and the first managed buffer,
		;; `twindrill-start' invokes
		;; `twindrill-get-and-render-timeline' indirectly.
		;; Otherwise, `twindrill-get-and-render-timeline' should be
		;; invoked here.
		(twindrill-get-and-render-timeline))))
	  buffer)))))

;;;;
;;;; Icon mode
;;;;

(defvar twindrill-icon-mode nil
  "You MUST NOT CHANGE this variable directly.
You should change through function `twindrill-icon-mode'.")

(defun twindrill-icon-mode (&optional arg)
  "Toggle display of icon images on timelines.
With a numeric argument, if the argument is positive, turn on
icon mode; otherwise, turn off icon mode."
  (interactive "P")
  (unless (eq major-mode 'twindrill-mode)
    (error "Major-mode is not twindrill-mode"))
  (let ((prev-mode twindrill-icon-mode))
    (setq twindrill-icon-mode
	  (if (null arg)
	      (not twindrill-icon-mode)
	    (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twindrill-icon-mode)
      (twindrill-update-mode-line)
      (twindrill-rerender-timeline-all (current-buffer) t))))

(defvar twindrill-icon-prop-hash (make-hash-table :test 'equal)
  "Hash table for storing display properties of icon. The key is the size of
icon and the value is a hash. The key of the child hash is URL and its value
is the display property for the icon.")

(defcustom twindrill-convert-program (executable-find "convert")
  "*A path of the command which is invoked for image conversion.

The default is determined by searching \"convert\" in `exec-path'.
The command must be compatible with \"convert\" of ImageMagick."
  :group 'twindrill-mode
  :type 'file)

(defcustom twindrill-convert-fix-size 48
  "*Size of an icon image.

If nil, an icon image is displayed as is."
  :group 'twindrill-mode
  :type '(choice (const nil)
		 integer))

(defcustom twindrill-use-convert (not (null twindrill-convert-program))
  "*If non-nil, use \"convert\" for converting or resizing icon images."
  :group 'twindrill-mode
  :type 'boolean)

(defcustom twindrill-fallback-image-format 'xpm
  "*Fallback format used for displaying an image without a supproted format.
Images which Emacs does not supports are converted into the fallback image
format."
  :group 'twindrill-mode
  :type 'symbol)

(defcustom twindrill-use-profile-image-api nil
  "*Whether to use `profile_image' API for retrieving scaled icon images.
NOTE: This API is rate limited and is obsolete in the Twitter REST API v1.1."
  :group 'twindrill-mode
  :type 'boolean)

(defcustom twindrill-icon-storage-file
  (expand-file-name "~/.twindrill-mode-icons.gz")
  "*The file to which icon images are stored.
`twindrill-icon-storage-limit' determines the number icons stored in the
file.
The file is loaded with `with-auto-compression-mode'."
  :group 'twindrill-mode
  :type 'file)

(defcustom twindrill-use-icon-storage nil
  "*Whether to use the persistent icon storage.
If this variable is non-nil, icon images are stored to the file specified
by `twindrill-icon-storage-file'."
  :group 'twindrill-mode
  :type 'boolean)

(defvar twindrill-icon-storage-recent-icons nil
  "List of recently rendered icons.")

(defcustom twindrill-icon-storage-limit 500
  "*How many icons are stored in the persistent storage.
If `twindrill-use-icon-storage' is nil, this variable is ignored.
If a positive integer N, `twindrill-save-icon-properties' saves N icons that
have been recently rendered.
If nil, the function saves all icons."
  :group 'twindrill-mode
  :type '(choice (const nil)
		 integer))

(defconst twindrill-error-icon-data-pair
  '(xpm . "/* XPM */
static char * yellow3_xpm[] = {
\"16 16 2 1\",
\" 	c None\",
\".	c #FF0000\",
\"................\",
\".              .\",
\". .          . .\",
\".  .        .  .\",
\".   .      .   .\",
\".    .    .    .\",
\".     .  .     .\",
\".      ..      .\",
\".      ..      .\",
\".     .  .     .\",
\".    .    .    .\",
\".   .      .   .\",
\".  .        .  .\",
\". .          . .\",
\".              .\",
\"................\"};
")
  "Image used when the valid icon cannot be retrieved.")

(defun twindrill-update-icon-storage-recent-icons (size image-url spec)
  (unless (null twindrill-icon-storage-limit)
    (let ((dummy-icon-properties (twindrill-make-display-spec-for-icon
				  twindrill-error-icon-data-pair)))
      (unless (equal spec dummy-icon-properties)
	(let ((history-delete-duplicates t))
	  (twindrill-add-to-history 'twindrill-icon-storage-recent-icons
				     (list size image-url)
				     twindrill-icon-storage-limit))))))

(defun twindrill-get-display-spec-for-icon (image-url)
  (let ((hash
	 (gethash twindrill-convert-fix-size twindrill-icon-prop-hash)))
    (when hash
      (let ((spec (gethash image-url hash))
	    (size twindrill-convert-fix-size))
	(when spec
	  (twindrill-update-icon-storage-recent-icons size image-url spec)
	  spec)))))

(defun twindrill-convert-image-data (image-data dest-type &optional src-type)
  "Convert IMAGE-DATA into XPM format and return it. If it fails to convert,
return nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (buffer-disable-undo)
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  (require-final-newline nil)
	  ;; Bind `default-directory' to the temporary directory
	  ;; because it is possible that the directory pointed by
	  ;; `default-directory' has been already removed.
	  (default-directory temporary-file-directory))
      (insert image-data)
      (let* ((args
	      `(,@(when (<= emacs-major-version 22)
		    ;; Emacs22 and earlier raises "Color allocation error"
		    ;; on decoding a XPM image with opacity. To ignore
		    ;; opacity, the option "+matte" is added.
		    '("+matte"))
		,@(unless (fboundp 'create-animated-image)
		    '("-flatten"))
		,(if src-type (format "%s:-" src-type) "-")
		,@(when (integerp twindrill-convert-fix-size)
		    `("-resize"
		      ,(format "%dx%d" twindrill-convert-fix-size
			       twindrill-convert-fix-size)))
		,(format "%s:-" dest-type)))
	     (exit-status
	      (apply 'call-process-region (point-min) (point-max)
		     twindrill-convert-program t `(t nil) nil args)))
	(if (equal 0 exit-status)
	    (buffer-string)
	  ;; failed to convert the image.
	  nil)))))

(defun twindrill-create-image-pair (image-data)
  "Return a pair of image type and image data.
IMAGE-DATA is converted by `convert' if the image type of IMAGE-DATA is not
available and `twindrill-use-convert' is non-nil."
  (let* ((image-type (and image-data (image-type-from-data image-data)))
	 (image-pair `(,image-type . ,image-data))
	 (converted-size
	  `(,twindrill-convert-fix-size . ,twindrill-convert-fix-size)))
    (cond
     ((null image-data)
      twindrill-error-icon-data-pair)
     ((and (image-type-available-p image-type)
	   (or (fboundp 'create-animated-image)
	       (not (and twindrill-use-convert
			 (eq image-type 'gif))))
	   (or (not (integerp twindrill-convert-fix-size))
	       (equal (image-size (create-image image-data image-type t) t)
		      converted-size)))
      image-pair)
     (twindrill-use-convert
      (let ((converted-data
	     (twindrill-convert-image-data image-data
					    twindrill-fallback-image-format)))
	(if converted-data
	    `(,twindrill-fallback-image-format . ,converted-data)
	  twindrill-error-icon-data-pair)))
     (t
      twindrill-error-icon-data-pair))))

(defun twindrill-register-image-spec (image-url spec size)
  (let ((hash (gethash size twindrill-icon-prop-hash)))
    (unless hash
      (setq hash (make-hash-table :test 'equal))
      (puthash size hash twindrill-icon-prop-hash))
    (puthash image-url spec hash)))

(defun twindrill-register-image-data (image-url image-data &optional size)
  (let ((image-pair (twindrill-create-image-pair image-data))
	(size (or size twindrill-convert-fix-size)))
    (when image-pair
      (let ((spec (twindrill-make-display-spec-for-icon image-pair)))
	(twindrill-register-image-spec image-url spec size)
	spec))))

(defun twindrill-make-slice-spec (image-spec)
  "Return slice property for reducing the image size by cropping it."
  (let* ((size (image-size image-spec t))
	 (width (car size))
	 (height (cdr size))
	 (fixed-length twindrill-convert-fix-size)
	 (half-fixed-length (/ fixed-length 2)))
    (if (or (< fixed-length width) (< fixed-length height))
	`(slice ,(max 0 (- (/ width 2) half-fixed-length))
		,(max 0 (- (/ height 2) half-fixed-length))
		,fixed-length ,fixed-length)
      `(slice 0 0 ,fixed-length ,fixed-length))))

(defun twindrill-make-display-spec-for-icon (image-pair)
  "Return the specification for `display' text property, which
limits the size of an icon image IMAGE-PAIR up to FIXED-LENGTH. If
the type of the image is not supported, nil is returned.

If the size of the image exceeds FIXED-LENGTH, the center of the
image are displayed."
  (let* ((type (car-safe image-pair))
	 (data (cdr-safe image-pair))
	 (raw-image-spec ;; without margins
	  (create-image data type t))
	 (slice-spec
	  (when (and twindrill-convert-fix-size (not twindrill-use-convert))
	    (twindrill-make-slice-spec raw-image-spec)))
	 (image-spec
	  (if (fboundp 'create-animated-image) ;; Emacs24 or later
	      (create-animated-image data type t :margin 2 :ascent 'center)
	    (create-image data type t :margin 2 :ascent 'center))))
    (if slice-spec
	`(display (,image-spec ,slice-spec))
      `(display ,image-spec))))

(defun twindrill-make-icon-string (beg end image-url)
  (let ((display-spec (twindrill-get-display-spec-for-icon image-url))
	(image-data (gethash image-url twindrill-url-data-hash))
	(properties (and beg (text-properties-at beg)))
	(icon-string (copy-sequence " ")))
    (when properties
      (add-text-properties 0 (length icon-string) properties icon-string))
    (cond
     (display-spec
      (let ((icon-string (apply 'propertize "_"
				(append properties display-spec))))
	;; Remove the property required no longer.
	(remove-text-properties 0 (length icon-string)
				'(need-to-be-updated nil)
				icon-string)
	icon-string))
     ((and (integerp image-data)
	   (<= twindrill-url-request-retry-limit image-data))
      ;; Try to retrieve the image no longer.
      (twindrill-register-image-data image-url nil)
      (twindrill-make-icon-string beg end image-url))
     ((and image-data (not (integerp image-data)))
      (twindrill-register-image-data image-url image-data)
      (twindrill-make-icon-string beg end image-url))
     (t
      (put-text-property 0 (length icon-string)
			 'need-to-be-updated
			 `(twindrill-make-icon-string ,image-url)
			 icon-string)
      (twindrill-url-retrieve-async image-url 'twindrill-register-image-data)
      icon-string))))

(defun twindrill-save-icon-properties (&optional filename)
  (let ((filename (or filename twindrill-icon-storage-file))
	(stored-data
	 (cond
	  ((null twindrill-icon-storage-limit)
	   (let ((result nil)
		 (dummy-icon-properties (twindrill-make-display-spec-for-icon
					 twindrill-error-icon-data-pair)))
	     (maphash
	      (lambda (size hash)
		(maphash (lambda (url properties)
			   (unless (equal properties dummy-icon-properties)
			     (setq result (cons (list size url) result))))
			 hash))
	      twindrill-icon-prop-hash)
	     result))
	  (t
	   (reverse twindrill-icon-storage-recent-icons))))
	;; Bind `default-directory' to the temporary directory
	;; because it is possible that the directory pointed by
	;; `default-directory' has been already removed.
	(default-directory temporary-file-directory))
    (when (require 'jka-compr nil t)
      (with-auto-compression-mode
	(let ((coding-system-for-write 'binary))
	  (with-temp-file filename
	    (insert "( 2 ")
	    (prin1 (cons 'emacs-version emacs-version) (current-buffer))
	    (insert "(icon-list ")
	    (mapc
	     (lambda (entry)
	       (let* ((size (elt entry 0))
		      (url (elt entry 1))
		      (properties
		       (gethash url
				(gethash size twindrill-icon-prop-hash))))
		 (insert (if size
			     (format "(%d " size)
			   "(nil "))
		 (prin1 url (current-buffer))
		 (insert " ")
		 (prin1 properties (current-buffer))
		 (insert ")\n")))
	     stored-data)
	    (insert "))")))))))

(defun twindrill-load-icon-properties (&optional filename)
  (let* ((filename (or filename twindrill-icon-storage-file))
	 ;; Bind `default-directory' to the temporary directory
	 ;; because it is possible that the directory pointed by
	 ;; `default-directory' has been already removed.
	 (default-directory temporary-file-directory)
	 (data
	  (with-temp-buffer
	    (condition-case err
		(cond
		 ((and (require 'jka-compr)
		       (file-exists-p filename))
		  (with-auto-compression-mode
		    (let ((coding-system-for-read 'binary)
			  (coding-system-for-write 'binary))
		      (insert-file-contents filename)))
		  (read (current-buffer)))
		 (t
		  nil))
	      (error
	       (message "Failed to load icon images. %s" (cdr err))
	       nil)))))
    (cond
     ((equal 2 (car data))
      (let ((version (cdr (assq 'emacs-version data))))
	(cond
	 ((or (equal version emacs-version)
	      (y-or-n-p
	       (format "%s is generated by Emacs %s! Use it?"
		       filename version)))
	  (mapc (lambda (entry)
		  (let ((size (elt entry 0))
			(url (elt entry 1))
			(properties (elt entry 2)))
		    (twindrill-update-icon-storage-recent-icons size url
								 properties)
		    (twindrill-register-image-spec url properties size)))
		(cdr (assq 'icon-list data))))
	 (t
	  (message "Stopped loading icons")))))
     (t
      (mapc (lambda (entry)
	      (let ((size (car entry))
		    (prop-alist (cdr entry)))
		(mapc (lambda (entry)
			(let ((url (car entry))
			      (properties (cdr entry)))
			  (twindrill-update-icon-storage-recent-icons
			   size url properties)
			  (twindrill-register-image-spec url properties
							  size)))
		      prop-alist)))
	    data)))))

;;;;
;;;; Mode-line
;;;;

;;; SSL
(defconst twindrill-ssl-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * lock[] = {
\"16 16 3 1\",
\" 	c None\",
\".	c #000000\",
\"#	c #FFFFFF\",
\"                \",
\"                \",
\"    ........    \",
\"   ..######..   \",
\"   .##....##.   \",
\"   .#..  ..#.   \",
\"   .#.    .#.   \",
\"  ..#......#..  \",
\"  .##########.  \",
\"  .##########.  \",
\"  .####..####.  \",
\"  .###....###.  \",
\"  .###....###.  \",
\"  .##########.  \",
\"  ............  \",
\"                \"};
"
	    ))
  "Image for indicator of SSL state.")

(defconst twindrill-modeline-ssl
  (if twindrill-ssl-indicator-image
      (propertize "SSL"
		  'display twindrill-ssl-indicator-image
		  'help-echo "SSL is enabled.")
    "SSL"))

;;; ACTIVE/INACTIVE
(defconst twindrill-active-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * connected[] = {
\"16 16 3 1\",
\" 	c None\",
\".	c #000000\",
\"#	c #FFFFFF\",
\"      .##. ...  \",
\"      .##. .#.  \",
\"      .##...#.. \",
\"      .##..###..\",
\"      .##..###..\",
\"      .##.#####.\",
\"  ... .##.#.#.#.\",
\"  .#. ..#...#...\",
\"...#...#.. .#.  \",
\".#.#.#.##. .#.  \",
\".#####.##. ...  \",
\"..### .##.      \",
\" .###..##.      \",
\" ..#...##.      \",
\"  .#. .##.      \",
\"  ... .##.      \"};
"))
  "Image for indicator of active state."
)

(defconst twindrill-inactive-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * disconnected[] = {
\"16 16 3 1\",
\" 	c None\",
\".	c #000000\",
\"#	c #FFFFFF\",
\"      .##.      \",
\"      .##.      \",
\"    . .##.      \",
\"    ...##.      \",
\"     ..##.      \",
\"      ..#.      \",
\"    .. ...      \",
\"     .. ..      \",
\"      .. ..     \",
\"      ... ..    \",
\"      .#..      \",
\"      .##..     \",
\"      .##...    \",
\"      .##. .    \",
\"      .##.      \",
\"      .##.      \"};
"))
  "Image for indicator of inactive state."
)

(defconst twindrill-modeline-properties
  (when (display-mouse-p)
    `(local-map
      ,(purecopy (make-mode-line-mouse-map
		  'mouse-2 #'twindrill-toggle-activate-buffer))
      help-echo "mouse-2 toggles activate buffer")))

(defconst twindrill-modeline-active
  (if twindrill-active-indicator-image
      (apply 'propertize " "
	     `(display ,twindrill-active-indicator-image
		       ,@twindrill-modeline-properties))
    " "))

(defconst twindrill-modeline-inactive
  (if twindrill-inactive-indicator-image
      (apply 'propertize "INACTIVE"
	     `(display ,twindrill-inactive-indicator-image
		       ,@twindrill-modeline-properties))
    "INACTIVE"))

(defun twindrill-mode-line-buffer-identification ()
  (let ((active-mode-indicator
	 (if twindrill-active-mode
	     twindrill-modeline-active
	   twindrill-modeline-inactive))
	(enabled-options
	 `(,(if twindrill-display-connection-method
		(concat
		 (when twindrill-use-ssl (concat twindrill-modeline-ssl ":"))
		 (twindrill-get-connection-method-name twindrill-use-ssl))
	      (when twindrill-use-ssl twindrill-modeline-ssl))
	   ,@(when twindrill-icon-mode '("icon"))
	   ,@(when twindrill-reverse-mode '("reverse"))
	   ,@(when twindrill-proxy-use '("proxy")))))
    (concat active-mode-indicator
	    (when twindrill-display-remaining
	      (let ((spec (twindrill-current-timeline-spec)))
		(twindrill-get-ratelimit-indicator-string spec)))
	    (when enabled-options
	      (concat "[" (mapconcat 'identity enabled-options " ") "]")))))

(defun twindrill-update-mode-line ()
  "Update mode line."
  (force-mode-line-update))

;;;;
;;;; Format of a status
;;;;

(eval-and-compile
  (defsubst twindrill-make-common-properties (status)
    "Generate a property list that tweets should have irrespective of format."
    (apply 'append
	   (mapcar (lambda (entry)
		     (let ((prop-sym (if (consp entry) (car entry) entry))
			   (status-sym (if (consp entry) (cdr entry) entry)))
		       (list prop-sym (cdr (assq status-sym status)))))
		   '(id retweeted-id source-spec
			(username . user-screen-name) text)))))

(defun twindrill-get-common-properties (pos)
  "Get a common property list of the tweet rendered at POS.
The common property list is added to each rendered tweet irrespective
of format. The common properties follows:
 properites generated by `twindrill-make-common-properties',
 `field' and `rendered-as' generated by `twindrill-render-a-field' or
 `twindrill-make-properties-of-popped-ancestors'."
  (apply 'append
	 (mapcar (lambda (prop)
		   (let ((value (get-text-property pos prop)))
		     (when value
		       `(,prop ,value))))
		 '(field id rendered-as retweeted-id source-spec
			 text username))))

(defun twindrill-format-string (string prefix replacement-table)
  "Format STRING according to PREFIX and REPLACEMENT-TABLE.
PREFIX is a regexp. REPLACEMENT-TABLE is a list of (FROM . TO) pairs,
where FROM is a regexp and TO is a string or a 2-parameter function.

The pairs in REPLACEMENT-TABLE are stored in order of precedence.
First, search PREFIX in STRING from left to right.
If PREFIX is found in STRING, try to match the following string with
FROM of each pair in the same order of REPLACEMENT-TABLE. If FROM in
a pair is matched, replace the prefix and the matched string with a
string generated from TO.
If TO is a string, the matched string is replaced with TO.
If TO is a function, the matched string is replaced with the
return value of (funcall TO CONTEXT), where CONTEXT is an alist.
Each element of CONTEXT is (KEY . VALUE) and KEY is one of the
following symbols;
  'following-string  --the matched string following the prefix
  'match-data --the match-data for the regexp FROM.
  'prefix --PREFIX.
  'replacement-table --REPLACEMENT-TABLE.
  'from --FROM.
  'processed-string --the already processed string."
  (let ((current-pos 0)
	(result "")
	(case-fold-search nil))
    (while (and (string-match prefix string current-pos)
		(not (eq (match-end 0) current-pos)))
      (let ((found nil)
	    (current-table replacement-table)
	    (next-pos (match-end 0))
	    (matched-string (match-string 0 string))
	    (skipped-string
	     (substring string current-pos (match-beginning 0))))
	(setq result (concat result skipped-string))
	(setq current-pos next-pos)
	(while (and (not (null current-table))
		    (not found))
	  (let ((key (caar current-table))
		(value (cdar current-table))
		(following-string (substring string current-pos))
		(case-fold-search nil))
	    (if (string-match (concat "\\`" key) following-string)
		(let ((next-pos (+ current-pos (match-end 0)))
		      (output
		       (if (stringp value)
			   value
			 (funcall value
				  `((following-string . ,following-string)
				    (match-data . ,(match-data))
				    (prefix . ,prefix)
				    (replacement-table . ,replacement-table)
				    (from . ,key)
				    (processed-string . ,result))))))
		  (setq found t)
		  (setq current-pos next-pos)
		  (setq result (concat result output)))
	      (setq current-table (cdr current-table)))))
	(if (not found)
	    (setq result (concat result matched-string)))))
    (let ((skipped-string (substring string current-pos)))
      (concat result skipped-string))
    ))

(eval-and-compile
  (defsubst twindrill-make-string-with-user-name-property (str status)
    (if str
	(let* ((user-screen-name (cdr (assq 'user-screen-name status)))
	       (uri (twindrill-get-status-url user-screen-name))
	       (spec
		(twindrill-make-user-timeline-spec-direct user-screen-name)))
	  (propertize str
		      'mouse-face 'highlight
		      'keymap twindrill-mode-on-uri-map
		      'uri uri
		      'screen-name-in-text user-screen-name
		      'goto-spec spec
		      'face 'twindrill-username-face
		      'front-sticky nil
		      'rear-nonsticky t))
      ""))

  (defsubst twindrill-make-string-with-source-property (str status)
    (if str
	(let ((uri (cdr (assq 'source-uri status))))
	  (propertize str
		      'mouse-face 'highlight
		      'keymap twindrill-mode-on-uri-map
		      'uri uri
		      'face 'twindrill-uri-face
		      'source str
		      'front-sticky nil
		      'rear-nonsticky t))
      ""))

  (defsubst twindrill-make-string-with-uri-property (str status)
    (if str
	(let ((uri
	       (if (assq 'retweeted-id status)
		   (twindrill-get-status-url
		    (cdr (assq 'retweeted-user-screen-name status))
		    (cdr (assq 'retweeted-id status)))
		 (twindrill-get-status-url
		  (cdr (assq 'user-screen-name status))
		  (cdr (assq 'id status))))))
	  (propertize str
		      'mouse-face 'highlight
		      'keymap twindrill-mode-on-uri-map
		      'uri uri
		      'face 'twindrill-uri-face
		      'front-sticky nil
		      'rear-nonsticky t))
      "")))

(defun twindrill-make-fontified-tweet-text (str-expr regexp-hash regexp-atmark)
  (let ((regexp-str
	 (mapconcat
	  'identity
	  (list
	   ;; hashtag
	   (concat regexp-hash "\\([[:alpha:]0-9_-]+\\)")
	   ;; @USER/LIST
	   (concat regexp-atmark
		   "\\(\\([a-zA-Z0-9_-]+\\)/\\([a-zA-Z0-9_-]+\\)\\)")
	   ;; @USER
	   (concat regexp-atmark "\\([a-zA-Z0-9_-]+\\)")
	   ;; URI
	   "\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)")
	  "\\|")))
    `(let ((pos 0)
	   (str (copy-sequence ,str-expr)))
       (while (string-match ,regexp-str str pos)
	 (let* ((beg (match-beginning 0))
		(end (match-end 0))
		(range-and-properties
		 (cond
		  ((get-text-property beg 'face str)
		   ;; The matched substring has been already fontified.
		   ;; The fontification with entities must fontify the
		   ;; head of the matched string.
		   nil)
		  ((match-string 1 str)
		   ;; hashtag
		   (let* ((hashtag (match-string 1 str))
			  (spec-string
			   (twindrill-make-hashtag-timeline-spec-string-direct
			    hashtag))
			  (url (twindrill-get-search-url
				(concat "#" hashtag))))
		     (list
		      beg end
		      'mouse-face 'highlight
		      'keymap twindrill-mode-on-uri-map
		      'uri url
		      'goto-spec spec-string
		      'face 'twindrill-username-face)))
		  ((match-string 2 str)
		   ;; @USER/LIST
		   (let ((owner (match-string 3 str))
			 (list-name (match-string 4 str))
			 ;; Properties are added to the matched part only.
			 ;; The prefixes `twindrill-regexp-atmark' will not
			 ;; be highlighted.
			 (beg (match-beginning 2)))
		     (list
		      beg end
		      'mouse-face 'highlight
		      'keymap twindrill-mode-on-uri-map
		      'uri (twindrill-get-list-url owner list-name)
		      'goto-spec
		      (twindrill-make-list-timeline-spec-direct owner
								 list-name)
		      'face 'twindrill-username-face)))
		  ((match-string 5 str)
		   ;; @USER
		   (let ((screen-name (match-string 5 str))
			 ;; Properties are added to the matched part only.
			 ;; The prefixes `twindrill-regexp-atmark' will not
			 ;; be highlighted.
			 (beg (match-beginning 5)))
		     (list
		      beg end
		      'mouse-face 'highlight
		      'keymap twindrill-mode-on-uri-map
		      'uri (twindrill-get-status-url screen-name)
		      'screen-name-in-text screen-name
		      'goto-spec
		      (twindrill-make-user-timeline-spec-direct screen-name)
		      'face 'twindrill-uri-face)))
		  ((match-string 6 str)
		   ;; URI
		   (let ((uri (match-string 6 str)))
		     (list
		      beg end
		      'mouse-face 'highlight
		      'keymap twindrill-mode-on-uri-map
		      'uri uri
		      'uri-origin 'explicit-uri-in-tweet
		      'face 'twindrill-uri-face)))))
		(beg (if range-and-properties
			 (car range-and-properties)
		       beg))
		(end (if range-and-properties
			 (cadr range-and-properties)
		       end))
		(properties
		 `(,@(cddr range-and-properties)
		   front-sticky nil
		   rear-nonsticky t)))
	   (when range-and-properties
	     (add-text-properties beg end properties str))
	   (setq pos end)))
       str)))

(eval-and-compile
  (defsubst twindrill-make-fontified-tweet-text-with-entity (status)
    (let* ((text (copy-sequence (cdr (assq 'text status))))
	   (text-length (length text))
	   (entities (cdr (assq 'entity status))))
      ;; hashtags
      (mapc (lambda (hashtag)
	      (let* ((start (cdr (assq 'start hashtag)))
		     (end (min (cdr (assq 'end hashtag)) text-length))
		     (tag (cdr (assq 'text hashtag)))
		     (spec-string
		      (twindrill-make-hashtag-timeline-spec-string-direct tag)))
		(set-text-properties
		 start end
		 `(mouse-face
		   highlight
		   keymap ,twindrill-mode-on-uri-map
		   uri ,(twindrill-get-search-url (concat "#" tag))
		   goto-spec ,spec-string
		   face twindrill-username-face
		   front-sticky nil
		   rear-nonsticky t)
		 text)))
	    (cdr (assq 'hashtags entities)))
      ;; mentions
      (mapc (lambda (mention)
	      (let ((start (cdr (assq 'start mention)))
		    (end (min (cdr (assq 'end mention)) text-length))
		    (screen-name (cdr (assq 'screen-name mention))))
		(set-text-properties
		 start end
		 `(mouse-face
		   highlight
		   keymap ,twindrill-mode-on-uri-map
		   uri ,(twindrill-get-status-url screen-name)
		   screen-name-in-text ,screen-name
		   goto-spec
		   ,(twindrill-make-user-timeline-spec-direct screen-name)
		   face twindrill-uri-face
		   front-sticky nil
		   rear-nonsticky t)
		 text)))
	    (cdr (assq 'mentions entities)))
      ;; urls
      (let ((offset 0))
	(mapc (lambda (url-info)
		(let* ((text-length (length text))
		       (start (cdr (assq 'start url-info)))
		       (end (cdr (assq 'end url-info)))
		       (url (cdr (assq 'url url-info)))
		       (expanded-url
			;; If the `url' is short and not wrapped,
			;; `expanded-url' is nil.
			(or (cdr (assq 'expanded-url url-info))
			    url))
		       (replacement
			(propertize
			 expanded-url
			 'mouse-face 'highlight
			 'keymap twindrill-mode-on-uri-map
			 'uri url
			 'uri-origin 'explicit-uri-in-tweet
			 'expanded-uri expanded-url
			 'face 'twindrill-uri-face
			 'front-sticky nil
			 'rear-nonsticky t)))
		  (setq text
			(concat
			 (substring text 0 (min (+ offset start) text-length))
			 replacement
			 (substring text (min (+ offset end) text-length))))
		  (setq offset
			(+ offset (- (length expanded-url) (- end start))))))
	      (cdr (assq 'urls entities))))
      text)))

(defun twindrill-generate-format-table (status-sym prefix-sym)
  `(("%" . "%")
    ("}" . "}")
    ("#" . (cdr (assq 'id ,status-sym)))
    ("'" . (when (cdr (assq 'truncated ,status-sym))
	     "..."))
    ("c" .
     (let ((system-time-locale "C"))
       (format-time-string "%a %b %d %H:%M:%S %z %Y"
			   (cdr (assq 'created-at ,status-sym)))))
    ("d" . (cdr (assq 'user-description ,status-sym)))
    ("f" .
     (twindrill-make-string-with-source-property
      (cdr (assq 'source ,status-sym)) ,status-sym))
    ("i" .
     (when (and twindrill-icon-mode window-system)
       (let ((url
	      (cond
	       ((and twindrill-use-profile-image-api
		     (eq twindrill-service-method 'twitter)
		     (or (null twindrill-convert-fix-size)
			 (member twindrill-convert-fix-size '(48 73))))
		(let ((user (cdr (assq 'user-screen-name ,status-sym)))
		      (size
		       (if (or (null twindrill-convert-fix-size)
			       (= 48 twindrill-convert-fix-size))
			   "normal"
			 "bigger")))
		  (format "http://%s/%s/%s.xml?size=%s" twindrill-api-host
			  (twindrill-api-path "users/profile_image") user size)))
	       (t
		(cdr (assq 'user-profile-image-url ,status-sym))))))
	 (twindrill-make-icon-string nil nil url))))
    ("j" . (cdr (assq 'user-id ,status-sym)))
    ("L" .
     (let ((location (or (cdr (assq 'user-location ,status-sym)) "")))
       (unless (string= "" location)
	 (concat " [" location "]"))))
    ("l" . (cdr (assq 'user-location ,status-sym)))
    ("p" . (when (cdr (assq 'user-protected ,status-sym))
	     "[x]"))
    ("r" .
     (let ((reply-id (or (cdr (assq 'in-reply-to-status-id ,status-sym)) ""))
	   (reply-name (or (cdr (assq 'in-reply-to-screen-name ,status-sym))
			   ""))
	   (recipient-screen-name
	    (cdr (assq 'recipient-screen-name ,status-sym))))
       (let* ((pair
	       (cond
		(recipient-screen-name
		 (cons (format "sent to %s" recipient-screen-name)
		       (twindrill-get-status-url recipient-screen-name)))
		((and (not (string= "" reply-id))
		      (not (string= "" reply-name)))
		 (cons (format "in reply to %s" reply-name)
		       (twindrill-get-status-url reply-name reply-id)))
		(t nil)))
	      (str (car pair))
	      (url (cdr pair))
	      (properties
	       (list 'mouse-face 'highlight 'face 'twindrill-uri-face
		     'keymap twindrill-mode-on-uri-map
		     'uri url
		     'front-sticky nil
		     'rear-nonsticky t)))
	 (when (and str url)
	   (concat " " (apply 'propertize str properties))))))
    ("R" .
     (let ((retweeted-by
	    (or (cdr (assq 'retweeting-user-screen-name ,status-sym)) "")))
       (unless (string= "" retweeted-by)
	 (concat " (retweeted by " retweeted-by ")"))))
    ("S" .
     (twindrill-make-string-with-user-name-property
      (cdr (assq 'user-name ,status-sym)) ,status-sym))
    ("s" .
     (twindrill-make-string-with-user-name-property
      (cdr (assq 'user-screen-name ,status-sym)) ,status-sym))
    ("T" .
     ,(twindrill-make-fontified-tweet-text
       `(twindrill-make-fontified-tweet-text-with-entity ,status-sym)
       twindrill-regexp-hash twindrill-regexp-atmark))
    ("t" .
     ,(twindrill-make-fontified-tweet-text
       `(twindrill-make-fontified-tweet-text-with-entity ,status-sym)
       twindrill-regexp-hash twindrill-regexp-atmark))
    ("u" . (cdr (assq 'user-url ,status-sym)))))

(defun twindrill-generate-formater-for-first-spec (format-str status-sym prefix-sym)
  (cond
   ((string-match "\\`}" format-str)
    ;; "}" at the first means the end of the current level.
    `(nil . ,(substring format-str (match-end 0))))
   ((string-match "\\`%" format-str)
    (let* ((following (substring format-str 1))
	   (table (twindrill-generate-format-table status-sym prefix-sym))
	   (regexp (concat "\\`\\(" (mapconcat 'car table "\\|") "\\)"))
	   (case-fold-search nil))
      (cond
       ((string-match "\\`@\\({\\([^}]*\\)}\\)?" following)
	(let ((time-format (or (match-string 2 following)
			       "%I:%M %p %B %d, %Y"))
	      (rest (substring following (match-end 0))))
	  `((let* ((created-at (cdr (assq 'created-at ,status-sym)))
		   (url
		    (if (assq 'retweeted-id ,status-sym)
			(twindrill-get-status-url
			 (cdr (assq 'retweeted-user-screen-name ,status-sym))
			 (cdr (assq 'retweeted-id ,status-sym)))
		      (twindrill-get-status-url
		       (cdr (assq 'user-screen-name ,status-sym))
		       (cdr (assq 'id ,status-sym)))))
		   (properties
		    (list 'mouse-face 'highlight 'face 'twindrill-uri-face
			  'keymap twindrill-mode-on-uri-map
			  'uri url
			  'front-sticky nil
			  'rear-nonsticky t)))
	      (twindrill-make-passed-time-string
	       nil nil created-at ,time-format properties))
	    . ,rest)))
       ((string-match "\\`C\\({\\([^}]*\\)}\\)?" following)
	(let ((time-format (or (match-string 2 following) "%H:%M:%S"))
	      (rest (substring following (match-end 0))))
	  `((let* ((created-at (cdr (assq 'created-at ,status-sym))))
	      (twindrill-make-string-with-uri-property
	       (format-time-string ,time-format created-at) ,status-sym))
	    . ,rest)))
       ((string-match "\\`FACE\\[\\([a-zA-Z0-9:-]+\\)\\]{" following)
	(let* ((face-name-str (match-string 1 following))
	       (str-after-brace (substring following (match-end 0)))
	       (face-sym (intern face-name-str))
	       (pair (twindrill-generate-formater-for-current-level
		      str-after-brace status-sym prefix-sym))
	       (braced-body (car pair))
	       (rest (cdr pair)))
	  `((propertize (concat ,@braced-body) 'face ',face-sym)
	    . ,rest)))
       ((string-match "\\`FIELD\\(\\[\\([^]]*\\)\\]\\)?{\\([a-z_]*\\)}"
		      following)
	(let* ((format-str (or (match-string 2 following) "%s"))
	       (field-raw-name (match-string 3 following))
	       (field-name (replace-regexp-in-string "_" "-" field-raw-name))
	       (field-symbol (intern field-name))
	       (rest (substring following (match-end 0))))
	  `((let* ((field-value (cdr (assq ',field-symbol ,status-sym))))
	      (if field-value
		  (format ,format-str field-value)
		""))
	    . ,rest)))
       ((string-match "\\`FIELD-IF-NONZERO\\(\\[\\([^]]*\\)\\]\\)?{\\([a-z_]*\\)}"
		      following)
	(let* ((format-str (or (match-string 2 following) "%s"))
	       (field-raw-name (match-string 3 following))
	       (field-name (replace-regexp-in-string "_" "-" field-raw-name))
	       (field-symbol (intern field-name))
	       (rest (substring following (match-end 0))))
	  `((let* ((field-value (cdr (assq ',field-symbol ,status-sym))))
	      (if (and (integerp field-value)
		       (not (zerop field-value)))
		  (format ,format-str field-value)
		""))
	    . ,rest)))
       ((string-match "\\`\\(FILL\\|FOLD\\)\\(\\[\\([^]]*\\)\\]\\)?{"
		      following)
	(let* ((str-after-brace (substring following (match-end 0)))
	       (specifier (match-string 1 following))
	       (prefix-str (match-string 3 following))
	       (pair (twindrill-generate-formater-for-current-level
		      str-after-brace status-sym prefix-sym))
	       (filled-body (car pair))
	       (formater
		`(lambda (,status-sym ,prefix-sym)
		   (let ((,prefix-sym (concat ,prefix-sym ,prefix-str)))
		     (concat ,@filled-body))))
	       (keep-newline (string= "FOLD" specifier))
	       (rest (cdr pair)))
	  `((twindrill-update-filled-string
	     nil nil ,formater ,status-sym ,prefix-sym ,prefix-str
	     ,keep-newline)
	    . ,rest)))
       ((string-match "\\`RT{" following)
	(let* ((str-after-brace (substring following (match-end 0)))
	       (pair (twindrill-generate-formater-for-current-level
		      str-after-brace 'retweeting prefix-sym))
	       (braced-body (car pair))
	       (rest (cdr pair)))
	  `((when (assq 'retweeted-id ,status-sym)
	      (let ((retweeting
		     (mapcar (lambda (entry)
			       (let ((key-str (symbol-name (car entry)))
				     (value (cdr entry)))
				 (when (string-match "\\`retweeting-" key-str)
				   (let ((new-key
					  (intern (substring key-str
							     (match-end 0)))))
				     (cons new-key value)))))
			     ,status-sym)))
		(concat ,@braced-body)))
	    . ,rest)))
       ((string-match regexp following)
	(let ((specifier (match-string 1 following))
	      (rest (substring following (match-end 0))))
	  `(,(cdr (assoc specifier table)) . ,rest)))
       (t
	`("%" . ,following)))))
   ((string-match "\\(%\\|}\\)" format-str)
    (let* ((sep (match-beginning 0))
	   (first (substring format-str 0 sep))
	   (last (substring format-str sep)))
      ;; Split before "%" or "}".
      `(,first . ,last)))
   (t
    `(,format-str . nil))))

(defun twindrill-generate-formater-for-current-level (format-str status-sym prefix-sym)
  (let ((result nil)
	(rest format-str)
	(continue t))
    (while (and continue rest)
      (let* ((pair
	      (twindrill-generate-formater-for-first-spec
	       rest status-sym prefix-sym))
	     (current-result (car pair)))
	(if current-result
	    (setq result (append result `(,current-result)))
	  ;; If `result' is nil, it means the end of the current level.
	  (setq continue nil))
	(setq rest (cdr pair))))
    `(,result . ,rest)))

(defun twindrill-generate-format-status-function (format-str)
  (let* ((status-sym 'status)
	 (prefix-sym 'prefix)
	 (pair
	  (twindrill-generate-formater-for-current-level
	   format-str status-sym prefix-sym))
	 (body (car pair))
	 (rest (cdr pair)))
    (cond
     ((null rest)
      `(lambda (status prefix)
	 (let* ((common-properties (twindrill-make-common-properties status))
		(str (concat ,@body))
		(str (if prefix
			 (replace-regexp-in-string "^" prefix str)
		       str))
		(next (next-single-property-change 0 'need-to-be-updated str))
		(need-to-be-updated
		 (or (get-text-property 0 'need-to-be-updated str)
		     (and next (< next (length str))))))
	   (add-text-properties 0 (length str) common-properties str)
	   (when (and prefix need-to-be-updated)
	     ;; With a prefix, redisplay the total status instead of
	     ;; redisplaying partially.
	     (remove-text-properties 0 (length str)
				     '(need-to-be-updated nil) str)
	     (put-text-property 0 (length str) 'need-to-be-updated
				`(twindrill-format-status-for-redisplay
				  ,status ,prefix)
				str))
	   str)))
     (t
      (message "Failed to generate a status formater for `twindrill-mode'.")
      nil))))

(defun twindrill-update-status-format (&optional format-str)
  "Update the format for rendering a tweet.
If FORMAT-STR is nil, `twindrill-status-format' is used in place of
FORMAT-STR.

If FORMAT-STR is valid as a format, `twindrill-format-status-function'
is replaced by the result of `twindrill-generate-format-status-function'
for FORMAT-STR.
If FORMAT-STR is invalid as a format, an error is signaled and
`twindrill-format-status-function' is not updated."
  (let ((format-str (or format-str twindrill-status-format)))
    (unless (string= format-str twindrill-format-status-function-source)
      (let* ((before (get-buffer "*Compile-Log*"))
	     (func (twindrill-generate-format-status-function format-str)))
	(cond
	 ((and func (functionp func))
	  (setq twindrill-format-status-function-source format-str)
	  (setq twindrill-format-status-function (byte-compile func))
	  (setq twindrill-format-status-function-without-compile func)
	  (setq twindrill-status-format format-str)
	  (let ((current (get-buffer "*Compile-Log*")))
	    (when (and (null before) current (= 0 (buffer-size current)))
	      (kill-buffer current))))
	 (t
	  (error "Invalid format: %s" format-str)
	  nil))))))

(defun twindrill-format-status (status &optional prefix)
  "Format a STATUS by using `twindrill-format-status-function'.
PREFIX is the prefix that will be added to the result of this function.
PREFIX is used in order to calculate appropriate width for filling texts.
Specification of the format is described in the document for the
variable `twindrill-status-format'."
  (funcall twindrill-format-status-function status prefix))

(defun twindrill-format-status-for-redisplay (beg end status &optional prefix)
  (twindrill-format-status status prefix))

;;;;
;;;; Rendering
;;;;

(defun twindrill-field-id< (field1 field2)
  (string< field1 field2))

(defun twindrill-field-id= (field1 field2)
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
Insert a field to the position pointed by FIELD-ID. The position is searched
after POS. The string for the field is generated by the GENERATOR expression.
This function does not render the status if a status with the same field ID
as FIELD-ID is already rendered.
Return non-nil if the status is rendered. Otherwise, return nil."
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
  (interactive)
  (if (twindrill-replied-statuses-visible-p)
      (twindrill-hide-replied-statuses (interactive-p))
    (twindrill-show-replied-statuses twindrill-show-replied-tweets
				      (interactive-p))))

;;;;
;;;; Unread statuses info
;;;;

(defvar twindrill-unread-status-info nil
  "A list of (buffer unread-statuses-counter), where `unread-statuses-counter'
means the number of statuses retrieved after the last visiting of the buffer.")

(defun twindrill-reset-unread-status-info-if-necessary ()
  (when (twindrill-buffer-p)
    (twindrill-set-number-of-unread (current-buffer) 0)))

(defun twindrill-set-number-of-unread (buffer number)
  (let* ((entry (assq buffer twindrill-unread-status-info))
	 (current (or (cadr entry) 0)))
    (unless (= number current)
      (setq twindrill-unread-status-info
	    (cons
	     `(,buffer ,number)
	     (if entry
		 (remq entry twindrill-unread-status-info)
	       twindrill-unread-status-info))))))

(defun twindrill-make-unread-status-notifier-string ()
  "Generate a string that displays unread statuses."
  (setq twindrill-unread-status-info
	(remove nil
		(mapcar (lambda (entry)
			  (when (buffer-live-p (car entry))
			    entry))
			twindrill-unread-status-info)))
  (let ((sum (apply '+ (mapcar 'cadr twindrill-unread-status-info))))
    (if (= 0 sum)
	""
      (format "tw(%d)" sum))))

(defun twindrill-update-unread-status-info ()
  "Update `twindrill-unread-status-info' with new tweets."
  (let* ((buffer (twindrill-get-buffer-from-spec
		  twindrill-rendered-new-tweets-spec))
	 (current (or (cadr (assq buffer twindrill-unread-status-info)) 0))
	 (result (+ current (length twindrill-rendered-new-tweets))))
    (when buffer
      (twindrill-set-number-of-unread buffer result))))

(defun twindrill-enable-unread-status-notifier ()
  "Enable a notifier of unread statuses on `twindrill-mode'."
  (interactive)
  (setq twindrill-unread-status-info
	(mapcar (lambda (buffer) `(,buffer ,0))
		(twindrill-get-buffer-list)))
  (add-hook 'twindrill-new-tweets-rendered-hook
	    'twindrill-update-unread-status-info)
  (add-hook 'post-command-hook
	    'twindrill-reset-unread-status-info-if-necessary)
  (add-to-list 'global-mode-string
	       '(:eval (twindrill-make-unread-status-notifier-string))
	       t))

(defun twindrill-disable-unread-status-notifier ()
  "Disable a notifier of unread statuses on `twindrill-mode'."
  (interactive)
  (setq twindrill-unread-status-info nil)
  (remove-hook 'twindrill-new-tweets-hook
	       'twindrill-update-unread-status-info)
  (remove-hook 'post-command-hook
	       'twindrill-reset-unread-status-info-if-necessary)
  (setq global-mode-string
	(remove '(:eval (twindrill-make-unread-status-notifier-string))
		global-mode-string)))

;;;;
;;;; Timer
;;;;

(defvar twindrill-idle-timer-for-redisplay nil)

(defun twindrill-timer-action (func)
  (let ((buf (twindrill-get-active-buffer-list)))
    (if (null buf)
	(twindrill-stop)
      (funcall func)
      )))

(defun twindrill-run-on-idle (idle-interval func &rest args)
  "Run FUNC the next time Emacs is idle for IDLE-INTERVAL.
Even if Emacs has been idle longer than IDLE-INTERVAL, run FUNC immediately.
Since immediate invocation requires `current-idle-time', it is available
on Emacs 22 and later.
FUNC is called as (apply FUNC ARGS)."
  (let ((sufficiently-idling
	 (and (fboundp 'current-idle-time)
	      (current-idle-time)
	      (time-less-p (seconds-to-time idle-interval)
			   (current-idle-time)))))
    (if (not sufficiently-idling)
	(apply 'run-with-idle-timer idle-interval nil func args)
      (apply func args)
      nil)))

(defun twindrill-run-repeatedly-on-idle (check-interval var idle-interval func &rest args)
  "Run FUNC every time Emacs is idle for IDLE-INTERVAL.
Even if Emacs remains idle longer than IDLE-INTERVAL, run FUNC every
CHECK-INTERVAL seconds. Since this behavior requires `current-idle-time',
invocation on long idle time is available on Emacs 22 and later.
VAR is a symbol of a variable to which the idle-timer is bound.
FUNC is called as (apply FUNC ARGS)."
  (apply 'run-at-time "0 sec"
	 check-interval
	 (lambda (var idle-interval func &rest args)
	   (let ((registerd (symbol-value var))
		 (sufficiently-idling
		  (and (fboundp 'current-idle-time)
		       (current-idle-time)
		       (time-less-p (seconds-to-time idle-interval)
				    (current-idle-time)))))
	     (when (or (not registerd) sufficiently-idling)
	       (when (and registerd sufficiently-idling)
		 (cancel-timer (symbol-value var))
		 (apply func args))
	       (set var (apply 'run-with-idle-timer idle-interval nil
			       (lambda (var func &rest args)
				 (set var nil)
				 (apply func args))
			       var func args)))))
	 var idle-interval func args))

(defun twindrill-start (&optional action)
  (interactive)
  (unless twindrill-timer
    (let ((action (or action #'twindrill-update-active-buffers)))
      ;; Update all active timelines forcibly.
      (twindrill-update-active-buffers t)
      (setq twindrill-timer
	    (run-at-time (format "%d sec" twindrill-timer-interval)
			 twindrill-timer-interval
			 #'twindrill-timer-action action))))
  (unless twindrill-timer-for-redisplaying
    (setq twindrill-timer-for-redisplaying
	  (twindrill-run-repeatedly-on-idle
	   (* 2 twindrill-timer-interval-for-redisplaying)
	   'twindrill-idle-timer-for-redisplay
	   twindrill-timer-interval-for-redisplaying
	   #'twindrill-redisplay-status-on-buffer))))

(defun twindrill-stop ()
  (interactive)
  (when twindrill-timer
    (cancel-timer twindrill-timer)
    (setq twindrill-timer nil))
  (when twindrill-timer-for-redisplaying
    (when twindrill-idle-timer-for-redisplay
      (cancel-timer twindrill-idle-timer-for-redisplay)
      (setq twindrill-idle-timer-for-redisplay))
    (cancel-timer twindrill-timer-for-redisplaying)
    (setq twindrill-timer-for-redisplaying nil)))

(defun twindrill-get-relative-interval (spec)
  (let* ((spec-string (twindrill-timeline-spec-to-string spec))
	 (normalized-alist
	  (apply 'append
		 (mapcar
		  (lambda (entry)
		    (let ((interval (car (last entry)))
			  (regexp-list (butlast entry 1)))
		      (when (integerp interval)
			(mapcar (lambda (regexp) `(,regexp . ,interval))
				regexp-list))))
		  twindrill-relative-retrieval-interval-alist)))
	 (rest normalized-alist)
	 (current normalized-alist)
	 (result 0))
    (while (not
	    (or (and (stringp (car current))
		     (string-match (car current) spec-string))
		(eq t (car current))))
      (setq current (car rest))
      (setq rest (cdr rest)))
    (if (integerp (cdr current))
	(cdr current)
      ;; The default relative interval is 1.
      1)))

(defun twindrill-get-retrieval-count (spec)
  (cdr (assoc spec twindrill-relative-retrieval-count-alist)))

(defun twindrill-set-retrieval-count (spec count)
  (let ((current (assoc spec twindrill-relative-retrieval-count-alist)))
    (if (null current)
	(add-to-list 'twindrill-relative-retrieval-count-alist
		     `(,spec . ,count))
      (setcdr current count))))

(defun twindrill-initialize-retrieval-count (spec)
  (twindrill-set-retrieval-count spec
				  (twindrill-get-relative-interval spec)))

(defun twindrill-update-active-buffers (&optional force noninteractive)
  "Update active buffers managed by `twindrill-mode' at a certain interval.

If FORCE is nil, each active buffer is updated at a relative interval
determined by `twindrill-relative-retrieval-interval-alist'.
If a relative interval of a timeline is 3, the timeline is updated once
by three invocations of this function.

If FORCE is non-nil, all active buffers are updated forcibly."
  (when (twindrill-account-authorized-p)
    (twindrill-update-service-configuration)
    (let* ((buffer-list (twindrill-get-active-buffer-list))
	   (primary-spec-list
	    (twindrill-remove-duplicates
	     (apply 'append
		    (mapcar
		     (lambda (buffer)
		       (twindrill-get-primary-base-timeline-specs
			(twindrill-get-timeline-spec-for-buffer buffer)))
		     buffer-list)))))
      (mapc
       (lambda (spec)
	 (let ((current
		(if force
		    1
		  (twindrill-get-retrieval-count spec))))
	   (cond
	    ((null current)
	     ;; Initialize the count if no entry for the primary timeline
	     ;; exists.
	     (twindrill-initialize-retrieval-count spec))
	    ((and (integerp current) (= 0 current))
	     ;; Do nothing.
	     )
	    ((and (integerp current) (= 1 current))
	     ;; Retrieve the timeline and initialize count.
	     (let ((spec-string
		    (twindrill-timeline-spec-to-string spec)))
	       (twindrill-get-and-render-timeline
		noninteractive nil spec spec-string)
	       (twindrill-initialize-retrieval-count spec)))
	    ((and (integerp current) (< 1 current))
	     ;; Decrement count.
	     (twindrill-set-retrieval-count spec (1- current)))
	    (t
	     nil))))
       primary-spec-list))))

;;;;
;;;; Keymap
;;;;

(if twindrill-mode-map
    (let ((km twindrill-mode-map))
      (define-key km (kbd "C-c C-f") 'twindrill-friends-timeline)
      (define-key km (kbd "C-c C-r") 'twindrill-replies-timeline)
      (define-key km (kbd "C-c C-u") 'twindrill-user-timeline)
      (define-key km (kbd "C-c C-d") 'twindrill-direct-messages-timeline)
      (define-key km (kbd "C-c C-s") 'twindrill-update-status-interactive)
      (define-key km (kbd "C-c C-e") 'twindrill-erase-old-statuses)
      (define-key km (kbd "C-c C-m") 'twindrill-retweet)
      (define-key km (kbd "C-c C-t") 'twindrill-set-current-hashtag)
      (define-key km (kbd "C-m") 'twindrill-enter)
      (define-key km (kbd "C-c C-l") 'twindrill-update-lambda)
      (define-key km (kbd "<mouse-1>") 'twindrill-click)
      (define-key km (kbd "C-<down-mouse-3>") 'mouse-set-point)
      (define-key km (kbd "C-<mouse-3>") 'twindrill-push-tweet-onto-kill-ring)
      (define-key km (kbd "C-c C-v") 'twindrill-view-user-page)
      (define-key km (kbd "C-c D") 'twindrill-delete-status)
      (define-key km (kbd "C-c C-w") 'twindrill-delete-status)
      (define-key km (kbd "a") 'twindrill-toggle-activate-buffer)
      (define-key km (kbd "g") 'twindrill-current-timeline)
      (define-key km (kbd "u") 'twindrill-update-status-interactive)
      (define-key km (kbd "U") 'twindrill-push-uri-onto-kill-ring)
      (define-key km (kbd "d") 'twindrill-direct-message)
      (define-key km (kbd "v") 'twindrill-other-user-timeline)
      (define-key km (kbd "V") 'twindrill-visit-timeline)
      (define-key km (kbd "L") 'twindrill-other-user-list-interactive)
      (define-key km (kbd "f") 'twindrill-switch-to-next-timeline)
      (define-key km (kbd "b") 'twindrill-switch-to-previous-timeline)
      ;; (define-key km (kbd "j") 'next-line)
      ;; (define-key km (kbd "k") 'previous-line)
      (define-key km (kbd "j") 'twindrill-goto-next-status)
      (define-key km (kbd "k") 'twindrill-goto-previous-status)
      (define-key km (kbd "l") 'forward-char)
      (define-key km (kbd "h") 'backward-char)
      (define-key km (kbd "0") 'beginning-of-line)
      (define-key km (kbd "^") 'beginning-of-line-text)
      (define-key km (kbd "$") 'end-of-line)
      (define-key km (kbd "n") 'twindrill-goto-next-status-of-user)
      (define-key km (kbd "p") 'twindrill-goto-previous-status-of-user)
      (define-key km (kbd "C-i") 'twindrill-goto-next-thing)
      (define-key km (kbd "M-C-i") 'twindrill-goto-previous-thing)
      (define-key km (kbd "<backtab>") 'twindrill-goto-previous-thing)
      (define-key km (kbd "<backspace>") 'twindrill-scroll-down)
      (define-key km (kbd "M-v") 'twindrill-scroll-down)
      (define-key km (kbd "SPC") 'twindrill-scroll-up)
      (define-key km (kbd "C-v") 'twindrill-scroll-up)
      (define-key km (kbd "G") 'twindrill-goto-last-status)
      (define-key km (kbd "H") 'twindrill-goto-first-status)
      (define-key km (kbd "i") 'twindrill-icon-mode)
      (define-key km (kbd "r") 'twindrill-toggle-show-replied-statuses)
      (define-key km (kbd "R") 'twindrill-toggle-or-retrieve-replied-statuses)
      (define-key km (kbd "t") 'twindrill-toggle-proxy)
      (define-key km (kbd "C-c C-p") 'twindrill-toggle-proxy)
      (define-key km (kbd "q") 'twindrill-kill-buffer)
      (define-key km (kbd "C-c C-q") 'twindrill-search)
      nil))

(let ((km twindrill-mode-menu-on-uri-map))
  (when km
    (define-key km [ct] '("Copy tweet" . twindrill-push-tweet-onto-kill-ring))
    (define-key km [cl] '("Copy link" . twindrill-push-uri-onto-kill-ring))
    (define-key km [ll] '("Load link" . twindrill-click))
    (let ((km-on-uri twindrill-mode-on-uri-map))
      (when km-on-uri
	(define-key km-on-uri (kbd "C-<down-mouse-3>") 'mouse-set-point)
	(define-key km-on-uri (kbd "C-<mouse-3>") km)))))

(defun twindrill-keybind-message ()
  (let ((important-commands
	 '(("Timeline" . twindrill-friends-timeline)
	   ("Replies" . twindrill-replies-timeline)
	   ("Update status" . twindrill-update-status-interactive)
	   ("Next" . twindrill-goto-next-status)
	   ("Prev" . twindrill-goto-previous-status))))
    (mapconcat (lambda (command-spec)
		 (let ((descr (car command-spec))
		       (command (cdr command-spec)))
		   (format "%s: %s" descr (key-description
					   (where-is-internal
					    command
					    overriding-local-map t)))))
	       important-commands ", ")))

;; (run-with-idle-timer
;;  0.1 t
;;  '(lambda ()
;;     (when (equal (buffer-name (current-buffer)) twindrill-buffer)
;;       (message (twindrill-keybind-message)))))


;;;;
;;;; Initialization
;;;;

(defvar twindrill-initialized nil)
(defvar twindrill-mode-syntax-table nil "")

(unless twindrill-mode-syntax-table
  (setq twindrill-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" twindrill-mode-syntax-table)
  (modify-syntax-entry ?\" "w" twindrill-mode-syntax-table)
  )

(defun twindrill-initialize-global-variables-if-necessary ()
  "Initialize global variables for `twindrill-mode' if they have not
been initialized yet."
  (unless twindrill-initialized
    (defface twindrill-username-face
      `((t ,(append '(:underline t)
		    (face-attr-construct
		     (if (facep 'font-lock-string-face)
			 'font-lock-string-face
		       'bold)))))
      "" :group 'faces)
    (defface twindrill-uri-face `((t (:underline t))) "" :group 'faces)
    (defface twindrill-timeline-header-face
      `((t ,(face-attr-construct
	     (if (facep 'font-lock-preprocessor-face)
		 'font-lock-preprocessor-face
	       'bold))))
      "Timeline header on twindrill-mode" :group 'faces)
    (defface twindrill-timeline-footer-face
      `((t ,(face-attr-construct
	     (if (facep 'font-lock-preprocessor-face)
		 'font-lock-preprocessor-face
	       'bold))))
      "Timeline footer on twindrill-mode" :group 'faces)
    (twindrill-update-status-format)
    (when twindrill-use-convert
      (if (null twindrill-convert-program)
	  (setq twindrill-use-convert nil)
	(with-temp-buffer
	  (let ((coding-system-for-read 'iso-safe)
		(coding-system-for-write 'iso-safe)
		;; Bind `default-directory' to the temporary directory
		;; because it is possible that the directory pointed by
		;; `default-directory' has been already removed.
		(default-directory temporary-file-directory))
	    (call-process twindrill-convert-program nil (current-buffer) nil
			  "-version")
	    (goto-char (point-min))
	    (if (null (search-forward-regexp "\\(Image\\|Graphics\\)Magick"
					     nil t))
		(setq twindrill-use-convert nil))))))
    (twindrill-setup-proxy)
    (when twindrill-use-icon-storage
      (cond
       ((require 'jka-compr nil t)
	(twindrill-load-icon-properties)
	(add-hook 'kill-emacs-hook 'twindrill-save-icon-properties))
       (t
	(setq twindrill-use-icon-storage nil)
	(error "Disabled icon-storage because it failed to load jka-compr."))))
    (cond
     ((and
       (boundp 'twindrill-sign-simple-string)
       twindrill-sign-simple-string
       (or (not (boundp 'twindrill-sign-string-function))
	   (null twindrill-sign-string-function))
       (eq twindrill-edit-skeleton 'none)
       (or (null twindrill-edit-skeleton-footer)
	   (string= twindrill-edit-skeleton-footer "")))
      ;; Configure `twindrill-edit-skeleton' as an alternative of
      ;; `twindrill-sign-simple-string'.
      (twindrill-edit-skeleton-change-footer
       (format " [%s]" twindrill-sign-simple-string))
      (setq twindrill-edit-skeleton 'footer)
      (message "Warning: `twindrill-sign-simple-string' is obsolete. Use `twindrill-edit-skeleton-footer' instead."))
     ((or (boundp 'twindrill-sign-simple-string)
	  (boundp 'twindrill-sign-string-function))
      (message "Warning: `twindrill-sign-simple-string' and `twindrill-sign-string-function' are obsolete. Use the new feature `twindrill-edit-skeleton'.")
      ))
    (run-hooks 'twindrill-mode-init-hook)
    (setq twindrill-initialized t)))

(defun twindrill-mode-setup (spec-string)
  "Set up the current buffer for `twindrill-mode'."
  (kill-all-local-variables)
  (setq major-mode 'twindrill-mode)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq mode-name "twindrill-mode")
  (setq mode-line-buffer-identification
	`(,(default-value 'mode-line-buffer-identification)
	  (:eval (twindrill-mode-line-buffer-identification))))

  ;; Prevent `global-font-lock-mode' enabling `font-lock-mode'.
  ;; This technique is derived from `lisp/bs.el' distributed with Emacs 22.2.
  (make-local-variable 'font-lock-global-modes)
  (setq font-lock-global-modes '(not twindrill-mode))

  ;; Prevent the field property attached to tweets from interfering
  ;; the cursor motion based on logical lines.
  (make-local-variable 'inhibit-field-text-motion)
  (setq inhibit-field-text-motion t)

  (make-local-variable 'twindrill-timeline-spec)
  (make-local-variable 'twindrill-timeline-spec-string)
  (make-local-variable 'twindrill-active-mode)
  (make-local-variable 'twindrill-icon-mode)
  (make-local-variable 'twindrill-reverse-mode)

  (setq twindrill-timeline-spec-string spec-string)
  (setq twindrill-timeline-spec
	(twindrill-string-to-timeline-spec spec-string))
  (setq twindrill-active-mode t)

  (use-local-map twindrill-mode-map)
  (twindrill-update-mode-line)
  (set-syntax-table twindrill-mode-syntax-table)
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1))
  (add-to-list 'twindrill-buffer-info-list (current-buffer) t)
  (run-hooks 'twindrill-mode-hook))

(defun twindrill-mode ()
  "Major mode for Twitter
\\{twindrill-mode-map}"
  (interactive)
  (let ((timeline-spec-list
	 (if (listp twindrill-initial-timeline-spec-string)
	     twindrill-initial-timeline-spec-string
	   (cons twindrill-initial-timeline-spec-string nil))))
    (twindrill-visit-timeline (car timeline-spec-list))
    (when (twindrill-account-authorized-p)
      (mapc 'twindrill-visit-timeline (cdr timeline-spec-list)))))

;;;;
;;;; Preparation for invoking APIs
;;;;

(defun twindrill-api-invocation-is-ready-p ()
  "Return non-nil if the preparation for invoking APIs has been completed."
  (and
   ;; The global variables are initialized.
   twindrill-initialized
   ;; A connection method is prepared.
   (let ((use-ssl (or twindrill-use-ssl twindrill-oauth-use-ssl)))
     (twindrill-lookup-connection-type use-ssl))
   ;; The account has been already authorized.
   (twindrill-account-authorized-p)))

(defun twindrill-ensure-preparation-for-api-invocation ()
  "Ensure prerequisites for invoking APIs. Return non-nil in success.
If prerequisites has been already satisifed, just return non-nil.
If prerequisites are not satisfied, this function try to satisfy them.
Then, return non-nil if they has been satisfied and return nil otherwise."
  (twindrill-initialize-global-variables-if-necessary)
  (and (twindrill-ensure-connection-method)
       (twindrill-ensure-private-info)
       (twindrill-ensure-account-verification)))

;;;;
;;;; Commands
;;;;

;;;; Commands for changing modes

(defun twindrill-toggle-reverse-mode (&optional arg)
  (interactive "P")
  (let ((prev-mode twindrill-reverse-mode))
    (setq twindrill-reverse-mode
	  (if (null arg)
	      (not twindrill-reverse-mode)
	    (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twindrill-reverse-mode)
      (twindrill-update-mode-line)
      (twindrill-rerender-timeline-all (current-buffer)))))

(defun twindrill-set-current-hashtag (&optional tag)
  (interactive)
  (unless tag
    (setq tag (twindrill-completing-read "hashtag (blank to clear): #"
					  twindrill-hashtag-history
					  nil nil
					  twindrill-current-hashtag
					  'twindrill-hashtag-history))
    (message
     (if (eq 0 (length tag))
	 (progn (setq twindrill-current-hashtag nil)
		"Current hashtag is not set.")
       (progn
	 (setq twindrill-current-hashtag tag)
	 (format "Current hashtag is #%s" twindrill-current-hashtag))))))

;;;; Commands for switching buffers
(defun twindrill-switch-to-next-timeline ()
  (interactive)
  (when (twindrill-buffer-p)
    (let* ((buffer-list (twindrill-get-buffer-list))
	   (following-buffers (cdr (memq (current-buffer) buffer-list)))
	   (next (if following-buffers
		     (car following-buffers)
		   (car buffer-list))))
      (unless (eq (current-buffer) next)
	(switch-to-buffer next)))))

(defun twindrill-switch-to-previous-timeline ()
  (interactive)
  (when (twindrill-buffer-p)
    (let* ((buffer-list (reverse (twindrill-get-buffer-list)))
	   (preceding-buffers (cdr (memq (current-buffer) buffer-list)))
	   (previous (if preceding-buffers
			 (car preceding-buffers)
		       (car buffer-list))))
      (unless (eq (current-buffer) previous)
	(switch-to-buffer previous)))))

;;;; Commands for visiting a timeline
(defun twindrill-visit-timeline (&optional timeline-spec initial)
  (interactive)
  (cond
   ((twindrill-ensure-preparation-for-api-invocation)
    (let ((timeline-spec
	   (or timeline-spec
	       (twindrill-read-timeline-spec-with-completion
		"timeline: " initial t))))
      (when timeline-spec
	(switch-to-buffer (twindrill-get-managed-buffer timeline-spec)))))
   (t
    nil)))

(defun twindrill-friends-timeline ()
  (interactive)
  (twindrill-visit-timeline '(friends)))

(defun twindrill-home-timeline ()
  (interactive)
  (twindrill-visit-timeline '(home)))

(defun twindrill-replies-timeline ()
  (interactive)
  (twindrill-visit-timeline '(replies)))

(defun twindrill-public-timeline ()
  (interactive)
  (twindrill-visit-timeline '(public)))

(defun twindrill-user-timeline ()
  (interactive)
  (twindrill-visit-timeline `(user ,(twindrill-get-username))))

(defun twindrill-direct-messages-timeline ()
  (interactive)
  (twindrill-visit-timeline '(direct_messages)))

(defun twindrill-sent-direct-messages-timeline ()
  (interactive)
  (twindrill-visit-timeline '(direct_messages_sent)))

(defun twindrill-other-user-timeline ()
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (goto-spec (get-text-property (point) 'goto-spec))
	 (screen-name-in-text
	  (get-text-property (point) 'screen-name-in-text))
	 (uri (or (get-text-property (point) 'expanded-uri)
		  (get-text-property (point) 'uri)))
	 (mentioned-id (when uri
			 (twindrill-extract-id-from-url uri)))
	 (spec (cond (goto-spec goto-spec)
		     (screen-name-in-text `(user ,screen-name-in-text))
		     (mentioned-id `(single ,mentioned-id))
		     (username `(user ,username))
		     (t nil))))
    (if spec
	(twindrill-visit-timeline spec)
      (message "No user selected"))))

(defun twindrill-other-user-timeline-interactive ()
  (interactive)
  (let ((username (or (twindrill-read-username-with-completion
		       "user: " nil
		       'twindrill-user-history)
		      "")))
    (if (string= "" username)
	(message "No user selected")
      (twindrill-visit-timeline `(user ,username)))))

(defun twindrill-other-user-list-interactive (&optional subscriptions)
  (interactive "P")
  (let* ((username (copy-sequence (get-text-property (point) 'username)))
	 (username (progn
		     (set-text-properties 0 (length username) nil username)
		     (or (twindrill-read-username-with-completion
			  (if subscriptions
			      "Whose subscription: "
			    "Whose list: ")
			  username
			  'twindrill-user-history)
			 ""))))
    (if (string= "" username)
	(message "No user selected")
      (let* ((list-name (if subscriptions
			    (twindrill-read-subscription-list-name username)
			  (twindrill-read-list-name username)))
	     (spec (cond
		    ((null list-name)
		     nil)
		    (subscriptions
		     (and (string-match "\\`\\(.*\\)/\\(.*\\)\\'" list-name)
			  `(list ,(match-string 1 list-name)
				 ,(match-string 2 list-name))))
		    (t
		     `(list ,username ,list-name)))))
	(if spec
	    (twindrill-visit-timeline spec)
	  ;; Don't show message here to prevent an overwrite of a
	  ;; message which is outputted by `twindrill-read-list-name'.
	  )))))

(defun twindrill-search (&optional word)
  (interactive)
  (let ((word (or word
		  (read-from-minibuffer "search: " nil nil nil
					'twindrill-search-history nil t)
		  "")))
    (if (string= "" word)
	(message "No query string")
      (let ((spec `(search ,word)))
	(twindrill-visit-timeline spec)))))

;;;; Commands for retrieving statuses

(defun twindrill-current-timeline-noninteractive ()
  (twindrill-current-timeline t))

(defun twindrill-current-timeline (&optional noninteractive)
  (interactive)
  (when (twindrill-buffer-p)
    (let ((spec-string (twindrill-current-timeline-spec-string)))
      (twindrill-get-and-render-timeline noninteractive))))

(defun twindrill-get-tweets-within-specific-time-range (time-beg time-end)
  "Get tweets within a time range between TIME-BEG and TIME-END.
TIME-BEG and TIME-END must be nil or an internal representation of time as
same as the returned value of `current-time'."
  (let* ((since_id (when time-beg
		     (twindrill-time-to-id time-beg)))
	 (max_id (when time-end
		   (twindrill-time-to-id time-end)))
	 (spec-string (twindrill-current-timeline-spec-string))
	 (noninteractive t)
	 (args
	  `(,@(cond
	       (max_id `((max_id . ,max_id)))
	       (since_id `((since_id . ,since_id)))
	       (t nil)))))
    (twindrill-retrieve-timeline spec-string noninteractive args nil)))

(defun twindrill-get-tweets-before (&optional before-str)
  (interactive)
  (let* ((id (when (null before-str)
	       (twindrill-get-id-at)))
	 (init-str
	  (when id
	    (let* ((status (twindrill-find-status id))
		   (init-time
		    (or (cdr (assq 'retweeting-created-at status))
			(cdr (assq 'created-at status)))))
	      (format-time-string "%Y-%m-%d %T" init-time))))
	 (before-str
	  (or before-str
	      (read-string "before [YYYY-MM-DD [HH:MM:SS]]: " init-str)))
	 (time-beg nil)
	 (time-end
	  (apply 'encode-time (twindrill-parse-time-string before-str t))))
    (twindrill-get-tweets-within-specific-time-range time-beg time-end)))

;;;; Commands for posting a status

(defun twindrill-update-status (&optional init-string-or-skeleton reply-to-id username tweet-type ignore-current-spec)
  "Post a tweet.
The first argument INIT-STRING-OR-SKELETON is nil, an initial text or a
skeleton to be inserted with `skeleton-insert'.
REPLY-TO-ID is an ID of a tweet which you are going to cite or reply to.
USERNAME is a recipient of a direct message.
TWEET-TYPE is a symbol meaning the type of the tweet being edited. It must
be one of 'direct-message, 'normal, 'organic-retweet and 'reply.
If TWEET-TYPE is nil, it is equivalent to 'normal, which means that a tweet
is edited as a normal tweet.
If IGNORE-CURRENT-SPEC is non-nil, the timeline spec of the current buffer
is sent to the function specified by `twindrill-update-status-function'.

How to edit a tweet is determined by `twindrill-update-status-funcion'."
  (let ((current-spec (unless ignore-current-spec
			(twindrill-current-timeline-spec)))
	(tweet-type (or tweet-type 'normal)))
    (funcall twindrill-update-status-function init-string-or-skeleton
	     reply-to-id username
	     tweet-type current-spec)))

(defun twindrill-update-status-interactive ()
  (interactive)
  (twindrill-update-status))

(defun twindrill-update-lambda ()
  (interactive)
  (when (and (string= "Japanese" current-language-environment)
	     (or (< 21 emacs-major-version)
		 (eq 'utf-8 (terminal-coding-system))))
    (let ((text (mapconcat
		 'char-to-string
		 (mapcar 'twindrill-ucs-to-char
			 '(955 12363 12431 12356 12356 12424 955)) "")))
      (twindrill-call-api 'update-status `((status . ,text))))))

(defun twindrill-direct-message ()
  (interactive)
  (let ((username (twindrill-read-username-with-completion
		   "Who would you like to receive the DM? "
		   (get-text-property (point) 'username)
		   'twindrill-user-history)))
    (if (string= "" username)
	(message "No user selected")
      (twindrill-update-status nil nil username 'direct-message))))

(defun twindrill-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twindrill-update-status (concat "@" username " "))
      (message "No user selected"))))

;;;; Command for deleting a status

(defun twindrill-delete-status (&optional id)
  (interactive)
  (let* ((id (twindrill-get-id-at))
	 (status (twindrill-find-status id))
	 (is-retweet (assq 'retweeted-id status))
	 (username (if is-retweet
		       (cdr (assq 'retweeting-user-screen-name status))
		     (cdr (assq 'user-screen-name status))))
	 (text (if is-retweet
		   (cdr (assq 'retweeting-text status))
		 (cdr (assq 'text status))))
	 (width (max 40 ;; XXX
		     (- (frame-width)
			1 ;; margin for wide characters
			11 ;; == (length (concat "Delete \"" "\"? "))
			9) ;; == (length "(y or n) ")
		     ))
	 (mes (format "Delete \"%s\"? "
		      (if (< width (string-width text))
			  (concat
			   (truncate-string-to-width text (- width 3))
			   "...")
			text))))
    (cond
     ((not (string= username (twindrill-get-username)))
      (message "The status is not yours!"))
     ((not id)
      (message "No status selected"))
     ((y-or-n-p mes)
      (twindrill-call-api 'destroy-status `((id . ,id))))
     (t
      (message "Request canceled")))))

;;;; Commands for retweet

(defun twindrill-retweet (&optional arg)
  (interactive "P")
  (let ((use-native-retweet-flag (if arg
				     (not twindrill-use-native-retweet)
				   twindrill-use-native-retweet)))
    (if use-native-retweet-flag
	(twindrill-native-RT)
      (twindrill-organic-RT))))

(defun twindrill-organic-RT ()
  (interactive)
  (let* ((id (twindrill-get-id-at))
	 (status (twindrill-find-status id))
	 (username (cdr (assq 'user-screen-name status)))
	 (text (cdr (assq 'text status)))
	 (retweet-time (current-time))
	 (skeleton-with-format-string
	  (cond
	   ((null twindrill-retweet-format)
	    '(nil _ " RT: %t (via @%s)"))
	   ((stringp twindrill-retweet-format)
	    `(nil ,twindrill-retweet-format _))
	   ((listp twindrill-retweet-format)
	    twindrill-retweet-format)
	   (t
	    nil))))
    (cond
     ((cdr (assq 'user-protected status))
      (error "Cannot retweet protected tweets."))
     (username
      (let ((prefix "%")
	    (replace-table
	     `(("%" . "%")
	       ("s" . ,username)
	       ("t" . ,text)
	       ("#" . ,id)
	       ("u" . ,(twindrill-get-status-url-from-alist status))
	       ("C{\\([^}]*\\)}" .
		(lambda (context)
		  (let ((str (cdr (assq 'following-string context)))
			(match-data (cdr (assq 'match-data context))))
		    (store-match-data match-data)
		    (format-time-string (match-string 1 str) ',retweet-time))))
	       ))
	    )
	(twindrill-update-status
	 (mapcar (lambda (element)
		   (if (stringp element)
		       (twindrill-format-string element prefix replace-table)
		     element))
		 skeleton-with-format-string)
	 id nil 'organic-retweet)
	)))))

(defun twindrill-native-RT ()
  (interactive)
  (let ((id (get-text-property (point) 'id))
	(text (copy-sequence (get-text-property (point) 'text)))
	(user (get-text-property (point) 'username))
	(width (max 40 ;; XXX
		    (- (frame-width)
		       1 ;; margin for wide characters
		       12 ;; == (length (concat "Retweet \"" "\"? "))
		       9) ;; == (length "(y or n) ")
		    )))
    (set-text-properties 0 (length text) nil text)
    (if id
	(if (not (string= user twindrill-username))
	    (let ((mes (format "Retweet \"%s\"? "
			       (if (< width (string-width text))
				   (concat
				    (truncate-string-to-width text (- width 3))
				    "...")
				 text))))
	      (if (y-or-n-p mes)
		  (twindrill-call-api 'retweet `((id . ,id)))
		(message "Request canceled")))
	  (message "Cannot retweet your own tweet"))
      (message "No status selected"))))

;;;; Commands for browsing information related to a status

(defun twindrill-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twindrill-enter ()
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (id (twindrill-get-id-at (point)))
	 (uri (get-text-property (point) 'uri))
	 (tweet-type
	  (cond
	   ((twindrill-timeline-spec-is-direct-messages-p
	     (get-text-property (point) 'source-spec))
	    'direct-message)
	   (t
	    'reply)))
	 (screen-name-in-text
	  (get-text-property (point) 'screen-name-in-text))
	 (initial-str
	  (when (and (not (eq tweet-type 'direct-message))
		     (or screen-name-in-text username))
	    (concat "@" (or screen-name-in-text username) " ")))
	 (field-id (get-text-property (point) 'field))
	 (is-latest-end (twindrill-field-id-is-timeline-latest-end field-id))
	 (is-oldest-end (twindrill-field-id-is-timeline-oldest-end field-id)))
    (cond
     (is-latest-end
      (message "Get more of the recent timeline...")
      (if twindrill-reverse-mode
	  (twindrill-goto-last-normal-field)
	(twindrill-goto-first-normal-field))
      (twindrill-get-and-render-timeline))
     (is-oldest-end
      (let* ((oldest-status (car (last (twindrill-current-timeline-data))))
	     (oldest-id (cdr (assq 'id oldest-status))))
	(message "Get more of the previous timeline...")
	(if twindrill-reverse-mode
	    (twindrill-goto-first-normal-field)
	  (twindrill-goto-last-normal-field))
	(twindrill-get-and-render-timeline nil oldest-id)))
     (screen-name-in-text
      (twindrill-update-status initial-str
				id screen-name-in-text tweet-type))
     (uri
      (browse-url uri))
     (username
      (twindrill-update-status initial-str id username tweet-type)))))

(defun twindrill-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

;;;;
;;;; Commands corresponding to operations on Twitter
;;;;

(defun twindrill-follow (&optional remove)
  (interactive "P")
  (let* ((method (if remove 'destroy-friendships 'create-friendships))
	 (mes (if remove "unfollow" "follow"))
	 (id (twindrill-get-id-at))
	 (status (when id (twindrill-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (default (if remove
				retweeting-username
			      retweeted-username))
		   (prompt (format "Who do you %s? (default:%s): "
				   mes default))
		   (candidates (list retweeted-username retweeting-username)))
	      (twindrill-completing-read prompt candidates nil t
					  nil nil default)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    (twindrill-read-username-with-completion
	     (format "Who do you %s? " mes) "" 'twindrill-user-history)))))
    (if (string= "" username)
	(message "No user selected")
      (if (y-or-n-p (format "%s %s? " (capitalize mes) username))
	  (twindrill-call-api method `((username . ,username)))
	(message "Request canceled")))))

(defun twindrill-unfollow ()
  (interactive)
  (twindrill-follow t))

(defun twindrill-favorite (&optional remove)
  (interactive "P")
  (let ((id (get-text-property (point) 'id))
	(text (copy-sequence (get-text-property (point) 'text)))
	(width (max 40 ;; XXX
		    (- (frame-width)
		       1 ;; margin for wide characters
		       15 ;; == (length (concat "Unfavorite \"" "\"? "))
		       9) ;; == (length "(y or n) ")
		    ))
	(method (if remove 'destroy-favorites 'create-favorites)))
    (set-text-properties 0 (length text) nil text)
    (if id
	(let ((mes (format "%s \"%s\"? "
			   (if remove "Unfavorite" "Favorite")
			   (if (< width (string-width text))
			       (concat
				(truncate-string-to-width text (- width 3))
				"...")
			     text))))
	  (if (or (not twindrill-request-confirmation-on-favouriting)
                  (y-or-n-p mes))
	      (twindrill-call-api method `((id . ,id)))
	    (message "Request canceled")))
      (message "No status selected"))))

(defun twindrill-unfavorite ()
  (interactive)
  (twindrill-favorite t))

(defun twindrill-mute (&optional remove)
  (interactive "P")
  (let* ((method (if remove 'unmute 'mute))
	 (mes (if remove "unmute" "mute"))
	 (id (twindrill-get-id-at))
	 (status (when id (twindrill-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (default (if remove
				retweeting-username
			      retweeted-username))
		   (prompt (format "Who do you %s? (default:%s): "
				   mes default))
		   (candidates (list retweeted-username retweeting-username)))
	      (twindrill-completing-read prompt candidates nil t
					  nil nil default)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    (twindrill-read-username-with-completion
	     (format "Who do you %s? " mes) "" 'twindrill-user-history)))))
    (if (string= "" username)
	(message "No user selected")
      (if (y-or-n-p (format "%s %s? " (capitalize mes) username))
	  (twindrill-call-api method `((username . ,username)))
	(message "Request canceled")))))

(defun twindrill-unmute ()
  (interactive)
  (twindrill-mute t))

(defun twindrill-block ()
  "Block a user who posted the tweet at the current position."
  (interactive)
  (let* ((id (twindrill-get-id-at))
	 (status (when id (twindrill-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (prompt "Who do you block? ")
		   (candidates (list retweeted-username retweeting-username)))
	      (twindrill-completing-read prompt candidates nil t)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    nil))))
    (cond
     ((or (null username) (string= "" username))
      (message "No user selected"))
     ((yes-or-no-p (format "Really block \"%s\"? " username))
      (twindrill-call-api 'block `((username . ,username))))
     (t
      (message "Request canceled")))))

(defun twindrill-block-and-report-as-spammer ()
  "Report a user who posted the tweet at the current position as a spammer.
The user is also blocked."
  (interactive)
  (let* ((id (twindrill-get-id-at))
	 (status (when id (twindrill-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (prompt "Who do you report as a spammer? ")
		   (candidates (list retweeted-username retweeting-username)))
	      (twindrill-completing-read prompt candidates nil t)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    nil))))
    (cond
     ((or (null username) (string= "" username))
      (message "No user selected"))
     ((yes-or-no-p
       (format "Really block \"%s\" and report him or her as a spammer? "
	       username))
      (twindrill-call-api 'block-and-report-as-spammer
			   `((username . ,username))))
     (t
      (message "Request canceled")))))

;;;; Commands for clearing stored statuses.

(defun twindrill-erase-old-statuses ()
  (interactive)
  (when (twindrill-buffer-p)
    (let ((spec (twindrill-current-timeline-spec)))
      (twindrill-remove-timeline-data spec) ;; clear current timeline.
      (twindrill-rerender-timeline-all (current-buffer)) ;; clear buffer.
      (twindrill-get-and-render-timeline))))

;;;; Cursor motion

(defun twindrill-get-id-at (&optional pos)
  "Return ID of the status at POS. If a separator is rendered at POS, return
the ID of the status rendered before the separator. The default value of POS
is `(point)'."
  (let ((pos (or pos (point))))
    (or (get-text-property pos 'id)
	(let ((prev (or (twindrill-get-current-status-head pos)
			(point-min))))
	  (and prev (get-text-property prev 'id))))))

(defun twindrill-get-current-status-head (&optional pos)
  "Return the head position of the status at POS.
If POS is nil, the value of point is used for POS.
If a separator is rendered at POS, return the head of the status followed
by the separator.
Return POS if no statuses are rendered."
  (let* ((pos (or pos (point)))
	 (field-id (get-text-property pos 'field))
	 ;; Find the beginning of the field regardless of stickiness.
	 (head (field-beginning pos t)))
    (cond
     ((null field-id)
      ;; A separator is rendered at `pos'.
      (if (get-text-property head 'field)
	  ;; When `pos' points the head of the separator, `head' points
	  ;; to the beginning of the status followed by the separator.
	  head
	;; In the case that `pos' points to a character of the separator,
	;; but not to the head of the separator.
	(field-beginning head t)))
     ((null (get-text-property head 'field))
      ;; When `head' points to a separator, `pos' points to the head
      ;; of a status.
      pos)
     ((not (twindrill-field-id= field-id (get-text-property head 'field)))
      ;; When `pos' points to the beginning of the field and it also
      ;; points to the end of the previous field, `head' points to the
      ;; head of the previous status.
      pos)
     (t
      head))))

(defun twindrill-goto-first-status ()
  "Go to the first status."
  (interactive)
  (goto-char (or (twindrill-get-first-status-head)
		 (point-min))))

(defun twindrill-get-first-status-head ()
  "Return the head position of the first status in the current buffer.
Return nil if no statuses are rendered."
  (if (get-text-property (point-min) 'field)
      (point-min)
    (twindrill-get-next-status-head (point-min))))

(defun twindrill-goto-last-status ()
  "Go to the last status."
  (interactive)
  (goto-char (or (twindrill-get-last-status-head)
		 (point-min))))

(defun twindrill-get-last-status-head ()
  "Return the head position of the last status in the current buffer.
Return nil if no statuses are rendered."
  (if (get-text-property (point-max) 'field)
      (point-max)
    (twindrill-get-previous-status-head (point-max))))

(defun twindrill-goto-first-normal-field ()
  "Go to the first normal field.
A normal field is a field corresponding to a tweet."
  (interactive)
  (goto-char (or (twindrill-get-first-normal-field-head)
		 (point-min))))

(defun twindrill-goto-last-normal-field ()
  "Go to the last normal field.
A normal field is a field corresponding to a tweet."
  (interactive)
  (goto-char (or (twindrill-get-last-normal-field-head)
		 (point-max))))

(defun twindrill-get-first-normal-field-head ()
  "Return the head position of the first normal field in the current buffer.
A normal field is a field corresponding to a tweet.
Return nil if no statuses are rendered."
  (let ((pos (twindrill-get-first-status-head)))
    (while (and pos
		(< pos (point-max))
		(null (get-text-property pos 'id)))
      (setq pos (twindrill-get-next-status-head pos)))
    (when (and pos (< pos (point-max)) (get-text-property pos 'id))
      pos)))

(defun twindrill-get-last-normal-field-head ()
  "Return the head position of the last normal field in the current buffer.
A normal field is a field corresponding to a tweet.
Return nil if no statuses are rendered."
  (let ((pos (twindrill-get-last-status-head)))
    (while (and pos
		(< (point-min) pos)
		(null (get-text-property pos 'id)))
      (setq pos (twindrill-get-previous-status-head pos)))
    (when (and pos (< (point-min) pos) (get-text-property pos 'id))
      pos)))

(defun twindrill-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos (twindrill-get-next-status-head)))
    (cond
     (pos
      (goto-char pos))
     (twindrill-reverse-mode
      (message "The latest status."))
     (t
      (let* ((oldest-status (car (last (twindrill-current-timeline-data))))
	     (oldest-id (cdr (assq 'id oldest-status)))
	     (spec-type (car (twindrill-current-timeline-spec))))
	(cond
	 (oldest-id
	  (message "Get more of the previous timeline...")
	  ;; Here, the cursor points to the footer field or the end of
	  ;; the buffer. It should be moved backward to a normal tweet.
	  (twindrill-goto-last-normal-field)
	  (twindrill-get-and-render-timeline nil oldest-id))))))))

(defun twindrill-get-next-status-head (&optional pos)
  "Search forward from POS for the nearest head of a status.
Return nil if there are no following statuses.
Otherwise, return a positive integer greater than POS."
  (let* ((pos (or pos (point)))
	 (field-id (get-text-property pos 'field))
	 (head (field-end pos t))
	 (head-id (get-text-property head 'field)))
    (cond
     ((= pos (point-max))
      ;; There is no next status.
      nil)
     ((and (null field-id) head-id)
      ;; `pos' points to a separator and `head' points to a head
      ;; of a status.
      head)
     ((null head-id)
      ;; `head' points to a head of a separator.
      (let ((next-head (field-end head t)))
	(if (get-text-property next-head 'field)
	    next-head
	  ;; There is no next status.
	  nil)))
     (t
      head))))

(defun twindrill-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((prev-pos (twindrill-get-previous-status-head)))
    (cond
     (prev-pos
      (goto-char prev-pos))
     (twindrill-reverse-mode
      (let* ((oldest-status (car (last (twindrill-current-timeline-data))))
	     (oldest-id (cdr (assq 'id oldest-status)))
	     (spec-type (car (twindrill-current-timeline-spec))))
	(cond
	 (oldest-id
	  (message "Get more of the previous timeline...")
	  ;; Here, the cursor points to the header field.
	  ;; It should be moved forward to a normal tweet.
	  (twindrill-goto-first-normal-field)
	  (twindrill-get-and-render-timeline nil oldest-id)))))
     (t
      (message "The latest status.")))))

(defun twindrill-get-previous-status-head (&optional pos)
  "Search backward from POS for the nearest head of a status.
If POS points to a head of a status, return the head of the *previous* status.
If there are no preceding statuses, return nil.
Otherwise, return a positive integer less than POS."
  (let* ((pos (or pos (point)))
	 (field-id (get-text-property pos 'field))
	 (head (field-beginning pos t))
	 (head-id (get-text-property head 'field)))
    (cond
     ((= pos (point-min))
      ;; There is no previous status.
      nil)
     ((and (null field-id) head-id)
      ;; `pos' points to a separator and `head' points to a head
      ;; of a status.
      head)
     ((null head-id)
      ;; `head' points to a head of a separator.
      (let ((prev-head (field-beginning head t)))
	(if (get-text-property prev-head 'field)
	    prev-head
	  ;; There is no previous status.
	  nil)))
     (t
      head))))

(defun twindrill-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twindrill-get-username-at-pos (point)))
	(pos (twindrill-get-next-status-head (point))))
    (while (and (not (eq pos nil))
		(not (equal (twindrill-get-username-at-pos pos) user-name)))
      (setq pos (twindrill-get-next-status-head pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "End of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twindrill-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (twindrill-get-username-at-pos (point)))
	(prev-pos (point))
	(pos (twindrill-get-previous-status-head (point))))
    (while (and (not (eq pos nil))
		(not (eq pos prev-pos))
		(not (equal (twindrill-get-username-at-pos pos) user-name)))
      (setq prev-pos pos)
      (setq pos (twindrill-get-previous-status-head pos)))
    (if (and pos
	     (not (eq pos prev-pos))
	     (equal (twindrill-get-username-at-pos pos) user-name))
	(goto-char pos)
      (if user-name
	  (message "Start of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twindrill-get-next-thing-pos (&optional backward ignore-implicit-uri)
  "Return the position of the next/previous thing.

The thing is one of username or URI or string with uri property.
If BACKWARD is nil, return the position of the next thing.
If BACKWARD is non-nil, return the position of the previous thing.

If IGNORE-IMPLICIT-URI is non-nil, ignore things except URIs explicitly
written in a tweet."
  (let* ((property-sym (if ignore-implicit-uri
			   'uri
			 'face))
	 (property-change-f (if backward
				'previous-single-property-change
			      'next-single-property-change))
	 (pos (funcall property-change-f (point) property-sym)))
    (while (and
	    pos
	    (cond
	     (ignore-implicit-uri
	      (not (eq 'explicit-uri-in-tweet
		       (get-text-property pos 'uri-origin))))
	     (t
	      (let* ((current-face (get-text-property pos property-sym))
		     (face-pred
		      (lambda (face)
			(cond
			 ((listp current-face) (memq face current-face))
			 ((symbolp current-face) (eq face current-face))
			 (t nil)))))
		(not (remove nil
			     (mapcar face-pred '(twindrill-username-face
						 twindrill-uri-face))))))))
      (setq pos (funcall property-change-f pos property-sym)))
    pos))

(defun twindrill-goto-next-thing (&optional arg)
  "Go to next interesting thing. ex) username, URI, ...

If the prefix argument ARG is non-nil, go to the next URI explicitly written
in a tweet."
  (interactive "P")
  (if arg
      (twindrill-goto-next-uri)
    (let* ((backward nil)
	   (pos (twindrill-get-next-thing-pos backward)))
      (when pos
	(goto-char pos)))))

(defun twindrill-goto-previous-thing (&optional arg)
  "Go to previous interesting thing. ex) username, URI, ...

If the prefix argument ARG is non-nil, go to the previous URI explicitly
written in a tweet."
  (interactive "P")
  (if arg
      (twindrill-goto-previous-uri)
    (let* ((backward t)
	   (pos (twindrill-get-next-thing-pos backward)))
      (when pos
	(goto-char pos)))))

(defun twindrill-goto-next-uri ()
  "Go to the next URI."
  (interactive)
  (let* ((ignore-implicit-uri t)
	 (backward nil)
	 (pos (twindrill-get-next-thing-pos backward ignore-implicit-uri)))
    (when pos
      (goto-char pos))))

(defun twindrill-goto-previous-uri ()
  "Go to the previous URI."
  (interactive)
  (let* ((ignore-implicit-uri t)
	 (backward t)
	 (pos (twindrill-get-next-thing-pos backward ignore-implicit-uri)))
    (when pos
      (goto-char pos))))

(defun twindrill-get-username-at-pos (pos)
  (or (get-text-property pos 'username)
      (get-text-property (max (point-min) (1- pos)) 'username)
      (let* ((border (or (previous-single-property-change pos 'username)
                         (point-min)))
             (pos (max (point-min) (1- border))))
        (get-text-property pos 'username))))

(defun twindrill-scroll-up()
  "Scroll up if possible; otherwise invoke `twindrill-goto-next-status',
which fetch older tweets on non reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-max))
    (twindrill-goto-next-status))
   ((= (window-end) (point-max))
    (goto-char (point-max)))
   (t
    (scroll-up))))

(defun twindrill-scroll-down()
  "Scroll down if possible; otherwise invoke `twindrill-goto-previous-status',
which fetch older tweets on reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-min))
    (twindrill-goto-previous-status))
   ((= (window-start) (point-min))
    (goto-char (point-min)))
   (t
    (scroll-down))))

;;;; Kill ring

(defun twindrill-push-uri-onto-kill-ring ()
  "Push URI on the current position onto the kill ring.
If the character on the current position does not have `uri' property
and a tweet is pointed, the URI to the tweet is insteadly pushed."
  (interactive)
  (let ((uri (or (get-text-property (point) 'uri)
		 (if (get-text-property (point) 'field)
		     (let* ((id (get-text-property (point) 'id))
			    (status (twindrill-find-status id)))
		       (twindrill-get-status-url-from-alist status))
		   nil))))
    (cond
     ((not (stringp uri))
      nil)
     ((and kill-ring (string= uri (current-kill 0 t)))
      (message "Already copied %s" uri)
      uri)
     (t
      (kill-new uri)
      (message "Copied %s" uri)
      uri))))

(defun twindrill-push-tweet-onto-kill-ring ()
  "Copy the tweet (format: \"username: text\") to the kill-ring."
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (text (get-text-property (point) 'text))
	 (copy (if (and username text)
		   (format "%s: %s" username text)
		 nil)))
    (cond
     ((null copy)
      nil)
     ((and kill-ring (string= copy (current-kill 0 t)))
      (message "Already copied %s" copy))
     (t
      (kill-new copy)
      (message "Copied %s" copy)
      copy))))

;;;; Suspend

(defun twindrill-suspend ()
  "Suspend twindrill-mode then switch to another buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;;
;;;; Resuming timeline buffers with revive.el
;;;;
(eval-when-compile
  (if (require 'revive nil t)
      (defmacro twindrill-revive:prop-get-value (x y)
	(macroexpand `(revive:prop-get-value ,x ,y)))
    ;; If `revive.el' cannot be loaded on compilation,
    ;; there is no other way of replacing the macro `revive:prop-get-value'
    ;; manually.
    ;; The current implementation assumes the `revive.el' 2.19.
    (defmacro twindrill-revive:prop-get-value (x y)
      `(cdr (assq ,y (nth 5 ,x))))))

(defun twindrill-revive:twindrill ()
  "Restore `twindrill-mode' timeline buffer with `revive.el'.
The Emacs LISP program `revive.el' written by HIROSE Yuuji can restore
timeline buffers of `twindrill-mode' by using this function.
There are two ways of configurations as follows;
1.manual registration
 (add-to-list 'revive:major-mode-command-alist-private
              '(twindrill-mode . twindrill-revive:twindrill))
 (add-to-list 'revive:save-variables-local-private
              '(twindrill-mode twindrill-timeline-spec-string))
 (require 'revive)

2.automatic registration (for revive.el 2.19)
 (require 'revive)
 (twindrill-setup-revive)

Note that (add-to-list ...) of the manual configuration must be evaluated
before loading `revive.el' and (twindrill-setup-revive) of the automatic one
must be evaluated after loading `revive.el'.
Since the Emacs LISP program `windows.el' written by HIROSE Yuuji
implicitly loads `revive.el' if possible, you should also take care
of the order of `windows.el' and the configuration."
  (interactive)
  (twindrill-visit-timeline
   (twindrill-revive:prop-get-value x 'twindrill-timeline-spec-string)))

(defun twindrill-setup-revive ()
  "Prepare the configuration of `revive.el' for twindrill-mode.
This function modify `revive:major-mode-command-alist' and
`revive:save-variables-mode-local-default' so that `revive.el' can restore
the timeline buffers of twindrill-mode.

This function must be invoked after loading `revive.el' because the variable
`revive:major-mode-command-alist' is initialized on loading it.
Note that the current implementation assumes `revive.el' 2.19 ."
  (unless (featurep 'revive)
    (error "`revive' has not been loaded yet"))
  (add-to-list 'revive:major-mode-command-alist
               '(twindrill-mode . twindrill-revive:twindrill) t)
  (add-to-list 'revive:save-variables-mode-local-default
               '(twindrill-mode twindrill-timeline-spec-string) t)
  nil)


(defalias 'twind-tweet 'twindrill-update-status-interactive)

;;;###autoload
(defun twind ()
  "Start twindrill-mode."
  (interactive)
  (twindrill-mode))

;; 　　　　　 　r /
;; 　 ＿＿ , --ヽ!-- .､＿
;; 　! 　｀/::::;::::ヽ l
;; 　!二二!::／}::::丿ハﾆ|
;; 　!ﾆニ.|:／　ﾉ／ }::::}ｺ
;; 　L二lイ　　0´　0 ,':ﾉｺ
;; 　lヽﾉ/ﾍ､ ''　▽_ノイ ソ
;;  　ソ´ ／}｀ｽ /￣￣￣￣/
;; 　　　.(_:;つ/  0401 /　ｶﾀｶﾀ
;;  ￣￣￣￣￣＼/＿＿＿＿/

(provide 'twindrill-mode)

                  (progn  (when  (
                   boundp  (  intern (
                    mapconcat 'identity '
                    ("twindrill" "oauth"
                      "consumer" "key" ) "-"
                       )  )  )   (eval  ` (
                        setq ,(intern (mapconcat
                         (quote identity) (quote
                          ("twindrill"    "oauth"
                           "consumer" "key")  )"-"
                           ))  (base64-decode-string
                         (apply  'string  (mapcar   '1-
                        (quote (90 111 101 107 90 88 58 108
                      82 49 105 117 83 89 113 118 102 73 109
                     72 90 86 74 54 99 109 113 98 99 52 75 118
                   91 82 62 62))))))       ))( when ( boundp  (
                  intern (mapconcat '      identity'("twindrill"
                 "oauth" "consumer"         "secret") "-")))(eval `
                (setq  ,(intern   (         mapconcat 'identity '(
               "twindrill" "oauth"          "consumer" "secret") "-"))
              (base64-decode-string          (apply 'string (mapcar '1-
            (quote (86 51 109 84 82          110 91 87 99 88 121 72 99 49
           113 112 90 51 71 49 78 86          71 55 99 49 117 84 86 52 75
          49 102 71 101 78 85 52 91           82 100 72 100 50 98 50 83 72
         99 88 50 83 80 72 105 71 90          86 113 85 98 49 112 51 101 70 78
        62))))))))(concat "ξ ^ω^)ξ"        "＜ You are an idiot, really."))

;;; twindrill-mode.el ends here
