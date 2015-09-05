;;; twindrill-api.el --- Utilities for twindrill-mode

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

(defvar twindrill-api-host "api.twitter.com")
(defvar twindrill-api-search-host "search.twitter.com")
(defvar twindrill-web-host "twitter.com")
(defvar twindrill-oauth-request-token-url-without-scheme
  "://api.twitter.com/oauth/request_token")
(defvar twindrill-oauth-authorization-url-base-without-scheme
  "://api.twitter.com/oauth/authorize?oauth_token=")
(defvar twindrill-oauth-access-token-url-without-scheme
  "://api.twitter.com/oauth/access_token")

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

(defvar twindrill-server-info-alist nil
  "Alist of server information.")

(defvar twindrill-api-limit-info-alist '()
  "Alist of an API identifier and an alist representing rate limit for the API.")

(defvar twindrill-cookie-alist nil
  "Alist for stroing cookies for each account.
This variable stores an alist.
A key of the alist is a string that is a screen name of an account.
A value of the alist is a cookie alist which corresponds to a list of
a pair of a cookie name and value.")

(defcustom twindrill-use-ssl t
  "*Use SSL connection if this variable is non-nil.

SSL connections use an external command as a backend."
  :type 'boolean
  :group 'twindrill-mode)

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

(provide 'twindrill-api)
;;; twindrill-api.el ends here
