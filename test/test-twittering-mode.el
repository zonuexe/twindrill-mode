;;; -*- coding: utf-8 -*-

(defcase test-version nil nil
  (test-assert-string-match "^twindrill-mode-v\\([0-9]+\\(\\.[0-9]+\\)*\\|HEAD\\)"
    (twindrill-mode-version)))

(defcase test-assocref nil nil
  (test-assert-eq 'bar (assocref 'foo '((baz . qux) (foo . bar))))
  (test-assert-eq nil (assocref 'quxx '((baz . qux) (foo . bar)))))

(defmacro test-setup-proxy(bindings)
  `(let ,(append '((process-environment nil)
		   (twindrill-proxy-use t)
		   (twindrill-proxy-server nil)
		   (twindrill-proxy-port nil)
		   (twindrill-http-proxy-server nil)
		   (twindrill-http-proxy-port nil)
		   (twindrill-https-proxy-server nil)
		   (twindrill-https-proxy-port nil))
		 bindings)
     (twindrill-setup-proxy)))

(defcase test-proxy nil nil
  (test-assert-ok
   (test-setup-proxy
    ((twindrill-proxy-server "proxy.example.com")
     (twindrill-proxy-port 8080))))
  (test-assert-ok
   (test-setup-proxy
    ((twindrill-proxy-server "proxy.example.com")
     (twindrill-proxy-port "8080"))))

  (test-assert-ok
   (test-setup-proxy
    ((twindrill-http-proxy-server "proxy.example.com")
     (twindrill-http-proxy-port 8080))))
  (test-assert-ok
   (test-setup-proxy
    ((twindrill-http-proxy-server "proxy.example.com")
     (twindrill-http-proxy-port "8080"))))

  (test-assert-ok
   (test-setup-proxy
    ((twindrill-https-proxy-server "proxy.example.com")
     (twindrill-https-proxy-port 8080))))
  (test-assert-ok
   (test-setup-proxy
    ((twindrill-https-proxy-server "proxy.example.com")
     (twindrill-https-proxy-port "8080"))))

  (test-assert-equal
   (let ((twindrill-proxy-use nil)
	 (process-environment nil)
	 (twindrill-https-proxy-server "proxy.example.com")
	 (twindrill-https-proxy-port 8080))
     (twindrill-toggle-proxy)
     (list twindrill-proxy-use
	   (progn
	     (twindrill-toggle-proxy)
	     twindrill-proxy-use)))
   '(t nil))

  ;; The test of configuration via an environment variable should be
  ;; performed last because it changes global variables of the url library
  ;; in an irreversible manner.
  (test-assert-ok
   (test-setup-proxy
    ((process-environment
      '("http_proxy=http://proxy1.example.com:8080/"))))))

(defcase test-user-agent nil nil
  (test-assert-string-equal (format "Emacs/%d.%d Twindrill-mode/%s"
				    emacs-major-version
				    emacs-minor-version
				    twindrill-mode-version)
    (twindrill-user-agent))
  (setq twindrill-user-agent-function
	(lambda () "foo user agent"))
  (test-assert-string-equal "foo user agent"
    (twindrill-user-agent))
  )

(defcase test-icon-mode nil nil
  (setq twindrill-icon-mode nil)
  (twindrill-icon-mode)
  (test-assert-ok twindrill-icon-mode)
  (twindrill-icon-mode)
  (test-assert-ok (not twindrill-icon-mode))
  (twindrill-icon-mode nil)
  (test-assert-ok twindrill-icon-mode)
  (twindrill-icon-mode t)
  (test-assert-ok twindrill-icon-mode)
  (twindrill-icon-mode -1)
  (test-assert-ok (not twindrill-icon-mode))
  (twindrill-icon-mode 1)
  (test-assert-ok twindrill-icon-mode)
  )

(defcase test-percent-encode nil nil
  (test-assert-string-equal "Rinko"
    (twindrill-percent-encode "Rinko"))
  (test-assert-string-equal "%25"
    (twindrill-percent-encode "%"))
  (test-assert-string-equal "love%20plus"
    (twindrill-percent-encode "love plus"))
  (test-assert-string-equal "%0A"
    (twindrill-percent-encode "\n")))

(with-network
 (when (require 'url nil t)
   (defcase tinyurl nil nil
     (test-assert-string-equal "http://tinyurl.com/3xsrg5"
       (twindrill-tinyurl-get "http://example.com/example"))
     )))

(defcase case-string nil nil
  (test-assert-string-equal "Kobayakawa"
    (case-string "Rinko"
      (("Rinko") "Kobayakawa")
      (t "unknown")))

  (test-assert-string-equal "unknown"
    (case-string "Manaka"
      (("Rinko") "Kobayakawa")
      (t "unknown")))
  
  (test-assert-string-equal "Kobayakawa"
    (case-string "Rinko"
      (("Manaka") "Takane")
      (("Rinko") "Kobayakawa")
      (("Nene") "Anegasaki")
      (t nil)))

  (test-assert-string-equal "Amphibian"
    (case-string "Frog"
      (("Rabbit") "Mammal")
      (("Salamandar" "Frog") "Amphibian")
      (t nil)))
  )

(defcase format-string nil nil
  (test-assert-string-equal ""
    (twindrill-format-string "" "" nil))

  (test-assert-string-equal "Hello world"
    (twindrill-format-string "Hello world" "" nil))

  (test-assert-string-equal "RT: twindrill-mode now (via @twmode)"
    (twindrill-format-string "RT: %t (via @%s)" "%"
			      '(("t" . "twindrill-mode now")
				("s" . "twmode"))))

  (test-assert-string-equal "RT: %t (via @twmode)"
    (twindrill-format-string "RT: %t (via @%s)" "%"
			      '(("t" . "%t")
				("s" . "twmode"))))

  (test-assert-string-equal "new\nline"
    (twindrill-format-string "new~%line" "~"
			      '(("%" . "\n"))))
  )

(defun test-restore-timeline-spec(spec-str spec normalized-spec)
  (let ((spec-from-str
	 (twindrill-string-to-timeline-spec spec-str)))
    (list
     (equal (twindrill-string-to-timeline-spec
	     (twindrill-timeline-spec-to-string spec))
	    normalized-spec)
     (equal normalized-spec spec-from-str))))

(defun test-exclude-if-function (status)
  (string-match "test" (cdr (assq 'text status))))

(defcase timeline-spec nil nil
  (test-assert-equal
   (test-restore-timeline-spec
    "(user+(user/mylist+(@))+:exclude-re/WORD/(USER2+:mentions))"
    '(merge (user "user")
	    (merge (list "user" "mylist")
		   (merge (mentions)))
	    (exclude-re "WORD" (merge (user "USER2")
				      (mentions))))
    '(merge (user "user")
	    (list "user" "mylist")
	    (mentions)
	    (exclude-re "WORD" (merge (user "USER2")
				      (mentions)))))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    "(user-A+~+((user-B))+(:exclude-re/R+/user-C+(:exclude-re/R3\\//USER-D+:public)))"
    '(merge (user "user-A")
	    (home)
	    (merge (user "user-B")
		   (exclude-re "R+" (user "user-C")))
	    (exclude-re "R3/" (user "USER-D"))
	    (public))
    '(merge (user "user-A")
	    (home)
	    (user "user-B")
	    (exclude-re "R+" (user "user-C"))
	    (exclude-re "R3/" (user "USER-D"))
	    (public)))
   '(t t))

  ;; Duplicates in a merge timeline are removed.
  (test-assert-equal
   (test-restore-timeline-spec
    "(ABC+user/mylist+user/mylist+ABC)"
    '(merge (user "ABC") (list "user" "mylist"))
    '(merge (user "ABC") (list "user" "mylist")))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":search/ABC/"
    '(search "ABC")
    '(search "ABC"))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    ":search/AB\\\\C\\/aaa\\\\/"
    '(search "AB\\C/aaa\\")
    '(search "AB\\C/aaa\\"))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":exclude-if/test-exclude-if-function/:home"
    '(exclude-if test-exclude-if-function (home))
    '(exclude-if test-exclude-if-function (home)))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":exclude-if/(lambda (x) t)/:home"
    '(exclude-if (lambda (x) t) (home))
    '(exclude-if (lambda (x) t) (home)))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":exclude-if/(lambda (tweet) (string-match \"test\\\\.\" (cdr (assq 'text tweet))))/@"
    '(exclude-if (lambda (tweet)
		   (string-match "test\\." (cdr (assq 'text tweet))))
		 (mentions))
    '(exclude-if (lambda (tweet)
		   (string-match "test\\." (cdr (assq 'text tweet))))
		 (mentions)))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":exclude-if/(lambda (tweet) (string-match \"\\\\\\\\\" (cdr (assq 'text tweet))))/:exclude-if/(lambda (tw) (assq 'retweeting-id tw))/user/mylist"
    '(exclude-if (lambda (tweet)
		   (string-match "\\\\" (cdr (assq 'text tweet))))
		 (exclude-if (lambda (tw) (assq 'retweeting-id tw))
			     (list "user" "mylist")))
    '(exclude-if (lambda (tweet)
		   (string-match "\\\\" (cdr (assq 'text tweet))))
		 (exclude-if (lambda (tw) (assq 'retweeting-id tw))
			     (list "user" "mylist"))))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":exclude-re/ABC/user"
    '(exclude-re "ABC" (user "user"))
    '(exclude-re "ABC" (user "user")))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    ":exclude-re/ABC/user/mylist"
    '(exclude-re "ABC" (list "user" "mylist"))
    '(exclude-re "ABC" (list "user" "mylist")))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    ":exclude-re/ABC\\\\/user/mylist"
    '(exclude-re "ABC\\\\" (list "user" "mylist"))
    '(exclude-re "ABC\\\\" (list "user" "mylist")))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    ":exclude-re/ABC\\/\\(word\\|123\\)/user"
    '(exclude-re "ABC/\\(word\\|123\\)" (user "user"))
    '(exclude-re "ABC/\\(word\\|123\\)" (user "user")))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":retweeted_by_me" '(retweeted_by_me)  '(retweeted_by_me))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    ":retweeted_to_me" '(retweeted_to_me) '(retweeted_to_me))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    ":retweets_of_me" '(retweets_of_me)  '(retweets_of_me))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":favorites" '(favorites) '(favorites))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":favorites/USER" '(favorites "USER") '(favorites "USER"))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    "#tag" '(search "#tag") '(search "#tag"))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ;; "リスト" consists of U+256A, U+2539 and U+2548.
    ;; They should be supported by raw Emacs21.
    "USER/non-ASCIIリスト"
    '(list "USER" "non-ASCIIリスト")
    '(list "USER" "non-ASCIIリスト"))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ;; An example from
    ;; https://dev.twitter.com/docs/api/1/get/statuses/home_timeline ,
    ;; which is posted by cindy li.
    ":single/18700887835"
    '(single "18700887835")
    '(single "18700887835"))
   '(t t))
  )

(defcase timeline-spec-dependence nil nil
  ;; Duplicates in a merge timeline are removed.
  (test-assert-equal
   (let ((spec (twindrill-string-to-timeline-spec "(:home+@+(@+:home))")))
     (twindrill-get-primary-base-timeline-specs spec))
   '((home) (mentions)))

  (test-assert-equal
   (let ((spec (twindrill-string-to-timeline-spec
		"(:home+@+:exclude-if/(lambda (x) t)/(@+:home+USER))")))
     (twindrill-get-primary-base-timeline-specs spec))
   '((home) (mentions) (user "USER")))
  )

(defun format-status (status format-str)
  (twindrill-update-status-format format-str)
  (twindrill-format-status status))

(lexical-let ((status (car (get-fixture 'timeline-data))))
  (defcase test-format-status nil nil
    (test-assert-string-equal "hello world"
      (format-status status "hello world"))
    (test-assert-string-equal "%"
      (format-status status "%%"))



    (test-assert-string-equal "something like emacs"
      (format-status status "something like %s"))

    (test-assert-string-equal "We love emacs!"
      (format-status status "We love %S!"))

    (test-assert-string-equal
     ""
     (let ((twindrill-icon-mode nil))
       (format-status status "%i")))
    (test-assert-string-equal
     " "
     (let ((twindrill-icon-mode t)
	   (window-system t))
       (format-status status "%i")))

    (test-assert-string-equal
	"Emacs is the extensible self-documenting text editor."
      (format-status status "%d"))

    (test-assert-string-equal "GNU project"
      (format-status status "%l"))
    (test-assert-string-equal " [GNU project]"
      (format-status status "%L"))
    (setcdr (assoc 'user-location status) "")
    (test-assert-string-equal ""
      (format-status status "%l"))
    (test-assert-string-equal ""
      (format-status status "%L"))

    (setcdr (assoc 'in-reply-to-screen-name status) "hoge")
    (setcdr (assoc 'in-reply-to-status-id status) "123456")
    (test-assert-string-equal " in reply to hoge"
      (format-status status "%r"))
    (setcdr (assoc 'in-reply-to-screen-name status) "foo")
    (setcdr (assoc 'in-reply-to-status-id status) "")
    (test-assert-string-equal ""
      (format-status status "%r"))
    (setcdr (assoc 'in-reply-to-screen-name status) "")
    (setcdr (assoc 'in-reply-to-status-id status) "654321")
    (test-assert-string-equal ""
      (format-status status "%r"))
    (setcdr (assoc 'in-reply-to-screen-name status) "")
    (setcdr (assoc 'in-reply-to-status-id status) "")
    (test-assert-string-equal ""
      (format-status status "%r"))

    (test-assert-string-equal "http://www.gnu.org/software/emacs/"
      (format-status status "%u"))

    (test-assert-string-equal "9492852"
      (format-status status "%j"))

    (test-assert-string-equal ""
      (format-status status "%p"))
    (setcdr (assoc 'user-protected status) t)
    (test-assert-string-equal "[x]"
      (format-status status "%p"))
    (setcdr (assoc 'user-protected status) nil)
    (test-assert-string-equal ""
      (format-status status "%p"))

    (test-assert-string-equal
     "created at Wed Dec 09 00:44:57 +0000 2009"
     (let ((tz (current-time-zone)))
       (set-time-zone-rule t)
       (prog1
	   (format-status status "created at %c")
	 (set-time-zone-rule (cddr tz)))))

    (test-assert-string-equal
     "created at 2009/12/09 09:44:57"
     (let ((tz (current-time-zone)))
       (set-time-zone-rule "JST-9")
       (prog1
	   (format-status status "created at %C{%Y/%m/%d %H:%M:%S}")
	 (set-time-zone-rule (cddr tz)))))

    ;; (test-assert-string-equal "09:44 午前 12月 09, 2009"
    ;;   (format-status status "%@"))

    (test-assert-string-equal "Help protect and support Free Software and the GNU Project by joining the Free Software Foundation! http://www.fsf.org/join?referrer=7019"
      (format-status status "%T"))

    (setcdr (assoc 'truncated status) nil)
    (test-assert-string-equal ""
      (format-status status "%'"))
    (setcdr (assoc 'truncated status) t)
    (test-assert-string-equal "..."
      (format-status status "%'"))
    (setcdr (assoc 'truncated status) nil)

    (test-assert-string-equal "web"
      (format-status status "%f"))

    (test-assert-string-equal "6480639448"
      (format-status status "%#"))

    (test-assert-string-equal
     " emacs,  :
  Help protect and support Free Software and the GNU Project by joining the Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
     (let ((twindrill-icon-mode nil))
       (format-status status "%i %s,  :\n  %T // from %f%L%r")))
    (test-assert-string-equal
     "  emacs,  :
  Help protect and support Free Software and the GNU Project by joining the Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
     (let ((twindrill-icon-mode t)
	   (window-system t))
       (format-status status "%i %s,  :\n  %T // from %f%L%r")))

    (test-assert-string-equal
     "
  Help protect and support Free Software and the GNU Project by joining the
  Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
     (let ((twindrill-fill-column 80))
       (format-status status "\n%FILL[  ]{%T // from %f%L%r}")))

    (test-assert-string-equal
     "
  Help protect and support Free Software and the GNU Project by joining the
  Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
     (let ((twindrill-fill-column 80))
       (format-status status "\n%FOLD[  ]{%T // from %f%L%r}")))

    (test-assert-string-equal
     "
  Edit XHTML5 documents in nxml-mode with on-the-fly validation:
  http://bit.ly/lYnEg (by @hober) // from web"
     (let ((twindrill-fill-column 80)
	   (oldest-status (car (last (get-fixture 'timeline-data)))))
       (format-status oldest-status "\n%FILL[  ]{%T // from %f%L%r}")))

    (test-assert-string-equal
     "
--Edit XHTML5 documents in nxml-mode with on-the-fly validation:
--http://bit.ly/lYnEg
--			     
--	(by @hober) // from web"
     (let ((twindrill-fill-column 80)
	   (oldest-status (car (last (get-fixture 'timeline-data)))))
       (format-status oldest-status "\n%FOLD[--]{%T // from %f%L%r}")))
    ))

(defcase test-find-curl-program nil nil
  (test-assert-string-match "curl" (twindrill-find-curl-program))
  (with-temp-buffer
    (when (twindrill-find-curl-program)
      (test-assert-eq 0
	(call-process (twindrill-find-curl-program)
		      nil (current-buffer) nil
		      "--help")))))

(with-network
 (defcase test-ensure-ca-cert nil nil
   (when (twindrill-find-curl-program)
     (test-assert-eq 0
       (with-temp-buffer
	 (call-process (twindrill-find-curl-program)
		       nil (current-buffer) nil
		       "--cacert"
		       (twindrill-ensure-ca-cert)
		       "https://twitter.com/"))))))

(defcase test-status-not-blank-p nil nil
  (test-assert-ok (not (twindrill-status-not-blank-p "")))
  (test-assert-ok (not (twindrill-status-not-blank-p "\n")))
  (test-assert-ok (not (twindrill-status-not-blank-p "@foo")))
  (test-assert-ok (not (twindrill-status-not-blank-p "@bar ")))
  (test-assert-ok (twindrill-status-not-blank-p "hello"))
  (test-assert-ok (twindrill-status-not-blank-p "@baz hello"))
  (test-assert-ok (twindrill-status-not-blank-p "@baz\n\nhello"))
  (test-assert-ok (twindrill-status-not-blank-p "\nhello"))
  (test-assert-ok (twindrill-status-not-blank-p "hello\n"))
  (test-assert-ok (twindrill-status-not-blank-p "@foo hello @bar"))
  (test-assert-ok (twindrill-status-not-blank-p "hello @foo"))
  )

(defcase test-hmac-sha1 nil nil
  ;; The following tests are copied from RFC 2202.
  (test-assert-string-equal
   (let* ((v (make-list 20 ?\x0b))
	  (key (cond
		((fboundp 'unibyte-string) (apply 'unibyte-string v))
		(t (concat v))))
	  (data "Hi There"))
     (mapconcat (lambda (c) (format "%02x" c))
		(twindrill-hmac-sha1 key data)
		""))
   "b617318655057264e28bc0b6fb378c8ef146be00")

  (test-assert-string-equal
   (let* ((key "Jefe")
	  (data "what do ya want for nothing?"))
     (mapconcat (lambda (c) (format "%02x" c))
		(twindrill-hmac-sha1 key data)
		""))
   "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79")

  (test-assert-string-equal
   (let* ((key-v (make-list 20 ?\xaa))
	  (key (cond
		((fboundp 'unibyte-string) (apply 'unibyte-string key-v))
		(t (concat key-v))))
	  (data-v (make-list 50 ?\xdd))
	  (data (cond
		 ((fboundp 'unibyte-string) (apply 'unibyte-string data-v))
		 (t (concat data-v)))))
     (mapconcat (lambda (c) (format "%02x" c))
		(twindrill-hmac-sha1 key data)
		""))
   "125d7342b9ac11cd91a39af48aa17b4f63f175d3")
  )

(defun verify-hmac-of-long-data(coding-system)
  (or (not (coding-system-p coding-system))
      (let ((coding-system-for-read coding-system)
	    (coding-system-for-write coding-system))
	(let* ((key "key")
	       (data (make-string 64 ?a))
	       (result
		(mapcar
		 (lambda (pair)
		   (let ((n (car pair))
			 (expected (cdr pair)))
		     (string=
		      (mapconcat (lambda (c) (format "%02x" c))
				 (twindrill-hmac-sha1 key (make-string n ?a))
				 "")
		      expected)))
		 '((64 . "804f18b0143cc2677eeda7b5f60f6984fc32d094")
		   (128 . "2ff5eea1596f3a41fbb5e1ff02b17a50a229c177")
		   (256 . "ce7f0d472740f975b137052e05955b4ca7ec9a58")
		   (512 . "9dce54d150cbf817a8d979bd6a371eaf4643ac40")
		   (1024 . "53e18d494844a645cd8ac0757873460696b0c9df")
		   (2048 . "0fabada271421b303434131195e67c006d3fc3d7")
		   (4095 . "ef35e91ef97d811cfecfe83071045c31de4e4e61")
		   (8193 . "cefba76fb9ad5a665887a875ed7bc42ec9f00919")
		   (16383 . "c6ba93118262ab9426760909fa4981558bdc10e5")
		   (32767 . "442813d73d237fb45fca6c05360c43db877744f6")))))
	  (if (equal result '(t t t t t t t t t t))
	      t
	    result)))))

(defcase test-hmac-sha1-long nil nil
  (test-assert-eq (verify-hmac-of-long-data 'iso-safe) t)
  (test-assert-eq (verify-hmac-of-long-data 'iso-safe-unix) t)
  (test-assert-eq (verify-hmac-of-long-data 'iso-safe-dos) t)
  (test-assert-eq (verify-hmac-of-long-data 'iso-safe-mac) t)
  (test-assert-eq (verify-hmac-of-long-data 'utf-8) t)
  (test-assert-eq (verify-hmac-of-long-data 'utf-8-unix) t)
  (test-assert-eq (verify-hmac-of-long-data 'utf-8-dos) t)
  (test-assert-eq (verify-hmac-of-long-data 'utf-8-mac) t)
  (test-assert-eq (verify-hmac-of-long-data 'utf-16) t)
  (test-assert-eq (verify-hmac-of-long-data 'utf-16-unix) t)
  (test-assert-eq (verify-hmac-of-long-data 'utf-16-dos) t)
  (test-assert-eq (verify-hmac-of-long-data 'utf-16-mac) t)
  (test-assert-eq (verify-hmac-of-long-data 'iso-latin-1) t)
  (test-assert-eq (verify-hmac-of-long-data 'iso-latin-1-unix) t)
  (test-assert-eq (verify-hmac-of-long-data 'iso-latin-1-dos) t)
  (test-assert-eq (verify-hmac-of-long-data 'iso-latin-1-mac) t)
  )

(defun verify-hmac-with-short-internal-length (coding-system)
  (or (not (coding-system-p coding-system))
      (not (boundp 'sha1-maximum-internal-length))
      (let* ((coding-system-for-read coding-system)
	     (coding-system-for-write coding-system)
	     (max-internal-length-list '(1 10 50 100 500 1000))
	     (result
	      (mapcar
	       (lambda (n)
		 (let ((sha1-maximum-internal-length n)
		       (key "Jefe")
		       (data "what do ya want for nothing?")
		       (expected "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"))
		   (string= (mapconcat (lambda (c) (format "%02x" c))
				       (twindrill-hmac-sha1 key data)
				       "")
			    expected)))
	       max-internal-length-list)))
	(if (equal result (make-list (length max-internal-length-list) t))
	    t
	  result))))

(defcase test-hmac-sha1-with-short-internal-length nil nil
  (test-assert-eq (verify-hmac-with-short-internal-length 'iso-safe) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'iso-safe) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'iso-safe-unix) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'iso-safe-dos) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'iso-safe-mac) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'utf-8) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'utf-8-unix) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'utf-8-dos) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'utf-8-mac) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'utf-16) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'utf-16-unix) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'utf-16-dos) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'utf-16-mac) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'iso-latin-1) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'iso-latin-1-unix) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'iso-latin-1-dos) t)
  (test-assert-eq (verify-hmac-with-short-internal-length 'iso-latin-1-mac) t)
  )

(defcase test-oauth nil nil
  ;; "Authenticating Requests | dev.twitter.com"
  ;; http://dev.twitter.com/pages/auth
  (setq sample-consumer-key "GDdmIQH6jhtmLUypg82g")
  (setq sample-consumer-secret "MCD8BKwGdgPHvAuvgvz4EQpqDAtx89grbuNMRd7Eh98")

  ;; Acquiring a request token
  ;; http://dev.twitter.com/pages/auth#request-token
  (test-assert-string-equal
   (let* ((oauth-params
	   `(("oauth_nonce" . "QP70eNmVz8jvdPevU3oJD2AfF7R7odC2XJcn4XlZJqk")
	     ("oauth_callback" . ,(twindrill-oauth-url-encode "http://localhost:3005/the_dance/process_callback?service_provider_id=11"))
	     ("oauth_signature_method" . "HMAC-SHA1")
	     ("oauth_timestamp" . "1272323042")
	     ("oauth_consumer_key" . ,sample-consumer-key)
	     ("oauth_version" . "1.0")))
	  (url "https://api.twitter.com/oauth/request_token"))
     (twindrill-oauth-auth-str-request-token
      url nil sample-consumer-key sample-consumer-secret oauth-params))
   "OAuth oauth_nonce=\"QP70eNmVz8jvdPevU3oJD2AfF7R7odC2XJcn4XlZJqk\",oauth_callback=\"http%3A%2F%2Flocalhost%3A3005%2Fthe_dance%2Fprocess_callback%3Fservice_provider_id%3D11\",oauth_signature_method=\"HMAC-SHA1\",oauth_timestamp=\"1272323042\",oauth_consumer_key=\"GDdmIQH6jhtmLUypg82g\",oauth_version=\"1.0\",oauth_signature=\"8wUi7m5HFQy76nowoCThusfgB%2BQ%3D\"")

  ;; response
  (test-assert-equal
   (let ((response-str "oauth_token=8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc&oauth_token_secret=x6qpRnlEmW9JbQn4PQVVeVG8ZLPEx6A0TOebgwcuA&oauth_callback_confirmed=true"))
     (twindrill-oauth-make-response-alist response-str))
   '(("oauth_token" . "8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc")
     ("oauth_token_secret"
      . "x6qpRnlEmW9JbQn4PQVVeVG8ZLPEx6A0TOebgwcuA")
     ("oauth_callback_confirmed" . "true")))

  ;; Sending the user to authorization
  ;; http://dev.twitter.com/pages/auth#authorization
  ;; response (when using callback)
  (test-assert-equal
   (let ((response-str "oauth_token=8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc&oauth_verifier=pDNg57prOHapMbhv25RNf75lVRd6JDsni1AJJIDYoTY"))
     (twindrill-oauth-make-response-alist response-str))
   '(("oauth_token" . "8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc")
     ("oauth_verifier"
      . "pDNg57prOHapMbhv25RNf75lVRd6JDsni1AJJIDYoTY")))

  ;; Exchanging a request token for an access token
  ;; http://dev.twitter.com/pages/auth#access-token
  (test-assert-string-equal
   (let* ((request-token "8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc")
	  (request-token-secret "x6qpRnlEmW9JbQn4PQVVeVG8ZLPEx6A0TOebgwcuA")
	  (verifier "pDNg57prOHapMbhv25RNf75lVRd6JDsni1AJJIDYoTY")
	  (oauth-params
	   `(("oauth_nonce" . "9zWH6qe0qG7Lc1telCn7FhUbLyVdjEaL3MO5uHxn8")
	     ("oauth_signature_method" . "HMAC-SHA1")
	     ("oauth_timestamp" . "1272323047")
	     ("oauth_consumer_key" . ,sample-consumer-key)
	     ("oauth_token" . ,request-token)
	     ("oauth_verifier" . ,verifier)
	     ("oauth_version" . "1.0")))
	  (url "https://api.twitter.com/oauth/access_token"))
     (twindrill-oauth-auth-str-exchange-token
      url nil
      sample-consumer-key sample-consumer-secret
      request-token request-token-secret verifier oauth-params))
   "OAuth oauth_nonce=\"9zWH6qe0qG7Lc1telCn7FhUbLyVdjEaL3MO5uHxn8\",oauth_signature_method=\"HMAC-SHA1\",oauth_timestamp=\"1272323047\",oauth_consumer_key=\"GDdmIQH6jhtmLUypg82g\",oauth_token=\"8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc\",oauth_verifier=\"pDNg57prOHapMbhv25RNf75lVRd6JDsni1AJJIDYoTY\",oauth_version=\"1.0\",oauth_signature=\"PUw%2FdHA4fnlJYM6RhXk5IU%2F0fCc%3D\"")

  ;; response
  (test-assert-equal
   (let ((response-str "oauth_token=819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw&oauth_token_secret=J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA&user_id=819797&screen_name=episod"))
     (twindrill-oauth-make-response-alist response-str))
   '(("oauth_token"
      . "819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw")
     ("oauth_token_secret"
      . "J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA")
     ("user_id" . "819797")
     ("screen_name" . "episod")))

  ;; Making a resource request on a user's behalf
  ;; http://dev.twitter.com/pages/auth#auth-request
  (test-assert-string-equal
   (let* ((access-token "819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw")
	  (access-token-secret "J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA")
	  (oauth-params
	   `(("oauth_nonce" . "oElnnMTQIZvqvlfXM56aBLAf5noGD0AQR3Fmi7Q6Y")
	     ("oauth_signature_method" . "HMAC-SHA1")
	     ("oauth_timestamp" . "1272325550")
	     ("oauth_consumer_key" . ,sample-consumer-key)
	     ("oauth_token" . ,access-token)
	     ("oauth_version" . "1.0")))
	  (url "http://api.twitter.com/1/statuses/update.json")
	  (encoded-query-parameters
	   `((,(twindrill-percent-encode "status")
	      . ,(twindrill-percent-encode
		  "setting up my twitter 私のさえずりを設定する")))))
     (twindrill-oauth-auth-str-access
      "POST" url encoded-query-parameters
      sample-consumer-key sample-consumer-secret
      access-token access-token-secret
      oauth-params))
   "OAuth oauth_nonce=\"oElnnMTQIZvqvlfXM56aBLAf5noGD0AQR3Fmi7Q6Y\",oauth_signature_method=\"HMAC-SHA1\",oauth_timestamp=\"1272325550\",oauth_consumer_key=\"GDdmIQH6jhtmLUypg82g\",oauth_token=\"819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw\",oauth_version=\"1.0\",oauth_signature=\"yOahq5m0YjDDjfjxHaXEsW9D%2BX0%3D\"")
  )

(defun find-least-unsupported-code-point (from to)
  (let ((n from)
        (c nil))
    (while (and (<= n to)
		(decode-char 'ucs n))
      (setq n (1+ n)))
    (when (<= n to)
      n)))

(defun encode-char-for-json (code-point)
  ;; According to RFC4627 http://www.ietf.org/rfc/rfc4627 ,
  ;; characters in the Basic Multilingual Plane (U+0000
  ;; through U+FFFF) are escaped as
  ;; "a six-character sequence: a reverse solidus, followed
  ;;  by the lowercase letter u, followed by four hexadecimal
  ;;  digits that encode the character's code point."
  ;; "To escape an extended character that is not in the Basic
  ;;  Multilingual Plane, the character is represented as a
  ;;  twelve-character sequence, encoding the UTF-16 surrogate
  ;;  pair."
  (cond
   ((< code-point #xFFFF)
    (format "\\u%04X" code-point))
   (t
    ;; a character not in the Basic Multilingual Plane
    (let* ((c (decode-char 'ucs unsupported-code-point))
	   (str (encode-coding-char c 'utf-16be)))
      (apply 'format "\\u%02X%02X\\u%02X%02X"
	     (string-to-list str))))))

(defun test-read-json-with-unsupported-code-point ()
  (let ((unsupported-code-point
	 (or (find-least-unsupported-code-point #x0000 #xD7FF)
	     (find-least-unsupported-code-point #xE000 #x10FFFF))))
    (cond
     ((null unsupported-code-point)
      t)
     (t
      (let* ((str (encode-char-for-json unsupported-code-point))
	     (quoted-str (concat "\"" str "\""))
	     (result (with-temp-buffer
		       (insert quoted-str)
		       (goto-char (point-min))
		       (condition-case err
			   (twindrill-json-read)
			 (error
			  nil)))))
	(if (and result
		 (stringp result)
		 (string= result
			  (string twindrill-unicode-replacement-char)))
	    t
	  (format "failed to decode %s!" quoted-str)))))))

(when (require 'json nil t)
  (defcase test-json-read nil nil
    (test-assert-eq t (test-read-json-with-unsupported-code-point))))

(defun test-read-xml-with-unsupported-code-point ()
  (let ((unsupported-code-point
	 (or (find-least-unsupported-code-point #x0000 #xD7FF)
	     (find-least-unsupported-code-point #xE000 #x10FFFF))))
    (cond
     ((null unsupported-code-point)
      t)
     (t
      (let* ((xml-template
	      ;; example from http://www.w3.org/TR/xhtml1/#strict
	      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
  <head>
    <title>test</title>
  </head>
  <body>
    <p>%s</p>
  </body>
</html>")
	     (encoded-str (format "&#%d;" unsupported-code-point))
	     (xml-str (format xml-template encoded-str))
	     (replacement-str
	      (string twindrill-unicode-replacement-char))
	     (result
	      (with-temp-buffer
		(insert xml-str)
		(condition-case err
		    (let* ((xml
			    (twindrill-xml-parse-region (point-min)
							 (point-max)))
			   (str
			    (elt (assq 'p (assq 'body (assq 'html xml))) 2)))
                      str)
		  (error
		   nil))))
	     (expected-result replacement-str))
	(cond
	 ((null result)
	  (format "failed to decode %s with an error" encoded-str))
	 ((equal result expected-result)
	  t)
	 (t
	  (format "failed to decode %s without errors" encoded-str))))))))

(when (require 'xml nil t)
  (defcase test-xml-parse nil nil
    (test-assert-eq t (test-read-xml-with-unsupported-code-point))))
