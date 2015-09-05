;;; twindrill-storage.el --- Local storage for twindrill-mode

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

(provide 'twindrill-storage)
;;; twindrill-storage.el ends here
