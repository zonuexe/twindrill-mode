;;; twindrill-plus.el --- Monkey patch for Twindrill mode

;; Copyright (C) 2015 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 08 Apr 2015
;; Version: 0.0.2
;; Keywords: twitter web
;; URL: https://github.com/zonuexe/twindrill-plus
;; Package-Requires: ((twindrill-mode "4.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; twindrill-plus.el is a monkey patch for twindrill-mode.
;; You can check friends timeline, and update your status on Emacs.
;;
;; To use Yorufukurou-like keybind, just add the following code into your .emacs:
;;
;;     (twindrill+tern-on-yorufukurou)
;;
;; Enjoy!

;;; Code:

;; Yorufukurou(夜フクロウ) like key binds
;; https://sites.google.com/site/yorufukurou/

(defun twindrill+yorufukurou-map (keymap)
  "Make Yorufukurou-like `KEYMAP' for twindrill-mode."
  (define-key keymap (kbd "f")       'twindrill-favorite)
  (define-key keymap (kbd "d")       'twindrill-direct-message)
  (define-key keymap (kbd "h")       'twindrill-user-timeline)
  (define-key keymap (kbd "<RET>")   'twindrill-other-user-timeline)
  (define-key keymap (kbd "<up>")    'twindrill-goto-previous-status)
  (define-key keymap (kbd "<down>")  'twindrill-goto-next-status)
  (define-key keymap (kbd "<right>") 'twindrill-enter)
  (define-key keymap (kbd "<left>")  'twindrill-toggle-show-replied-statuses)
  keymap)

(defun twindrill+yorufukurou-hook ()
  "Set Yorufukurou-like key binding to twindrill-mode."
  (if (eq major-mode 'twindrill-mode)
      (twindrill+yorufukurou-map (current-local-map))
    (error "Current major-mode is not `twindrill-mode'")))

;;;###autoload
(defun twindrill+tern-on-yorufukurou ()
  "Turn on Yorufukurou-like key binding."
  (if (boundp 'twindrill-mode-hook)
      (add-to-list 'twindrill-mode-hook 'twindrill+yorufukurou-hook)
    (error "`twindrill-mode' not found"))
  t)

(provide 'twindrill-plus)
;;; twindrill-plus.el ends here
