;;; share2computer.el --- Elisp helper of android ShareToComputer -*- lexical-binding: t -*-

;; * Header
;; Copyright (c) 2020, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/share2computer
;; Keywords: convenience, comm
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * README                                                  :README:
;; share2computer is an elisp helper of android app: [[https://github.com/jimmod/ShareToComputer][Share to Computer]].
;; When files shared by android ShareToComputer, user can run Emacs command `share2computer'
;; in computer to download.

;;; Code:

;; * 代码                                                      :code:

;; ** Share to Computer
(require 'url)
(require 'json)

(defgroup share2computer nil
  "Elisp helper of android ShareToComputer."
  :group 'convenience)

(defcustom share2computer-urls nil
  "The possible download urls of Android app: ShareToComputer."
  :group 'share2computer
  :type '(repeat string))

(defcustom share2computer-default-directory "~/share2computer/"
  "The default download directory of share2computer."
  :group 'share2computer
  :type '(string))

(defcustom share2computer-finish-hook nil
  "Hook run when share2computer download finish.
hook function's argument is download directory."
  :group 'share2computer
  :type 'hook)

(defvar share2computer--file-number 0
  "The number of downloaded files.")
(defvar share2computer--buffers nil
  "The buffers created by `url-retrieve' when run share2computer.")
(defvar share2computer--timer1 nil
  "The timer used to cancel download.")
(defvar share2computer--timer2 nil
  "The timer used to show second of time when connect url.")

(defun share2computer--register (url buffer)
  "Register BUFFER to URL key of `share2computer--buffers'."
  (push buffer (alist-get url share2computer--buffers nil t 'equal)))

(defun share2computer--registered-p (url buffer)
  "Check BUFFER register status to URL key of `share2computer--buffers'."
  (member buffer (alist-get url share2computer--buffers nil t 'equal)))

(defun share2computer--kill (url &optional reverse)
  "Kill buffers and network processes related to URL.
When url is 'all, kill all buffers and network processes of
share2computer.  when REVERSE is non-nil, kill all except related
to URL."
  (let ((kill-buffer-query-functions nil)
        buffers result)
    (dolist (x share2computer--buffers)
      (let ((condi (or (equal url (car x))
                       (equal url 'all))))
        (if (if reverse (not condi) condi)
            (dolist (buff (cdr x))
              (when (buffer-live-p buff)
                (push buff buffers)))
          (push x result)))
      (setq share2computer--buffers result)
      ;; 必须先设置 share2computer--buffers 然后再删除 buffer
      (mapc #'kill-buffer buffers))))

(defun share2computer--write (status url link directory n &optional _retry-n)
  "Download file from LINK and write to DIRECTORY.
If multi files are shared,  N is the number of downloaded files.
If retry is need, RETRY-N is retry time..
Argument STATUS used by `url-retrieve'.
Argument URL is used to message."
  (let* ((err (plist-get status :error))
         (disposition
          (mail-fetch-field "Content-Disposition"))
         (filename
          (when disposition
            (replace-regexp-in-string
             file-name-invalid-regexp ""
             (replace-regexp-in-string
              ".*filename=\"\\(.*\\)\"$" "\\1"
              (decode-coding-string disposition 'utf-8))))))
    (if (and filename (not err))
        (let ((file (concat (file-name-as-directory directory) filename)))
          (delete-region
           (point-min)
           (progn
             (re-search-forward "\n\n" nil 'move)
             (point)))
          (let ((coding-system-for-write 'no-conversion))
            (write-region nil nil file))
          (setq share2computer--file-number
                (+ share2computer--file-number 1))
          (if (= share2computer--file-number n)
              (progn
                (message "share2computer: download finished from %S" url)
                (run-hook-with-args 'share2computer-finish-hook directory))
            (message "share2computer: download %s/%s files to %S ..."
                     share2computer--file-number n directory)))
      (share2computer--download-1 (current-buffer) url link directory n 1))))

(defun share2computer--download (_status url directory)
  "Download all files from ShareToComputer URL to DIRECTORY.
STATUS is use by `url-retrieve'."
  (let ((n (save-excursion
             (goto-char (point-min))
             (re-search-forward "\n\n" nil 'move)
             (ignore-errors
               (cdr (assoc 'total
                           (json-read-from-string
                            (buffer-substring (point) (point-max)))))))))
    (when (and (numberp n) (> n 0))
      (share2computer--kill url t)
      (share2computer--cancel-timer)
      (message "share2computer: start download ...")
      (dotimes (i n)
        (share2computer--download-1
         (current-buffer) url (format "%s%S" url i) directory n)))))

(defun share2computer--download-1 (buffer url link directory n &optional retry-n)
  "The internal function of `share2computer--download'.
Arguments BUFFER, URL, LINK, DIRECTORY, N and RETRY-N are
similar with `share2computer--write'."
  (if (and (numberp retry-n)
           (> retry-n 4))
      (message "share2computer: fail after retry download 3 times !!!")
    (share2computer--register
     url
     (url-retrieve
      link
      (lambda (status buffer url link directory n)
        (when (share2computer--registered-p url buffer)
          (share2computer--write status url link directory n)))
      (list buffer url link directory n)
      t t))
    (when (numberp retry-n)
      (message "share2computer: retry(%s) download file from %S ..." retry-n link)
      (setq retry-n (+ retry-n 1)))))

(defun share2computer--active-timers ()
  "Active timers which used to download cancel and progress."
  (let ((sec (string-to-number (format-time-string "%s"))))
    (share2computer--cancel-timer)
    (setq share2computer--timer1
          (run-with-timer
           4 nil
           (lambda ()
             (message "share2computer: cancel download for wait too long time.")
             (share2computer--cancel-timer)
             (share2computer--kill 'all))))
    (setq share2computer--timer2
          (run-with-timer
           nil 1
           `(lambda ()
              (message "share2computer: read info (%ss) ..."
                       (- (string-to-number (format-time-string "%s")) ,sec)))))))

(defun share2computer--cancel-timer ()
  "Cancel timers which used to download cancel and progress."
  (when share2computer--timer1
    (cancel-timer share2computer--timer1))
  (when share2computer--timer2
    (cancel-timer share2computer--timer2)))

(defun share2computer--internal (directory)
  "Internal function of share2computer.
Argument DIRECTORY ."
  (setq directory (expand-file-name (file-name-as-directory directory)))
  (setq share2computer--file-number 0)
  (make-directory directory t)
  (while (not (cl-some (lambda (x)
                         (> (length x) 0))
                       share2computer-urls))
    (share2computer-setup))
  (share2computer--kill 'all)
  (share2computer--active-timers)
  (dolist (url share2computer-urls)
    (let ((url (file-name-as-directory url)))
      (share2computer--register
       url (url-retrieve (concat url "info")
                         'share2computer--download
                         (list url directory)
                         t t)))))


;;;###autoload
(defun share2computer-kill ()
  "Kill all share2computer downloads."
  (interactive)
  (share2computer--kill 'all))

;;;###autoload
(defun share2computer ()
  "Download files shared by Android ShareToComputer."
  (interactive)
  (share2computer--internal share2computer-default-directory))

(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-attach-dir "org" (&optional create-if-not-exists-p no-fs-check))

;;;###autoload
(defun share2computer-org ()
  "Download files shared by Android ShareToComputer to org attach dir."
  (interactive)
  (require 'org)
  (let (marker)
    (when (eq major-mode 'org-agenda-mode)
      (setq marker (or (get-text-property (point) 'org-hd-marker)
		       (get-text-property (point) 'org-marker)))
      (unless marker
	(error "No task in current line")))
    (save-excursion
      (when marker
	(set-buffer (marker-buffer marker))
	(goto-char marker))
      (org-back-to-heading t)
      (share2computer--internal (org-attach-dir t)))))

(defun share2computer-setup ()
  "Setup share2computer."
  (interactive)
  (let ((status t))
    (while status
      (push (read-from-minibuffer "share2computer url: " "http://192.168.0.X:8080")
            share2computer-urls)
      (unless (y-or-n-p "Share2computer: adding another url? ")
        (setq status nil))))
  (setq share2computer-urls
        (delete-dups share2computer-urls))
  (customize-save-variable
   'share2computer-urls
   share2computer-urls))

;; * Footer
(provide 'share2computer)

;;; share2computer.el ends here
