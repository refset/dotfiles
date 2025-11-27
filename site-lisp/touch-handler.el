;;; touch-handler.el --- Touch screen events handler  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright 2024-present Naheel Azawy.  All rights reserved.

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

;; Author: Naheel Azawy
;; Version: 1.0.0
;; Keywords: touchscreen
;; URL: https://github.com/Naheel-Azawy/touch-handler.el
;;
;; This file is not part of GNU Emacs.
;;; Code:

(require 'scrollbar-scroll)

(defvar touchscreen-last-time)
(defvar touchscreen-last-pos-pixel)
(defvar touchscreen-last-dist 0)
(defvar touchscreen-begin-char)
(defvar touchscreen-momentum 0.0)
(defvar touchscreen-momentum-decay 0.92)
(defvar touchscreen-momentum-timer nil)
(defvar touchscreen-last-velocity 0.0)
(defvar touchscreen-scroll-window nil)

(defun touchscreen-time ()
  "Time in seconds."
  (float-time))

(defun touchscreen-momentum-tick ()
  "Apply momentum scrolling."
  (when (> (abs touchscreen-momentum) 1.0)
    (scrollbar-scroll--do-scroll touchscreen-scroll-window touchscreen-momentum)
    (setq touchscreen-momentum (* touchscreen-momentum touchscreen-momentum-decay)))
  (when (<= (abs touchscreen-momentum) 1.0)
    (when touchscreen-momentum-timer
      (cancel-timer touchscreen-momentum-timer)
      (setq touchscreen-momentum-timer nil))))

(defun touchscreen-handle-touch-begin (input)
  "Handle touch begining at input INPUT."
  (interactive "e")
  (let* ((event     (nth 1 input))
         (pos-pixel (nth 3 event))
         (pos-char  (nth 6 event))
         (win       (nth 1 event)))
    ;; Cancel any existing momentum
    (when touchscreen-momentum-timer
      (cancel-timer touchscreen-momentum-timer)
      (setq touchscreen-momentum-timer nil))
    (setq touchscreen-momentum 0.0)
    (setq touchscreen-last-velocity 0.0)
    (if (not (equal (selected-window) win))
        (select-window win))
    (setq touchscreen-scroll-window win)
    (setq touchscreen-last-time (touchscreen-time))
    (setq touchscreen-last-pos-pixel pos-pixel)
    (setq touchscreen-begin-char pos-char)))

(defun touchscreen-handle-touch-update (input)
  "Handle touch update at input INPUT."
  (interactive "e")
  (let* ((event      (nth 0 (nth 1 input)))
         (pos-pixel  (nth 3 event))
         (pos-char   (nth 6 event))
         (diff-time  (- (touchscreen-time) touchscreen-last-time))
         (diff-pixel (- (cdr touchscreen-last-pos-pixel) (cdr pos-pixel)))
         (diff-char  (abs (- touchscreen-begin-char pos-char))))

    (if (= (length (nth 1 input)) 2)
        ;; pinch zoom
        (let* ((event2     (nth 1 (nth 1 input)))
               (pos-pixel2 (nth 3 event2))
               (dist       (sqrt (+ (expt (- (car pos-pixel2) (car pos-pixel)) 2)
                                    (expt (- (cdr pos-pixel2) (cdr pos-pixel)) 2))))
               (dist-diff  (- dist touchscreen-last-dist)))
          (setq touchscreen-last-dist dist)
          (if (> dist-diff 0)
              (text-scale-increase 0.1)
            (if (< dist-diff 0)
                (text-scale-decrease 0.1))))

      (if (> diff-time 1)
          ;; TODO: set marker on long press
          (goto-char pos-char))
      (if (> diff-char 1)
          ;; scroll using scrollbar-scroll's approach
          (progn
            (scrollbar-scroll--do-scroll touchscreen-scroll-window diff-pixel)
            (setq touchscreen-last-velocity (float diff-pixel))
            (setq touchscreen-last-time (touchscreen-time))
            (setq touchscreen-last-pos-pixel pos-pixel))))))

(defun touchscreen-handle-touch-end (input)
  "Handle touch end at input INPUT."
  (interactive "e")
  (let* ((event    (nth 1 input))
         (pos-char (nth 6 event)))
    (if (= touchscreen-begin-char pos-char)
        ;; move cursor
        (goto-char pos-char)
      ;; start momentum scrolling
      (when (> (abs touchscreen-last-velocity) 5.0)
        (setq touchscreen-momentum touchscreen-last-velocity)
        (setq touchscreen-momentum-timer
              (run-at-time 0.016 0.016 #'touchscreen-momentum-tick))))))

(global-set-key [touchscreen-begin]  #'touchscreen-handle-touch-begin)
(global-set-key [touchscreen-update] #'touchscreen-handle-touch-update)
(global-set-key [touchscreen-end]    #'touchscreen-handle-touch-end)

(provide 'touch-handler)

;;; touch-handler.el ends here
