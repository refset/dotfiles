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
(defvar touchscreen-momentum-decay 0.95)
(defvar touchscreen-momentum-multiplier 2.0
  "Multiplier applied to initial momentum velocity.")
(defvar touchscreen-momentum-timer nil)
(defvar touchscreen-last-velocity 0.0)
(defvar touchscreen-scroll-window nil)
(defvar touchscreen-did-scroll nil
  "Non-nil if we scrolled during this touch gesture.")

(defvar touchscreen--hl-line-was-on nil
  "Whether hl-line was on before scrolling.")

(defvar touchscreen--cursor-was-on nil
  "Whether cursor was visible before scrolling.")

(defvar touchscreen--saved-window-start nil
  "Saved window-start to restore between events.")

(defvar touchscreen--saved-vscroll nil
  "Saved vscroll to restore between events.")

(defvar touchscreen--clear-saved-timer nil
  "Timer to clear saved scroll position after delay.")

(defun touchscreen-time ()
  "Time in seconds."
  (float-time))

(defun touchscreen--clear-saved-position ()
  "Clear the saved scroll position."
  (setq touchscreen--saved-window-start nil)
  (setq touchscreen--saved-vscroll nil)
  (setq touchscreen--clear-saved-timer nil))

(defun touchscreen--restore-display ()
  "Restore cursor and hl-line after scrolling."
  (when touchscreen--hl-line-was-on
    (global-hl-line-mode 1)
    (setq touchscreen--hl-line-was-on nil))
  (when touchscreen--cursor-was-on
    (setq cursor-type touchscreen--cursor-was-on)
    (setq touchscreen--cursor-was-on nil))
  ;; Clear saved scroll position after a delay to allow consecutive touches
  (when touchscreen--clear-saved-timer
    (cancel-timer touchscreen--clear-saved-timer))
  (setq touchscreen--clear-saved-timer
        (run-at-time 0.5 nil #'touchscreen--clear-saved-position)))

(defun touchscreen-momentum-tick ()
  "Apply momentum scrolling with pixel precision."
  (when (> (abs touchscreen-momentum) 1.0)
    (let* ((win touchscreen-scroll-window)
           ;; Use saved position, not window state
           (current-vscroll (or touchscreen--saved-vscroll (window-vscroll win t)))
           (new-vscroll (+ current-vscroll touchscreen-momentum)))
      (with-current-buffer (window-buffer win)
        (let* ((line-height (line-pixel-height))
               (win-height (window-body-height win))
               ;; Allow scrolling until last line is ~20% from top
               (scroll-limit (truncate (* win-height 0.8)))
               (max-start (save-excursion
                            (goto-char (point-max))
                            (forward-line (- scroll-limit))
                            (point)))
               (current-start (or touchscreen--saved-window-start (window-start win)))
               (hit-boundary nil))
          ;; Handle scrolling down (positive vscroll)
          (while (>= new-vscroll line-height)
            (let ((next-start (save-excursion
                                (goto-char current-start)
                                (forward-line 1)
                                (point))))
              (if (or (= next-start current-start)
                      (> next-start max-start))
                  (progn
                    (setq new-vscroll 0)
                    (setq hit-boundary t))
                (setq current-start next-start)
                (setq new-vscroll (- new-vscroll line-height)))))
          ;; Handle scrolling up (negative vscroll)
          (while (< new-vscroll 0)
            (let ((prev-start (save-excursion
                                (goto-char current-start)
                                (forward-line -1)
                                (point))))
              (if (= prev-start current-start)
                  (progn
                    (setq new-vscroll 0)
                    (setq hit-boundary t))
                (setq current-start prev-start)
                (setq new-vscroll (+ new-vscroll line-height)))))
          ;; Move point to center (using pixels for zoom accuracy)
          (let* ((win-pixel-height (window-body-height win t))
                 (half-pixels (/ win-pixel-height 2))
                 (lines-to-center (truncate (/ half-pixels line-height)))
                 (target-point (save-excursion
                                 (goto-char current-start)
                                 (forward-line lines-to-center)
                                 (point))))
            (with-selected-window win
              (goto-char target-point)))
          (set-window-start win current-start t)
          (set-window-vscroll win new-vscroll t)
          ;; Update saved position for next tick or next touch
          (setq touchscreen--saved-window-start current-start)
          (setq touchscreen--saved-vscroll new-vscroll)
          ;; Stop momentum if we hit a boundary
          (when hit-boundary
            (setq touchscreen-momentum 0)))))
    (setq touchscreen-momentum (* touchscreen-momentum touchscreen-momentum-decay)))
  (when (<= (abs touchscreen-momentum) 1.0)
    (when touchscreen-momentum-timer
      (cancel-timer touchscreen-momentum-timer)
      (setq touchscreen-momentum-timer nil))
    (touchscreen--restore-display)))

(defun touchscreen-handle-touch-begin (input)
  "Handle touch begining at input INPUT."
  (interactive "e")
  (let* ((event     (nth 1 input))
         (pos-pixel (nth 3 event))
         (pos-char  (nth 6 event))
         (win       (nth 1 event)))
    ;; Cancel any existing momentum (but don't restore display - we're continuing to scroll)
    (when touchscreen-momentum-timer
      (cancel-timer touchscreen-momentum-timer)
      (setq touchscreen-momentum-timer nil))
    ;; Cancel any pending clear of saved position
    (when touchscreen--clear-saved-timer
      (cancel-timer touchscreen--clear-saved-timer)
      (setq touchscreen--clear-saved-timer nil))
    (setq touchscreen-momentum 0.0)
    (setq touchscreen-last-velocity 0.0)
    ;; Keep the saved display state if we're interrupting a scroll
    ;; (don't re-save nil cursor)
    (if (not (equal (selected-window) win))
        (select-window win))
    (setq touchscreen-scroll-window win)
    (setq touchscreen-last-time (touchscreen-time))
    (setq touchscreen-last-pos-pixel pos-pixel)
    (setq touchscreen-begin-char pos-char)
    (setq touchscreen-did-scroll nil)
    ;; If we have a saved scroll position, restore it (Emacs may have auto-scrolled)
    ;; Otherwise initialize from current window state
    (if (and touchscreen--saved-window-start touchscreen--saved-vscroll)
        (progn
          (set-window-start win touchscreen--saved-window-start t)
          (set-window-vscroll win touchscreen--saved-vscroll t))
      (setq touchscreen--saved-window-start (window-start win))
      (setq touchscreen--saved-vscroll (window-vscroll win t)))))

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
        ;; pinch zoom (coarse - requires 50px change)
        (let* ((event2     (nth 1 (nth 1 input)))
               (pos-pixel2 (nth 3 event2))
               (dist       (sqrt (+ (expt (- (car pos-pixel2) (car pos-pixel)) 2)
                                    (expt (- (cdr pos-pixel2) (cdr pos-pixel)) 2))))
               (dist-diff  (- dist touchscreen-last-dist)))
          (when (> (abs dist-diff) 50)
            (setq touchscreen-last-dist dist)
            (if (> dist-diff 0)
                (text-scale-increase 1)
              (text-scale-decrease 1))))

      (if (> diff-time 1)
          ;; TODO: set marker on long press
          (goto-char pos-char))
      (when (> diff-char 1)
        ;; scroll 1:1 with finger movement using pixel-level vscroll
        ;; Hide cursor and hl-line during scroll for performance
        (unless (or touchscreen-did-scroll touchscreen--cursor-was-on touchscreen--hl-line-was-on)
          (setq touchscreen--hl-line-was-on (bound-and-true-p global-hl-line-mode))
          (setq touchscreen--cursor-was-on cursor-type)
          (when touchscreen--hl-line-was-on
            (global-hl-line-mode -1))
          (setq cursor-type nil))
        ;; Use SAVED scroll position (not window state which Emacs may have changed)
        (let* ((max-diff 100)
               (clamped-diff (max (- max-diff) (min max-diff diff-pixel)))
               (win touchscreen-scroll-window)
               (current-vscroll touchscreen--saved-vscroll)
               (new-vscroll (+ current-vscroll clamped-diff)))
          (with-current-buffer (window-buffer win)
            (let* ((line-height (line-pixel-height))
                   (win-height (window-body-height win))
                   ;; Allow scrolling until last line is ~20% from top
                   (scroll-limit (truncate (* win-height 0.8)))
                   (max-start (save-excursion
                                (goto-char (point-max))
                                (forward-line (- scroll-limit))
                                (point)))
                   (current-start touchscreen--saved-window-start))
              ;; Handle scrolling down (positive vscroll)
              (while (>= new-vscroll line-height)
                (let ((next-start (save-excursion
                                    (goto-char current-start)
                                    (forward-line 1)
                                    (point))))
                  (if (or (= next-start current-start)
                          (> next-start max-start))
                      (setq new-vscroll 0)  ; hit bottom
                    (setq current-start next-start)
                    (setq new-vscroll (- new-vscroll line-height)))))
              ;; Handle scrolling up (negative vscroll)
              (while (< new-vscroll 0)
                (let ((prev-start (save-excursion
                                    (goto-char current-start)
                                    (forward-line -1)
                                    (point))))
                  (if (= prev-start current-start)
                      (setq new-vscroll 0)  ; hit top
                    (setq current-start prev-start)
                    (setq new-vscroll (+ new-vscroll line-height)))))
              ;; Set window position and save for next event
              (set-window-start win current-start t)
              (set-window-vscroll win new-vscroll t)
              (setq touchscreen--saved-window-start current-start)
              (setq touchscreen--saved-vscroll new-vscroll)
              ;; Move point to visible area to prevent Emacs from auto-scrolling
              (let* ((win-pixel-height (window-body-height win t))
                     (visible-center (/ win-pixel-height 2))
                     (lines-from-start (truncate (/ (+ new-vscroll visible-center) line-height)))
                     (target-point (save-excursion
                                     (goto-char current-start)
                                     (forward-line lines-from-start)
                                     (point))))
                (with-selected-window win
                  (goto-char target-point)))))
          (setq touchscreen-last-velocity (float diff-pixel)))
        (setq touchscreen-did-scroll t)
        (setq touchscreen-last-time (touchscreen-time))
        (setq touchscreen-last-pos-pixel pos-pixel)))))

(defun touchscreen-handle-touch-end (input)
  "Handle touch end at input INPUT."
  (interactive "e")
  (let* ((event    (nth 1 input))
         (pos-char (nth 6 event)))
    (if touchscreen-did-scroll
        ;; we scrolled - start momentum if we have velocity
        (if (> (abs touchscreen-last-velocity) 2.0)
            (progn
              (setq touchscreen-momentum (* touchscreen-last-velocity touchscreen-momentum-multiplier))
              (setq touchscreen-momentum-timer
                    (run-at-time 0.016 0.016 #'touchscreen-momentum-tick)))
          ;; no momentum, restore display now
          (touchscreen--restore-display))
      ;; tap - restore display (clears saved position), move cursor
      (touchscreen--restore-display)
      (goto-char pos-char))))

(global-set-key [touchscreen-begin]  #'touchscreen-handle-touch-begin)
(global-set-key [touchscreen-update] #'touchscreen-handle-touch-update)
(global-set-key [touchscreen-end]    #'touchscreen-handle-touch-end)

(provide 'touch-handler)

;;; touch-handler.el ends here
