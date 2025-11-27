;;; scrollbar-scroll.el --- Scroll like dragging the scrollbar -*- lexical-binding: t; -*-

;;; Commentary:
;; Translates scroll wheel events into proportional buffer position jumps,
;; mimicking GTK scrollbar drag behavior for smoother scrolling.

;;; Code:

(defvar scrollbar-scroll-sensitivity 1.0
  "Multiplier for scroll amount. Higher = faster scrolling.")

(defvar scrollbar-scroll-throttle 0.016
  "Minimum time in seconds between scroll updates. 0.016 = ~60fps.")

(defvar scrollbar-scroll--last-time 0
  "Time of last scroll event processed.")

(defvar scrollbar-scroll--accumulated-delta 0.0
  "Accumulated scroll delta between throttled updates.")

(defvar scrollbar-scroll--point-before-scroll nil
  "Position of point before scrolling started.")

(defvar scrollbar-scroll--end-scroll-timer nil
  "Timer for detecting end of scroll.")

(defvar scrollbar-scroll-end-delay 0.3
  "Delay in seconds to detect end of scrolling.")

(defvar scrollbar-scroll--scroll-window nil
  "Window being scrolled.")

(defun scrollbar-scroll--do-scroll (window delta)
  "Scroll WINDOW by DELTA pixels, with throttling.
Positive DELTA scrolls down, negative scrolls up.
Uses the same technique as scroll-bar-drag for smooth scrolling."
  (let ((now (float-time)))
    ;; Cancel any pending end-scroll timer
    (when scrollbar-scroll--end-scroll-timer
      (cancel-timer scrollbar-scroll--end-scroll-timer)
      (setq scrollbar-scroll--end-scroll-timer nil))
    ;; Track window being scrolled
    (setq scrollbar-scroll--scroll-window window)
    ;; Ignore scroll-down deltas if end of buffer is already visible
    (when (and (> delta 0)
               (with-current-buffer (window-buffer window)
                 (pos-visible-in-window-p (point-max) window)))
      (setq delta 0))
    ;; Accumulate delta
    (setq scrollbar-scroll--accumulated-delta
          (+ scrollbar-scroll--accumulated-delta delta))
    ;; Only process if enough time has passed
    (when (> (- now scrollbar-scroll--last-time) scrollbar-scroll-throttle)
      (let* ((line-height (float (frame-char-height)))
             (raw-lines (/ scrollbar-scroll--accumulated-delta line-height))
             (scroll-lines (if (>= raw-lines 0)
                               (ceiling raw-lines)
                             (floor raw-lines))))
        (when (not (zerop scroll-lines))
          (with-current-buffer (window-buffer window)
            ;; Save point before scroll (like scroll-bar-drag does)
            (unless scrollbar-scroll--point-before-scroll
              (setq scrollbar-scroll--point-before-scroll (point)))
            ;; Scroll by adjusting window-start directly
            (condition-case nil
                (let* ((current-start (window-start window))
                       (new-start (save-excursion
                                    (goto-char current-start)
                                    (forward-line scroll-lines)
                                    (point)))
                       (moved (not (= new-start current-start))))
                  (when moved
                    (set-window-start window new-start t)
                    ;; If original point is now out of view, move to center
                    (when (and scrollbar-scroll--point-before-scroll
                               (not (pos-visible-in-window-p scrollbar-scroll--point-before-scroll window)))
                      (with-selected-window window
                        (move-to-window-line nil))
                      (setq scrollbar-scroll--point-before-scroll (point)))))
              (error nil))
            (setq scrollbar-scroll--accumulated-delta 0.0)))
        (setq scrollbar-scroll--last-time now)))
    ;; Schedule end-scroll to finalize
    (setq scrollbar-scroll--end-scroll-timer
          (run-at-time scrollbar-scroll-end-delay nil
                       #'scrollbar-scroll--end-scroll))))

(defun scrollbar-scroll--end-scroll ()
  "Called when scrolling ends."
  (setq scrollbar-scroll--end-scroll-timer nil)
  (setq scrollbar-scroll--point-before-scroll nil)
  (setq scrollbar-scroll--scroll-window nil))

(defun scrollbar-scroll--handler (event)
  "Handle scroll EVENT by jumping to buffer position like scrollbar drag."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (delta-pair (car (last event)))
         (delta (if (consp delta-pair) (cdr delta-pair) 0)))
    (scrollbar-scroll--do-scroll window (- delta))))

(define-minor-mode scrollbar-scroll-mode
  "Minor mode that scrolls by setting window position like scrollbar drag."
  :global t
  :lighter " SBS"
  (if scrollbar-scroll-mode
      (progn
        (global-set-key [wheel-up] #'scrollbar-scroll--handler)
        (global-set-key [wheel-down] #'scrollbar-scroll--handler)
        (global-set-key [double-wheel-up] #'scrollbar-scroll--handler)
        (global-set-key [double-wheel-down] #'scrollbar-scroll--handler)
        (global-set-key [triple-wheel-up] #'scrollbar-scroll--handler)
        (global-set-key [triple-wheel-down] #'scrollbar-scroll--handler))
    (global-unset-key [wheel-up])
    (global-unset-key [wheel-down])
    (global-unset-key [double-wheel-up])
    (global-unset-key [double-wheel-down])
    (global-unset-key [triple-wheel-up])
    (global-unset-key [triple-wheel-down])))

(provide 'scrollbar-scroll)
;;; scrollbar-scroll.el ends here
