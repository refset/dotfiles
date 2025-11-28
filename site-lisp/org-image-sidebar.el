;;; org-image-sidebar.el --- Display images from org buffer in a sidebar -*- lexical-binding: t; -*-

(defvar-local org-image-sidebar--buffer nil)
(defvar-local org-image-sidebar--source-buffer nil)
(defvar org-image-sidebar-width 0.4)
(defvar org-image-sidebar-extensions '("svg" "png" "jpg" "jpeg" "gif" "webp" "bmp" "tiff" "tif"))

(defun org-image-sidebar--extension-regexp ()
  "Return regexp matching image file extensions."
  (concat "\\.\\(" (mapconcat #'identity org-image-sidebar-extensions "\\|") "\\)"))

(defun org-image-sidebar--buffer-has-images-p ()
  "Return non-nil if current buffer contains image file references."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "\\[\\[file:[^]]+\\(" (org-image-sidebar--extension-regexp) "\\)\\]") nil t)))

(defun org-image-sidebar--get-visible-images ()
  "Get list of image file links visible in current window."
  (let ((start (window-start))
        (end (window-end nil t))
        (ext-re (org-image-sidebar--extension-regexp))
        images)
    (save-excursion
      (goto-char start)
      (while (re-search-forward (concat "\\[\\[file:\\([^]]+\\)" ext-re "\\]") end t)
        (push (concat (match-string 1) (match-string 2)) images))
      (goto-char start)
      (while (re-search-forward (concat "^#\\+RESULTS:.*\n\\[\\[file:\\([^]]+\\)" ext-re "\\]") end t)
        (let ((path (concat (match-string 1) (match-string 2))))
          (unless (member path images)
            (push path images)))))
    (nreverse images)))

(defun org-image-sidebar--expand-path (path)
  "Expand PATH relative to current buffer's directory."
  (if (file-name-absolute-p path)
      path
    (expand-file-name path (file-name-directory (or buffer-file-name default-directory)))))

(defun org-image-sidebar--image-type (path)
  "Determine image type from PATH extension."
  (let ((ext (downcase (file-name-extension path))))
    (cond
     ((string= ext "svg") 'svg)
     ((member ext '("jpg" "jpeg")) 'jpeg)
     ((string= ext "png") 'png)
     ((string= ext "gif") 'gif)
     ((string= ext "webp") 'webp)
     ((member ext '("tiff" "tif")) 'tiff)
     ((string= ext "bmp") 'bmp)
     (t nil))))

(defun org-image-sidebar--update ()
  "Update the sidebar with images visible in the source buffer."
  (when (and org-image-sidebar--buffer
             (buffer-live-p org-image-sidebar--buffer)
             (get-buffer-window org-image-sidebar--buffer))
    (let ((images (org-image-sidebar--get-visible-images))
          (sidebar-buf org-image-sidebar--buffer)
          (sidebar-win (get-buffer-window org-image-sidebar--buffer)))
      (with-current-buffer sidebar-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if (null images)
              (insert (propertize "No images in view" 'face 'font-lock-comment-face))
            (dolist (img-path images)
              (let ((full-path (org-image-sidebar--expand-path img-path)))
                (insert (propertize (file-name-nondirectory img-path)
                                    'face 'font-lock-keyword-face)
                        "\n")
                (if (file-exists-p full-path)
                    (let* ((win-width (window-body-width sidebar-win t))
                           (img-type (org-image-sidebar--image-type full-path))
                           (img (create-image full-path img-type nil
                                              :max-width (- win-width 20))))
                      (insert-image img)
                      (insert "\n\n"))
                  (insert (propertize "  [file not found]\n\n" 'face 'error)))))))
        (goto-char (point-min))))))

(defun org-image-sidebar--scroll-hook (&rest _)
  "Hook to update sidebar on scroll."
  (when org-image-sidebar-mode
    (org-image-sidebar--update)))

(defun org-image-sidebar--create-buffer ()
  "Create or get the sidebar buffer."
  (let ((buf (get-buffer-create (format "*Images: %s*" (buffer-name)))))
    (with-current-buffer buf
      (special-mode)
      (setq org-image-sidebar--source-buffer (current-buffer)))
    buf))

(defun org-image-sidebar--show-sidebar ()
  "Show the sidebar window."
  (unless (and org-image-sidebar--buffer
               (get-buffer-window org-image-sidebar--buffer))
    (setq org-image-sidebar--buffer (org-image-sidebar--create-buffer))
    (let ((win (display-buffer-in-side-window
                org-image-sidebar--buffer
                `((side . right)
                  (window-width . ,org-image-sidebar-width)
                  (preserve-size . (t . nil))))))
      (set-window-dedicated-p win t))
    (org-image-sidebar--update)))

(defun org-image-sidebar--hide-sidebar ()
  "Hide and kill the sidebar window."
  (when (and org-image-sidebar--buffer (buffer-live-p org-image-sidebar--buffer))
    (let ((win (get-buffer-window org-image-sidebar--buffer)))
      (when win (delete-window win)))
    (kill-buffer org-image-sidebar--buffer))
  (setq org-image-sidebar--buffer nil))

(defun org-image-sidebar--window-buffer-change (&rest _)
  "Handle window/buffer changes to show/hide sidebar appropriately."
  (let ((buf (window-buffer (selected-window))))
    (if (and (buffer-local-value 'org-image-sidebar-mode buf)
             (with-current-buffer buf (org-image-sidebar--buffer-has-images-p)))
        (with-current-buffer buf
          (org-image-sidebar--show-sidebar))
      (dolist (b (buffer-list))
        (when (buffer-local-value 'org-image-sidebar-mode b)
          (with-current-buffer b
            (org-image-sidebar--hide-sidebar)))))))

(defun org-image-sidebar--setup ()
  "Set up the sidebar mode."
  (add-hook 'window-scroll-functions #'org-image-sidebar--scroll-hook nil t)
  (add-hook 'post-command-hook #'org-image-sidebar--scroll-hook nil t)
  (add-hook 'window-buffer-change-functions #'org-image-sidebar--window-buffer-change)
  (when (org-image-sidebar--buffer-has-images-p)
    (org-image-sidebar--show-sidebar)))

(defun org-image-sidebar--teardown ()
  "Remove the sidebar."
  (remove-hook 'window-scroll-functions #'org-image-sidebar--scroll-hook t)
  (remove-hook 'post-command-hook #'org-image-sidebar--scroll-hook t)
  (org-image-sidebar--hide-sidebar))

;;;###autoload
(define-minor-mode org-image-sidebar-mode
  "Minor mode to display images from org buffer in a sidebar."
  :lighter " Img"
  (if org-image-sidebar-mode
      (org-image-sidebar--setup)
    (org-image-sidebar--teardown)))

(provide 'org-image-sidebar)
;;; org-image-sidebar.el ends here
