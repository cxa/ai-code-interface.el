;;; test_ai-code-backends-infra-ghostel.el --- Ghostel lifecycle tests -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for Ghostel-specific lifecycle integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-backends-infra-ghostel)

(defvar ghostel-command-finish-functions)
(defvar ghostel-command-start-functions)
(defvar ghostel-progress-function)
(defvar ghostel-set-title-function)
(defvar ghostel-kill-buffer-on-exit)
(defvar ghostel-kitty-graphics-mediums)
(defvar ghostel--plain-link-detection-begin)
(defvar ghostel--plain-link-detection-end)
(defvar ai-code-backends-infra--session-directory)

(ert-deftest test-ai-code-backends-infra-ghostel-create-session-restores-ai-state ()
  "Ghostel session creation should restore AI Code local state after mode reset."
  (let* ((working-dir (file-name-as-directory
                       (expand-file-name default-directory)))
         (buffer-name " *ai-code-ghostel-reset-test*")
         (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-backends-infra--set-session-directory)
                   (lambda (target directory)
                     (with-current-buffer target
                       (setq-local ai-code-backends-infra--session-directory
                                   (file-name-as-directory
                                    (expand-file-name directory))))))
                  ((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                   (lambda () nil))
                  ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                   (lambda () nil))
                  ((symbol-function 'ghostel-exec)
                   (lambda (_buffer _program _args)
                     (kill-local-variable
                      'ai-code-backends-infra--session-terminal-backend)
                     (kill-local-variable
                      'ai-code-backends-infra--session-directory)
                     nil)))
          (setq buffer
                (car (ai-code-backends-infra-ghostel-create-session
                      buffer-name working-dir "codex" nil)))
          (with-current-buffer buffer
            (should (eq (and (boundp
                              'ai-code-backends-infra--session-terminal-backend)
                             ai-code-backends-infra--session-terminal-backend)
                        'ghostel))
            (should (equal ai-code-backends-infra--session-directory
                           working-dir))
            (should
             (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-configures-lifecycle-hooks ()
  "Ghostel AI session configuration should install lifecycle hooks locally."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (setq-local ghostel-command-start-functions nil)
    (setq-local ghostel-command-finish-functions nil)
    (setq-local ghostel-progress-function #'ignore)
    (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
               (lambda () nil))
              ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
               (lambda () nil)))
      (ai-code-backends-infra--configure-ghostel-buffer))
    (should (memq #'ai-code-backends-infra-ghostel--command-start
                  ghostel-command-start-functions))
    (should (memq #'ai-code-backends-infra-ghostel--command-finish
                  ghostel-command-finish-functions))
    (should (eq ghostel-progress-function
                #'ai-code-backends-infra-ghostel--progress))
    (should (eq ai-code-backends-infra-ghostel--progress-function
                #'ignore))))

(ert-deftest test-ai-code-backends-infra-ghostel-configures-visible-image-hook ()
  "Ghostel AI session configuration should install visible image recovery."
  (with-temp-buffer
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
               (lambda () nil))
              ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
               (lambda () nil)))
      (ai-code-backends-infra--configure-ghostel-buffer))
    (should (memq #'ai-code-backends-infra-ghostel--window-scroll
                  window-scroll-functions))))

(ert-deftest test-ai-code-backends-infra-ghostel-configures-image-mediums ()
  "Ghostel AI session configuration should enable local image mediums."
  (with-temp-buffer
    (let ((ai-code-backends-infra-ghostel-kitty-graphics-mediums
           '(file temp-file))
          (original-bound (boundp 'ghostel-kitty-graphics-mediums))
          (original-value (and (boundp 'ghostel-kitty-graphics-mediums)
                               ghostel-kitty-graphics-mediums)))
      (unwind-protect
          (progn
            (setq ghostel-kitty-graphics-mediums '(shared-mem))
            (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
            (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                       (lambda () nil))
                      ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                       (lambda () nil)))
              (ai-code-backends-infra--configure-ghostel-buffer))
            (should (equal ghostel-kitty-graphics-mediums
                           '(file temp-file shared-mem))))
        (if original-bound
            (setq ghostel-kitty-graphics-mediums original-value)
          (makunbound 'ghostel-kitty-graphics-mediums))))))

(ert-deftest test-ai-code-backends-infra-ghostel-remote-keeps-user-image-mediums ()
  "Remote Ghostel sessions should not add AI Code local image mediums."
  (with-temp-buffer
    (let ((ai-code-backends-infra-ghostel-kitty-graphics-mediums
           '(file temp-file))
          (original-bound (boundp 'ghostel-kitty-graphics-mediums))
          (original-value (and (boundp 'ghostel-kitty-graphics-mediums)
                               ghostel-kitty-graphics-mediums)))
      (unwind-protect
          (progn
            (setq ghostel-kitty-graphics-mediums '(shared-mem))
            (setq-local ai-code-backends-infra--session-directory
                        "/ssh:example:/repo/")
            (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
            (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                       (lambda () nil))
                      ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                       (lambda () nil)))
              (ai-code-backends-infra--configure-ghostel-buffer))
            (should (equal ghostel-kitty-graphics-mediums '(shared-mem))))
        (if original-bound
            (setq ghostel-kitty-graphics-mediums original-value)
          (makunbound 'ghostel-kitty-graphics-mediums))))))

(ert-deftest test-ai-code-backends-infra-ghostel-visible-linkify-restores-image-preview ()
  "Visible Ghostel image linkification should restore inline image previews."
  (let* ((root (make-temp-file "ai-code-ghostel-history-image-" t))
         (image-file (expand-file-name "history.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "Restored history\n")
              (insert "history.png\n")
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (should
               (= (length
                   (cl-remove-if-not
                    (lambda (overlay)
                      (overlay-get overlay 'ai-code-session-image-preview))
                    (overlays-in (point-min) (point-max))))
                  1)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest test-ai-code-backends-infra-ghostel-schedules-visible-linkify ()
  "Visible image linkification should schedule bounded window scans."
  (let (scheduled)
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (let ((ai-code-backends-infra-ghostel-visible-image-linkify-delays
             '(0.1 0.5)))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (delay _repeat function &rest args)
                     (push (list delay function args) scheduled)
                     'mock-timer)))
          (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
           (selected-window))
          (should (equal (mapcar #'car (reverse scheduled))
                          '(0.1 0.5))))))))

(ert-deftest test-ai-code-backends-infra-ghostel-schedules-visible-linkify-remotely ()
  "Visible linkification should still refresh URL links for remote sessions."
  (let (scheduled)
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (setq-local default-directory "/ssh:example:/repo/")
      (setq-local ai-code-backends-infra--session-directory default-directory)
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (let ((ai-code-backends-infra-ghostel-visible-image-linkify-delays
             '(0.1)))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (delay _repeat function &rest args)
                     (push (list delay function args) scheduled)
                     'mock-timer)))
          (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
           (selected-window))
          (should (= (length scheduled) 1))
          (should (equal (caar scheduled) 0.1)))))))

(ert-deftest test-ai-code-backends-infra-ghostel-visible-linkify-wraps-url ()
  "Visible Ghostel linkification should relinkify wrapped URLs."
  (with-temp-buffer
    (set-window-buffer (selected-window) (current-buffer))
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (insert "origin\n")
    (insert "https://example.com/repo/project-int   \n")
    (insert "erface.el\n")
    (insert "HEAD\n")
    (goto-char (point-min))
    (set-window-start (selected-window) (point-min) t)
    (ai-code-backends-infra-ghostel--linkify-visible-image-previews
     (current-buffer)
     (selected-window))
    (goto-char (point-min))
    (search-forward "https://example.com/repo/project-int")
    (let ((url "https://example.com/repo/project-interface.el"))
      (should (equal (get-text-property (match-beginning 0)
                                        'ai-code-session-link)
                     url))
      (search-forward "erface.el")
      (should (equal (get-text-property (match-beginning 0)
                                        'ai-code-session-link)
                     url)))))

(ert-deftest test-ai-code-backends-infra-ghostel-visible-linkify-remote-url ()
  "Visible Ghostel linkification should linkify URLs for remote sessions."
  (with-temp-buffer
    (set-window-buffer (selected-window) (current-buffer))
    (setq-local default-directory "/ssh:example:/repo/")
    (setq-local ai-code-backends-infra--session-directory default-directory)
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (insert "https://example.com/repo/project-int   \n")
    (insert "erface.el\n")
    (goto-char (point-min))
    (set-window-start (selected-window) (point-min) t)
    (cl-letf (((symbol-function
                'ai-code-session-link--linkify-strict-image-preview-region)
               (lambda (&rest _args)
                 (error "Strict image previews should be skipped remotely"))))
      (ai-code-backends-infra-ghostel--linkify-visible-image-previews
       (current-buffer)
       (selected-window)))
    (goto-char (point-min))
    (search-forward "erface.el")
    (should (equal (get-text-property (match-beginning 0)
                                      'ai-code-session-link)
                   "https://example.com/repo/project-interface.el"))))

(ert-deftest test-ai-code-backends-infra-ghostel-region-linkify-remote-url ()
  "Queued Ghostel region linkification should linkify remote URLs only."
  (with-temp-buffer
    (setq-local default-directory "/ssh:example:/repo/")
    (setq-local ai-code-backends-infra--session-directory default-directory)
    (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
    (insert "https://example.com/repo/project-int   \n")
    (insert "erface.el\n")
    (cl-letf (((symbol-function
                'ai-code-session-link--linkify-strict-image-preview-region)
               (lambda (&rest _args)
                 (error "Strict image previews should be skipped remotely"))))
      (ai-code-backends-infra-ghostel--linkify-image-preview-region
       (current-buffer)
       (point-min)
       (point-max)))
    (goto-char (point-min))
    (search-forward "erface.el")
    (should (equal (get-text-property (match-beginning 0)
                                      'ai-code-session-link)
                   "https://example.com/repo/project-interface.el"))))

(ert-deftest test-ai-code-backends-infra-ghostel-schedules-visible-linkify-per-window ()
  "Visible image linkification timers should be isolated per window."
  (let ((buffer (generate-new-buffer " *ai-code-ghostel-window-timers*"))
        (owners (make-hash-table :test 'eq))
        cancelled
        right-window)
    (unwind-protect
        (let ((window-min-width 1))
          (delete-other-windows)
          (setq right-window (split-window-right))
          (set-window-buffer (selected-window) buffer)
          (set-window-buffer right-window buffer)
          (with-current-buffer buffer
            (setq-local ai-code-backends-infra--session-terminal-backend
                        'ghostel)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_delay _repeat _function &rest args)
                         (let ((timer (timer-create)))
                           (puthash timer (cadr args) owners)
                           timer)))
                      ((symbol-function 'cancel-timer)
                       (lambda (timer)
                         (push (gethash timer owners) cancelled))))
              (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
               (selected-window)
               '(0.1))
              (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
               right-window
               '(0.2))
              (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
               (selected-window)
               '(0.3))
              (should (equal cancelled (list (selected-window))))
              (should (assq (selected-window)
                            ai-code-backends-infra-ghostel--visible-image-linkify-timers))
              (should (assq right-window
                            ai-code-backends-infra-ghostel--visible-image-linkify-timers)))))
      (when (window-live-p right-window)
        (delete-window right-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-visible-linkify-is-bounded ()
  "Visible image linkification should not scan the full scrollback."
  (let (linkified)
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (insert (make-string 1000 ?x))
      (goto-char (point-min))
      (set-window-start (selected-window) (point-min) t)
      (let ((ai-code-backends-infra-ghostel-visible-image-linkify-max-chars
             100))
        (cl-letf (((symbol-function
                    'ai-code-session-link--linkify-strict-image-preview-region)
                   (lambda (start end)
                     (setq linkified (cons start end)))))
          (ai-code-backends-infra-ghostel--linkify-visible-image-previews
           (current-buffer)
           (selected-window))
          (should linkified)
          (should (<= (- (cdr linkified) (car linkified)) 100)))))))

(ert-deftest test-ai-code-backends-infra-ghostel-visible-linkify-retries-same-region ()
  "Visible image linkification should retry delayed scans for the same text."
  (let ((scan-count 0))
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (insert "history.png\n")
      (goto-char (point-min))
      (set-window-start (selected-window) (point-min) t)
      (cl-letf (((symbol-function
                  'ai-code-session-link--linkify-strict-image-preview-region)
                 (lambda (_start _end)
                   (setq scan-count (1+ scan-count)))))
        (ai-code-backends-infra-ghostel--linkify-visible-image-previews
         (current-buffer)
         (selected-window))
        (ai-code-backends-infra-ghostel--linkify-visible-image-previews
         (current-buffer)
         (selected-window))
        (should (= scan-count 2))))))

(ert-deftest test-ai-code-backends-infra-ghostel-remote-linkify-skips-file-stats ()
  "Remote Ghostel image recovery should not inspect remote file paths."
  (let (path-resolution-called strict-linkify-called)
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (setq-local ai-code-backends-infra--session-directory
                  "/ssh:example:/repo/")
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (insert "./remote.png\n")
      (goto-char (point-min))
      (set-window-start (selected-window) (point-min) t)
      (cl-letf (((symbol-function
                  'ai-code-session-link--resolve-existing-local-path)
                 (lambda (&rest _args)
                   (setq path-resolution-called t)
                   nil))
                ((symbol-function
                  'ai-code-session-link--linkify-strict-image-preview-region)
                 (lambda (&rest _args)
                   (setq strict-linkify-called t)
                   (ai-code-session-link--resolve-existing-local-path
                    "remote.png"
                    "/ssh:example:/repo/"))))
        (ai-code-backends-infra-ghostel--linkify-visible-image-previews
         (current-buffer)
         (selected-window))
        (ai-code-backends-infra-ghostel--linkify-image-preview-region
         (current-buffer)
         (point-min)
         (point-max))
        (goto-char (point-min))
        (search-forward "./remote.png")
        (should (equal (get-text-property (match-beginning 0)
                                          'ai-code-session-link)
                       "./remote.png"))
        (should-not strict-linkify-called)
        (should-not path-resolution-called)))))

(ert-deftest test-ai-code-backends-infra-ghostel-redraw-linkify-is-bounded ()
  "Ghostel redraw linkification should use the queued link-detection region."
  (let (linkified original-called)
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (setq-local ghostel--plain-link-detection-begin 4)
      (setq-local ghostel--plain-link-detection-end 900)
      (insert (make-string 1000 ?x))
      (let ((ai-code-backends-infra-ghostel-visible-image-linkify-max-chars
             100))
        (cl-letf (((symbol-function
                    'ai-code-session-link--linkify-strict-image-preview-region)
                   (lambda (start end)
                     (setq linkified (cons start end)))))
          (ai-code-backends-infra-ghostel--run-queued-link-detection-around
           (lambda (_buffer)
             (setq original-called t))
           (current-buffer))
          (should original-called)
          (should linkified)
          (should (<= (- (cdr linkified) (car linkified)) 100)))))))

(ert-deftest test-ai-code-backends-infra-ghostel-readonly-entry-schedules-visible-linkify ()
  "Ghostel copy/emacs mode entry should schedule visible image recovery."
  (let (scheduled)
    (with-temp-buffer
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (cl-letf (((symbol-function
                  'ai-code-backends-infra-ghostel-schedule-visible-image-linkify-for-buffer)
                 (lambda (&optional buffer delays)
                   (setq scheduled (list buffer delays)))))
        (ai-code-backends-infra-ghostel--after-readonly-command)
        (should (equal scheduled (list (current-buffer) nil)))))))

(ert-deftest test-ai-code-backends-infra-ghostel-start-process-binds-image-mediums-before-exec ()
  "Ghostel startup should pass image mediums to `ghostel-exec' before init."
  (with-temp-buffer
    (let ((ai-code-backends-infra-ghostel-kitty-graphics-mediums
           '(file temp-file))
          (mediums-seen nil)
          (original-bound (boundp 'ghostel-kitty-graphics-mediums))
          (original-value (and (boundp 'ghostel-kitty-graphics-mediums)
                               ghostel-kitty-graphics-mediums)))
      (unwind-protect
          (progn
            (setq ghostel-kitty-graphics-mediums nil)
            (cl-letf (((symbol-function 'ai-code-backends-infra--configure-session-input-shortcuts)
                       (lambda () nil))
                      ((symbol-function 'ai-code-backends-infra--install-navigation-cursor-sync)
                       (lambda () nil))
                      ((symbol-function 'ghostel-exec)
                       (lambda (_buffer _program _args)
                         (setq mediums-seen ghostel-kitty-graphics-mediums)
                         nil)))
              (ai-code-backends-infra--start-ghostel-process
               (current-buffer) "codex --foo"))
            (should (equal mediums-seen '(file temp-file)))
            (should (equal ghostel-kitty-graphics-mediums '(file temp-file))))
        (if original-bound
            (setq ghostel-kitty-graphics-mediums original-value)
          (makunbound 'ghostel-kitty-graphics-mediums))))))

(ert-deftest test-ai-code-backends-infra-ghostel-command-lifecycle-updates-session-metadata ()
  "OSC 133 command hooks should write structured session metadata."
  (let ((buffer (generate-new-buffer "*ai-code-ghostel-lifecycle*"))
        (updates nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-session-update-metadata)
                   (lambda (target metadata)
                     (push (list target metadata) updates))))
          (with-current-buffer buffer
            (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel))
          (ai-code-backends-infra-ghostel--command-start buffer)
          (ai-code-backends-infra-ghostel--command-finish buffer 2)
          (let ((start (cadr (nth 1 updates)))
                (finish (cadr (nth 0 updates))))
            (should (eq (plist-get start :ghostel-command-state) 'started))
            (should (eq (plist-get start :ghostel-status) 'running))
            (should (null (plist-get start :ghostel-command-exit-status)))
            (should (eq (plist-get finish :ghostel-command-state) 'finished))
            (should (= (plist-get finish :ghostel-command-exit-status) 2))
            (should (eq (plist-get finish :ghostel-status) 'error))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-command-hooks-ignore-non-ai-buffers ()
  "Global Ghostel command hooks should not touch unrelated Ghostel buffers."
  (let ((buffer (generate-new-buffer "*plain-ghostel*"))
        (updated nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-session-update-metadata)
                   (lambda (&rest _args)
                     (setq updated t))))
          (ai-code-backends-infra-ghostel--command-start buffer)
          (ai-code-backends-infra-ghostel--command-finish buffer 0)
          (should-not updated))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-ai-code-backends-infra-ghostel-progress-updates-session-and-delegates ()
  "OSC 9;4 progress should update metadata and preserve Ghostel's handler."
  (let ((updates nil)
        (delegated nil))
    (cl-letf (((symbol-function 'ai-code-session-update-metadata)
               (lambda (target metadata)
                 (push (list target metadata) updates))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (setq-local ai-code-backends-infra-ghostel--progress-function
                    (lambda (state progress)
                      (setq delegated (list state progress))))
        (ai-code-backends-infra-ghostel--progress 'set 42)
        (let ((metadata (cadar updates)))
          (should (eq (plist-get metadata :ghostel-progress-state) 'set))
          (should (= (plist-get metadata :ghostel-progress-value) 42))
          (should (eq (plist-get metadata :ghostel-status) 'running)))
        (should (equal delegated '(set 42)))))))

(ert-deftest test-ai-code-backends-infra-ghostel-progress-remove-marks-idle ()
  "OSC 9;4 remove progress should mark the Ghostel status as idle."
  (let ((updates nil))
    (cl-letf (((symbol-function 'ai-code-session-update-metadata)
               (lambda (_target metadata)
                 (push metadata updates))))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
        (ai-code-backends-infra-ghostel--progress 'remove nil)
        (should (eq (plist-get (car updates) :ghostel-progress-state) 'remove))
        (should (eq (plist-get (car updates) :ghostel-status) 'idle))))))

(provide 'test_ai-code-backends-infra-ghostel)
;;; test_ai-code-backends-infra-ghostel.el ends here
