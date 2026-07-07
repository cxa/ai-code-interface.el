;;; ai-code-backends-infra-ghostel.el --- Ghostel support for AI Code terminals  -*- lexical-binding: t; -*-

;; Author: Kang Tu, AI Agent
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Ghostel-specific support for `ai-code-backends-infra'.

;;; Code:

(require 'ai-code-session-link)

;; Prefer `ghostel-exec' for Ghostel backend startup when available, as
;; it simplifies process startup integration.

(defcustom ai-code-backends-infra-ghostel-kitty-graphics-mediums
  '(file temp-file)
  "Extra Kitty graphics image-loading mediums for AI Code Ghostel sessions.

Ghostel always supports direct inline image transmission.  Enabling `file'
and `temp-file' lets trusted local AI CLI tools display image files through
the Kitty graphics protocol as well.  `shared-mem' is intentionally not
enabled by default because it widens the terminal's local resource access."
  :type '(set (const :tag "Local file medium" file)
              (const :tag "Temp-file medium" temp-file)
              (const :tag "Shared-memory medium" shared-mem))
  :group 'ai-code-backends-infra)

(declare-function ai-code-backends-infra--configure-session-input-shortcuts
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--install-navigation-cursor-sync
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--note-meaningful-output
                  "ai-code-backends-infra" ())
(declare-function ai-code-backends-infra--output-meaningful-p
                  "ai-code-backends-infra" (output))
(declare-function ai-code-backends-infra--set-session-directory
                  "ai-code-backends-infra" (buffer directory))
(declare-function ai-code-backends-infra--sync-terminal-cursor
                  "ai-code-backends-infra" ())
(declare-function ai-code-session-update-metadata
                  "ai-code-session" (id-or-buffer metadata))
(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel--run-queued-plain-link-detection
                  "ghostel" (buffer))
(declare-function ghostel--redispatch-scroll-event "ghostel" (event))
(declare-function ghostel-copy-mode "ghostel" ())
(declare-function ghostel-emacs-mode "ghostel" ())
(declare-function ai-code-session-link--linkify-strict-image-preview-region
                  "ai-code-session-link" (start end))

(defvar ai-code-backends-infra--session-terminal-backend)
(defvar ghostel-kitty-graphics-mediums)
(eval-when-compile
  (defvar ghostel-command-finish-functions)
  (defvar ghostel-command-start-functions)
  (defvar ghostel-kill-buffer-on-exit)
  (defvar ghostel-progress-function)
  (defvar ghostel-set-title-function)
  (defvar ghostel--copy-mode-active)
  (defvar ghostel--input-mode)
  (defvar ghostel--plain-link-detection-begin)
  (defvar ghostel--plain-link-detection-end))

(defvar-local ai-code-backends-infra-ghostel--progress-function nil
  "Original Ghostel progress function captured before AI Code wrapping.")

(defconst ai-code-backends-infra-ghostel--linkify-redraw-delay 0.05
  "Seconds to wait before relinkifying recent Ghostel output.")

(defcustom ai-code-backends-infra-ghostel-visible-image-linkify-delays
  '(0.15 0.5)
  "Delays used to scan visible Ghostel text for image previews.

Only the visible window region is scanned.  This keeps restored session
history responsive even when Ghostel has a large scrollback."
  :type '(repeat number)
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-visible-image-linkify-max-chars
  20000
  "Maximum visible Ghostel characters scanned for image previews at once."
  :type 'integer
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-scroll-image-linkify-delay 0.08
  "Debounce delay before scanning visible Ghostel text after scrolling."
  :type 'number
  :group 'ai-code-backends-infra)

(defvar-local ai-code-backends-infra-ghostel--visible-image-linkify-timers nil
  "Timers used to scan visible Ghostel text for image previews.")

(defun ai-code-backends-infra-ghostel-ensure-backend ()
  "Ensure the Ghostel backend is available."
  (unless (featurep 'ghostel) (require 'ghostel nil t))
  (unless (featurep 'ghostel)
    (user-error "The package ghostel is not installed")))

(defun ai-code-backends-infra-ghostel-navigation-mode-p ()
  "Return non-nil when the current Ghostel buffer is in copy mode."
  (or (bound-and-true-p ghostel--copy-mode-active)
      (eq ghostel--input-mode 'copy)))

(defun ai-code-backends-infra-ghostel-install-navigation-cursor-sync ()
  "Install cursor synchronization for Ghostel navigation mode."
  (add-hook 'post-command-hook
            #'ai-code-backends-infra--sync-terminal-cursor nil t))

(defun ai-code-backends-infra-ghostel--ai-session-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is an AI Code Ghostel session buffer."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (and (boundp 'ai-code-backends-infra--session-terminal-backend)
           (eq ai-code-backends-infra--session-terminal-backend 'ghostel)))))

(defun ai-code-backends-infra-ghostel--update-session-metadata (buffer metadata)
  "Merge METADATA into the AI Code session associated with BUFFER."
  (when (and (buffer-live-p buffer)
             (fboundp 'ai-code-session-update-metadata)
             (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer))
    (with-current-buffer buffer
      (ai-code-session-update-metadata buffer metadata))))

(defun ai-code-backends-infra-ghostel--command-start (buffer)
  "Record a Ghostel OSC 133 command-start event for BUFFER."
  (ai-code-backends-infra-ghostel--update-session-metadata
   buffer
   (list :ghostel-command-state 'started
         :ghostel-command-started-at (float-time)
         :ghostel-command-finished-at nil
         :ghostel-command-exit-status nil
         :ghostel-status 'running)))

(defun ai-code-backends-infra-ghostel--command-finish (buffer exit-status)
  "Record a Ghostel OSC 133 command-finish event for BUFFER.
EXIT-STATUS is the status reported by Ghostel, or nil when unavailable."
  (ai-code-backends-infra-ghostel--update-session-metadata
   buffer
   (list :ghostel-command-state 'finished
         :ghostel-command-finished-at (float-time)
         :ghostel-command-exit-status exit-status
         :ghostel-status (if (and exit-status (/= exit-status 0))
                              'error
                            'idle))))

(defun ai-code-backends-infra-ghostel--progress-status (state)
  "Return an AI Code status symbol for Ghostel progress STATE."
  (pcase state
    ('remove 'idle)
    ('error 'error)
    ('pause 'paused)
    ((or 'set 'indeterminate) 'running)
    (_ 'running)))

(defun ai-code-backends-infra-ghostel--progress (state progress)
  "Record a Ghostel OSC 9;4 progress report and delegate to Ghostel.
STATE and PROGRESS use the signature of `ghostel-progress-function'."
  (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
    (ai-code-backends-infra-ghostel--update-session-metadata
     (current-buffer)
     (list :ghostel-progress-state state
           :ghostel-progress-value progress
           :ghostel-progress-updated-at (float-time)
           :ghostel-status
           (ai-code-backends-infra-ghostel--progress-status state))))
  (when (and ai-code-backends-infra-ghostel--progress-function
             (not (eq ai-code-backends-infra-ghostel--progress-function
                      #'ai-code-backends-infra-ghostel--progress)))
    (funcall ai-code-backends-infra-ghostel--progress-function state progress)))

(defun ai-code-backends-infra-ghostel--install-lifecycle-hooks ()
  "Install Ghostel lifecycle hooks for the current AI Code session."
  (when (boundp 'ghostel-command-start-functions)
    (add-hook 'ghostel-command-start-functions
              #'ai-code-backends-infra-ghostel--command-start nil t))
  (when (boundp 'ghostel-command-finish-functions)
    (add-hook 'ghostel-command-finish-functions
              #'ai-code-backends-infra-ghostel--command-finish nil t))
  (when (boundp 'ghostel-progress-function)
    (unless (eq ghostel-progress-function
                #'ai-code-backends-infra-ghostel--progress)
      (setq-local ai-code-backends-infra-ghostel--progress-function
                  ghostel-progress-function))
    (setq-local ghostel-progress-function
                #'ai-code-backends-infra-ghostel--progress)))

(defun ai-code-backends-infra-ghostel--visible-image-region (window)
  "Return the bounded visible buffer region for WINDOW."
  (when (and (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (with-current-buffer (window-buffer window)
      (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
        (save-excursion
          (save-restriction
            (widen)
            (let* ((start (max (point-min) (window-start window)))
                   (end (or (ignore-errors (window-end window t))
                            (min (point-max)
                                 (+ start
                                    ai-code-backends-infra-ghostel-visible-image-linkify-max-chars))))
                   (end (min (point-max) end)))
              (when (> (- end start)
                       ai-code-backends-infra-ghostel-visible-image-linkify-max-chars)
                (setq start (max (point-min)
                                 (- end
                                    ai-code-backends-infra-ghostel-visible-image-linkify-max-chars))))
              (goto-char start)
              (setq start (line-beginning-position))
              (goto-char end)
              (setq end (min (point-max) (line-end-position)))
              (when (> (- end start)
                       ai-code-backends-infra-ghostel-visible-image-linkify-max-chars)
                (setq end (min (point-max)
                                (+ start
                                   ai-code-backends-infra-ghostel-visible-image-linkify-max-chars))))
              (and (< start end) (cons start end)))))))))

(defun ai-code-backends-infra-ghostel--linkify-visible-image-previews
    (buffer window)
  "Linkify image previews for visible text in BUFFER shown by WINDOW."
  (when (and (buffer-live-p buffer)
             (window-live-p window)
             (eq (window-buffer window) buffer))
    (with-current-buffer buffer
      (setq ai-code-backends-infra-ghostel--visible-image-linkify-timers
            (delq nil
                  (mapcar
                   (lambda (timer)
                     (and (timerp timer) timer))
                   ai-code-backends-infra-ghostel--visible-image-linkify-timers)))
      (when-let* ((region
                   (ai-code-backends-infra-ghostel--visible-image-region
                    window)))
        (ai-code-session-link--linkify-strict-image-preview-region
         (car region)
         (cdr region))))))

(defun ai-code-backends-infra-ghostel--linkify-image-preview-region
    (buffer start end)
  "Linkify image previews in BUFFER between START and END.
The region is bounded so Ghostel redraw/link-detection advice cannot
accidentally scan an entire scrollback buffer."
  (when (and (buffer-live-p buffer)
             (integer-or-marker-p start)
             (integer-or-marker-p end))
    (with-current-buffer buffer
      (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
        (save-restriction
          (widen)
          (let ((start (if (markerp start) (marker-position start) start))
                (end (if (markerp end) (marker-position end) end)))
            (setq start (max (point-min) (min (point-max) start))
                  end (max (point-min) (min (point-max) end)))
            (when (< start end)
              (when (> (- end start)
                       ai-code-backends-infra-ghostel-visible-image-linkify-max-chars)
                (setq start
                      (max (point-min)
                           (- end
                              ai-code-backends-infra-ghostel-visible-image-linkify-max-chars))))
              (save-excursion
                (goto-char start)
                (setq start (line-beginning-position))
                (goto-char end)
                (setq end (min (point-max) (line-end-position)))
                (when (> (- end start)
                         ai-code-backends-infra-ghostel-visible-image-linkify-max-chars)
                  (setq start
                        (max (point-min)
                             (- end
                                ai-code-backends-infra-ghostel-visible-image-linkify-max-chars))))
                (when (< start end)
                  (ai-code-session-link--linkify-strict-image-preview-region
                   start end))))))))))

(defun ai-code-backends-infra-ghostel-schedule-visible-image-linkify
    (window &optional delays)
  "Schedule image preview linkification for visible Ghostel text in WINDOW.
Optional DELAYS overrides
`ai-code-backends-infra-ghostel-visible-image-linkify-delays'."
  (when (and (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (let ((buffer (window-buffer window)))
      (when (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer)
        (with-current-buffer buffer
          (dolist (timer ai-code-backends-infra-ghostel--visible-image-linkify-timers)
            (when (timerp timer)
              (cancel-timer timer)))
          (setq ai-code-backends-infra-ghostel--visible-image-linkify-timers
                (mapcar
                 (lambda (delay)
                   (run-at-time
                    delay nil
                    #'ai-code-backends-infra-ghostel--linkify-visible-image-previews
                    buffer window))
                 (or delays
                     ai-code-backends-infra-ghostel-visible-image-linkify-delays))))))))

(defun ai-code-backends-infra-ghostel-schedule-visible-image-linkify-for-buffer
    (&optional buffer delays)
  "Schedule visible image preview linkification for BUFFER's live windows.
Optional DELAYS overrides the default visible image linkification delays."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (dolist (window (get-buffer-window-list buffer nil t))
        (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
         window delays)))))

(defun ai-code-backends-infra-ghostel--window-scroll (window _display-start)
  "Schedule visible image preview linkification after WINDOW scrolls."
  (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
   window
   (list ai-code-backends-infra-ghostel-scroll-image-linkify-delay)))

(defun ai-code-backends-infra-ghostel--after-readonly-command (&rest _args)
  "Schedule visible image preview linkification after Ghostel mode changes."
  (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
    (ai-code-backends-infra-ghostel-schedule-visible-image-linkify-for-buffer
     (current-buffer))))

(defun ai-code-backends-infra-ghostel--after-redispatch-scroll-event (event)
  "Schedule visible image preview linkification after Ghostel scroll EVENT."
  (when-let* ((window (ignore-errors (posn-window (event-start event))))
              ((window-live-p window)))
    (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
     window
     (list ai-code-backends-infra-ghostel-scroll-image-linkify-delay))))

(defun ai-code-backends-infra-ghostel--run-queued-link-detection-around
    (original buffer)
  "Run ORIGINAL Ghostel link detection in BUFFER and add image previews."
  (let (start end)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
          (setq start (and (boundp 'ghostel--plain-link-detection-begin)
                           ghostel--plain-link-detection-begin)
                end (and (boundp 'ghostel--plain-link-detection-end)
                         ghostel--plain-link-detection-end)))))
    (prog1 (funcall original buffer)
      (when (and start end)
        (ai-code-backends-infra-ghostel--linkify-image-preview-region
         buffer start end)))))

(defun ai-code-backends-infra-ghostel--install-image-preview-advice ()
  "Install Ghostel advice used to add and refresh image previews."
  (when (and (fboundp 'ghostel--run-queued-plain-link-detection)
             (not (advice-member-p
                   #'ai-code-backends-infra-ghostel--run-queued-link-detection-around
                   'ghostel--run-queued-plain-link-detection)))
    (advice-add 'ghostel--run-queued-plain-link-detection
                :around
                #'ai-code-backends-infra-ghostel--run-queued-link-detection-around))
  (when (and (fboundp 'ghostel--redispatch-scroll-event)
             (not (advice-member-p
                   #'ai-code-backends-infra-ghostel--after-redispatch-scroll-event
                   'ghostel--redispatch-scroll-event)))
    (advice-add 'ghostel--redispatch-scroll-event
                :after
                #'ai-code-backends-infra-ghostel--after-redispatch-scroll-event))
  (when (and (fboundp 'ghostel-copy-mode)
             (not (advice-member-p
                   #'ai-code-backends-infra-ghostel--after-readonly-command
                   'ghostel-copy-mode)))
    (advice-add 'ghostel-copy-mode
                :after
                #'ai-code-backends-infra-ghostel--after-readonly-command))
  (when (and (fboundp 'ghostel-emacs-mode)
             (not (advice-member-p
                   #'ai-code-backends-infra-ghostel--after-readonly-command
                   'ghostel-emacs-mode)))
    (advice-add 'ghostel-emacs-mode
                :after
                #'ai-code-backends-infra-ghostel--after-readonly-command)))

(defun ai-code-backends-infra-ghostel--effective-kitty-graphics-mediums ()
  "Return Kitty graphics mediums for the current AI Code Ghostel session."
  (delete-dups
   (append ai-code-backends-infra-ghostel-kitty-graphics-mediums
           (and (boundp 'ghostel-kitty-graphics-mediums)
                ghostel-kitty-graphics-mediums))))

(defun ai-code-backends-infra-ghostel--configure-image-support ()
  "Configure Ghostel image support for the current AI Code session."
  (when (boundp 'ghostel-kitty-graphics-mediums)
    (setq-local ghostel-kitty-graphics-mediums
                (ai-code-backends-infra-ghostel--effective-kitty-graphics-mediums))))

(defun ai-code-backends-infra-ghostel-send-string (string &optional paste)
  "Send STRING to the current Ghostel process.
If PASTE is non-nil, send it as a pasted string."
  (if (and paste (fboundp 'ghostel-paste-string))
      (ghostel-paste-string string)
    (ghostel-send-string string)))

(defun ai-code-backends-infra-ghostel-send-escape ()
  "Send escape to the current Ghostel process."
  (ghostel-send-key "escape"))

(defun ai-code-backends-infra-ghostel-send-return ()
  "Send return to the current Ghostel process."
  (ghostel-send-key "return"))

(defun ai-code-backends-infra-ghostel-send-backspace ()
  "Send backspace to the current Ghostel process."
  (ghostel-send-key "backspace"))

(defun ai-code-backends-infra-ghostel-resize-handler ()
  "Return the Ghostel resize handler.
Ghostel owns terminal-model resizing through its mode-local window hooks."
  nil)

(defun ai-code-backends-infra--configure-ghostel-buffer ()
  "Configure the current Ghostel buffer for AI Code sessions."
  (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
  (setq-local ghostel-set-title-function nil)
  (setq-local ghostel-kill-buffer-on-exit nil)
  (ai-code-backends-infra-ghostel--configure-image-support)
  (ai-code-backends-infra-ghostel--install-lifecycle-hooks)
  (ai-code-backends-infra-ghostel--install-image-preview-advice)
  (add-hook 'window-scroll-functions
            #'ai-code-backends-infra-ghostel--window-scroll nil t)
  (ai-code-backends-infra--configure-session-input-shortcuts)
  (ai-code-backends-infra--install-navigation-cursor-sync))

(defun ai-code-backends-infra--start-ghostel-process (buffer command)
  "Start a Ghostel session in BUFFER for COMMAND."
  (with-current-buffer buffer
    (ai-code-backends-infra--configure-ghostel-buffer)
    (let* ((argv (split-string-shell-command command))
           (program (car argv))
           (args (cdr argv)))
      (cond
       ((not program) nil)
       ((fboundp 'ghostel-exec)
        (let ((proc
               (let ((ghostel-kitty-graphics-mediums
                      (ai-code-backends-infra-ghostel--effective-kitty-graphics-mediums)))
                 (ghostel-exec buffer program args))))
          ;; `ghostel-exec' enters `ghostel-mode', which resets local state.
          (ai-code-backends-infra--configure-ghostel-buffer)
          proc))
       (t
        (user-error
         "Ghostel backend requires a Ghostel version that provides `ghostel-exec`"))))))

(defun ai-code-backends-infra-ghostel-create-session (buffer-name working-dir command env-vars)
  "Create a Ghostel session named BUFFER-NAME in WORKING-DIR.
COMMAND is the shell command to run and ENV-VARS are extra environment
variables for the terminal process."
  (let* ((working-dir (file-name-as-directory (expand-file-name working-dir)))
         (buffer (get-buffer-create buffer-name))
         (process-environment (append env-vars process-environment)))
    (ai-code-backends-infra--set-session-directory buffer working-dir)
    (with-current-buffer buffer
      (setq-local default-directory working-dir)
      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
      (let ((default-directory working-dir)
            (proc (ai-code-backends-infra--start-ghostel-process buffer command)))
        (setq-local default-directory working-dir)
        (ai-code-backends-infra--set-session-directory buffer working-dir)
        (ai-code-backends-infra--configure-ghostel-buffer)
        (when (processp proc)
          (ignore-errors
            (set-process-query-on-exit-flag proc nil))
          (when-let* ((sentinel (ignore-errors (process-sentinel proc))))
            (ignore-errors
              (process-put proc
                           'ai-code-backends-infra--ghostel-sentinel
                           sentinel)))
          (let ((orig-filter (process-filter proc)))
            (set-process-filter
             proc
             (lambda (process output)
               (when (buffer-live-p buffer)
                 (when orig-filter
                   (funcall orig-filter process output))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (ai-code-backends-infra--output-meaningful-p output)
                       (ai-code-backends-infra--note-meaningful-output))
                     (ai-code-session-link--schedule-linkify-recent-output
                      buffer
                      output
                      ai-code-backends-infra-ghostel--linkify-redraw-delay))))))))
        (cons buffer proc)))))

(provide 'ai-code-backends-infra-ghostel)
;;; ai-code-backends-infra-ghostel.el ends here
