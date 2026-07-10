;;; ai-code-backends-infra-ghostel.el --- Ghostel support for AI Code terminals  -*- lexical-binding: t; -*-

;; Author: Kang Tu, realazy (cxa)
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Ghostel-specific support for `ai-code-backends-infra'.

;;; Code:

(require 'cl-lib)
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
(declare-function ghostel--schedule-link-detection
                  "ghostel" (&optional begin end))
(declare-function ghostel--viewport-start "ghostel" ())
(declare-function ghostel--run-queued-plain-link-detection
                  "ghostel" (buffer))
(declare-function ghostel--redispatch-scroll-event "ghostel" (event))
(declare-function ghostel-copy-mode "ghostel" ())
(declare-function ghostel-emacs-mode "ghostel" ())
(declare-function ghostel-ime-mode "ghostel-ime" (&optional arg))
(declare-function ai-code-session-link--linkify-strict-image-preview-region
                  "ai-code-session-link" (start end))
(declare-function ai-code-session-link--recent-output-plain-text
                  "ai-code-session-link" (output))

(defvar ai-code-backends-infra--session-terminal-backend)
(defvar ai-code-backends-infra--session-directory)
(defvar ai-code-session-link--path-base-regexp)
(defvar ai-code-session-link--url-pattern-regexp)
(defvar ai-code-session-link--image-preview-position-function)
(defvar ai-code-session-link-inhibit-functions)
(defvar ghostel-inhibit-redraw-functions)
(defvar ghostel-kitty-graphics-mediums)
(defvar ghostel-link-map)
(eval-when-compile
  (defvar ghostel-command-finish-functions)
  (defvar ghostel-command-start-functions)
  (defvar ghostel-kill-buffer-on-exit)
  (defvar ghostel-progress-function)
  (defvar ghostel-set-title-function)
  (defvar ghostel--copy-mode-active)
  (defvar ghostel--fake-cursor-overlay)
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

(defcustom ai-code-backends-infra-ghostel-anti-flicker t
  "Enable short output batching for Ghostel TUI redraws.

Interactive AI CLIs often repaint status rows by clearing and rewriting the
same terminal line in several process-output chunks.  Batching redraw-like
chunks prevents users from seeing those incomplete intermediate frames."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-render-delay 0.01
  "Seconds to wait before flushing queued Ghostel redraw output."
  :type 'number
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-enable-ime-integration t
  "Enable Ghostel IME integration in AI Code Ghostel sessions.

When available, `ghostel-ime-mode' defers Ghostel redraws while an
Emacs Lisp input-method composition is active, preventing active TUI
redraws from clobbering in-progress preedit text."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-inhibit-redraw-during-native-preedit t
  "Defer Ghostel redraws while a GUI-native preedit overlay is active.

This covers native input-method preedit paths that do not go through
`ghostel-ime-mode', such as GUI framework preedit overlays."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-inhibit-redraw-after-input nil
  "Defer Ghostel redraws briefly after user input in AI Code sessions.

Some GUI-native preedit underlines are not visible to Lisp as overlays or
text properties.  Deferring redraws during the short window after input keeps
animated status rows from repeatedly repainting the buffer over preedit text.
This is disabled by default because it can make interactive echo feel laggy."
  :type 'boolean
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-input-redraw-inhibit-delay 0.8
  "Seconds to keep Ghostel redraws deferred after recent user input."
  :type 'number
  :group 'ai-code-backends-infra)

(defcustom ai-code-backends-infra-ghostel-native-preedit-overlay-symbols
  '(x-preedit-overlay pgtk-preedit-overlay ns-preedit-overlay
                      mac-preedit-overlay)
  "Overlay variables that may hold an active GUI-native preedit overlay."
  :type '(repeat symbol)
  :group 'ai-code-backends-infra)

(defconst ai-code-backends-infra-ghostel--preedit-properties
  '(preedit preedit-text input-method)
  "Text and overlay properties that identify in-progress preedit text.")

(defconst ai-code-backends-infra-ghostel--redraw-regexp
  "\033\\[[0-9;?]*[A-GJKMH]"
  "Regexp matching ANSI terminal redraw, clear, or cursor movement sequences.")

(defvar-local ai-code-backends-infra-ghostel--visible-image-linkify-timers nil
  "Alist of (WINDOW . TIMERS) for visible image preview scans.")

(defvar-local ai-code-backends-infra-ghostel--last-live-viewport-start nil
  "Marker at the previous start of Ghostel's mutable live viewport.")

(defvar-local ai-code-backends-infra-ghostel--render-queue nil
  "Queued Ghostel output waiting for anti-flicker rendering.")

(defvar-local ai-code-backends-infra-ghostel--render-timer nil
  "Timer used to flush `ai-code-backends-infra-ghostel--render-queue'.")

(defvar-local ai-code-backends-infra-ghostel--dim-foreground-active nil
  "Non-nil when AI Code injected an ANSI gray foreground for SGR dim text.")

(defvar-local ai-code-backends-infra-ghostel--foreground-state nil
  "Current foreground state tracked for AI Code Ghostel SGR normalization.
The value is nil for the default foreground, `explicit' for a foreground
set by the subprocess, and `injected' for AI Code's ANSI gray foreground.")

(defvar-local ai-code-backends-infra-ghostel--last-input-activity-time nil
  "Float timestamp of the most recent user input in this Ghostel session.")

(defconst ai-code-backends-infra-ghostel--process-filter-wrapped-property
  'ai-code-backends-infra-ghostel-process-filter-wrapped
  "Process property marking AI Code's Ghostel output filter wrapper.")

(defconst ai-code-backends-infra-ghostel--preserved-link-properties
  '(help-echo mouse-face keymap follow-link font-lock-face face
    ghostel-link-id ai-code-session-link ai-code-session-symbol-link
    ai-code-session-symbol-file ai-code-session-hover-link)
  "Text properties to preserve across Ghostel redraws.")

(defconst ai-code-backends-infra-ghostel--session-link-candidate-regexp
  (concat "\\(?:"
          ai-code-session-link--url-pattern-regexp
          "\\|"
          ai-code-session-link--path-base-regexp
          "\\(?:[#:(][[:alnum:],L-]+\\)?"
          "\\)")
  "Regexp matching URL and path candidates for Ghostel redraw linkify.")

(defvar-local ai-code-backends-infra-ghostel--preserved-link-spans nil
  "Cached clickable link spans restored after Ghostel redraws.")

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

(defun ai-code-backends-infra-ghostel--trusted-local-session-p ()
  "Return non-nil when this Ghostel session can safely read local files."
  (not (file-remote-p
        (or ai-code-backends-infra--session-directory
            default-directory))))

(defun ai-code-backends-infra-ghostel--live-viewport-start ()
  "Return the start of Ghostel's mutable live viewport, or nil."
  (when (fboundp 'ghostel--viewport-start)
    (when-let* ((start (ignore-errors (ghostel--viewport-start)))
                ((integer-or-marker-p start)))
      (max (point-min) (min (point-max) start)))))

(defun ai-code-backends-infra-ghostel--stable-image-preview-position-p
    (_start end)
  "Return non-nil when an image ending at END is in stable scrollback."
  (when-let* ((viewport-start
               (ai-code-backends-infra-ghostel--live-viewport-start)))
    (<= end viewport-start)))

(defun ai-code-backends-infra-ghostel--newly-stable-image-region
    (viewport-start)
  "Return rows made stable by VIEWPORT-START since the previous redraw.
Remember VIEWPORT-START for the next redraw."
  (let* ((viewport-start
          (max (point-min) (min (point-max) viewport-start)))
         (previous-marker
          ai-code-backends-infra-ghostel--last-live-viewport-start)
         (previous-start
          (and (markerp previous-marker)
               (marker-position previous-marker)))
         (region
          (and previous-start
               (< previous-start viewport-start)
               (cons previous-start viewport-start))))
    (if (markerp previous-marker)
        (set-marker previous-marker viewport-start (current-buffer))
      (setq ai-code-backends-infra-ghostel--last-live-viewport-start
            (copy-marker viewport-start)))
    region))

(defun ai-code-backends-infra-ghostel--active-preedit-overlay-p
    (overlay buffer)
  "Return non-nil when OVERLAY is an active preedit overlay in BUFFER."
  (and (overlayp overlay)
       (eq (overlay-buffer overlay) buffer)
       (overlay-start overlay)
       (overlay-end overlay)
       (or (< (overlay-start overlay) (overlay-end overlay))
           (overlay-get overlay 'after-string)
           (overlay-get overlay 'before-string)
           (overlay-get overlay 'display))))

(defun ai-code-backends-infra-ghostel--preedit-value-active-p (value)
  "Return non-nil when VALUE represents active preedit state."
  (cond
   ((stringp value) (> (length value) 0))
   ((overlayp value) (overlay-buffer value))
   ((consp value) t)
   (t value)))

(defun ai-code-backends-infra-ghostel--preedit-property-active-p
    (getter object)
  "Return non-nil when GETTER finds a preedit property on OBJECT."
  (cl-some
   (lambda (property)
     (ai-code-backends-infra-ghostel--preedit-value-active-p
      (funcall getter object property)))
   ai-code-backends-infra-ghostel--preedit-properties))

(defun ai-code-backends-infra-ghostel--overlay-near-point-p
    (overlay point)
  "Return non-nil when OVERLAY is attached near POINT."
  (let ((start (overlay-start overlay))
        (end (overlay-end overlay)))
    (and start
         end
         (<= (1- start) point)
         (<= point (1+ end)))))

(defun ai-code-backends-infra-ghostel--margin-display-string-p (string)
  "Return non-nil when STRING is a margin display spec, not preedit text."
  (and (stringp string)
       (> (length string) 0)
       (let ((disp (get-text-property 0 'display string)))
         (and (consp disp)
              (or (eq (car disp) 'margin)
                  (and (consp (car disp))
                       (eq (caar disp) 'margin)))))))

(defun ai-code-backends-infra-ghostel--known-non-preedit-overlay-p
    (overlay)
  "Return non-nil when OVERLAY is known not to represent preedit text."
  (or (overlay-get overlay 'ai-code-session-image-preview)
      (and (boundp 'ghostel--fake-cursor-overlay)
           (eq overlay ghostel--fake-cursor-overlay))
      (ai-code-backends-infra-ghostel--margin-display-string-p
       (overlay-get overlay 'before-string))
      (ai-code-backends-infra-ghostel--margin-display-string-p
       (overlay-get overlay 'after-string))))

(defun ai-code-backends-infra-ghostel--point-preedit-overlay-p
    (overlay buffer point)
  "Return non-nil when OVERLAY looks like preedit text near POINT in BUFFER."
  (and (ai-code-backends-infra-ghostel--active-preedit-overlay-p
        overlay buffer)
       (ai-code-backends-infra-ghostel--overlay-near-point-p
        overlay point)
       (not
        (ai-code-backends-infra-ghostel--known-non-preedit-overlay-p
         overlay))
       (or (ai-code-backends-infra-ghostel--preedit-property-active-p
            #'overlay-get overlay)
           (overlay-get overlay 'after-string)
           (overlay-get overlay 'before-string)
           (overlay-get overlay 'display))))

(defun ai-code-backends-infra-ghostel--point-preedit-overlays-p
    (buffer)
  "Return non-nil when BUFFER has preedit overlays around point."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((point (point))
             (start (max (point-min) (1- point)))
             (end (min (point-max) (1+ point)))
             (overlays (delete-dups
                        (append (overlays-at point)
                                (overlays-in start end)))))
        (cl-some
         (lambda (overlay)
           (ai-code-backends-infra-ghostel--point-preedit-overlay-p
            overlay buffer point))
         overlays)))))

(defun ai-code-backends-infra-ghostel--point-preedit-properties-p
    (buffer)
  "Return non-nil when BUFFER has preedit text properties around point."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((positions (delete-dups
                        (list (point)
                              (max (point-min) (1- (point)))))))
        (cl-some
         (lambda (position)
           (and (<= (point-min) position)
                (< position (point-max))
                (ai-code-backends-infra-ghostel--preedit-property-active-p
                 #'get-text-property position)))
         positions)))))

(defun ai-code-backends-infra-ghostel--preedit-symbol-active-p ()
  "Return non-nil when a native preedit dynamic variable is active."
  (and (boundp 'preedit-text)
       (ai-code-backends-infra-ghostel--preedit-value-active-p
        (symbol-value 'preedit-text))))

(defun ai-code-backends-infra-ghostel--native-preedit-active-p
    (&optional buffer)
  "Return non-nil when BUFFER has an active GUI-native preedit overlay.

BUFFER defaults to the current buffer.  This function is installed in
`ghostel-inhibit-redraw-functions' for AI Code Ghostel sessions."
  (let ((buffer (or buffer (current-buffer))))
    (and ai-code-backends-infra-ghostel-inhibit-redraw-during-native-preedit
         (buffer-live-p buffer)
         (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer)
         (with-current-buffer buffer
           (or
            (ai-code-backends-infra-ghostel--preedit-symbol-active-p)
            (cl-some
             (lambda (symbol)
               (and (boundp symbol)
                    (ai-code-backends-infra-ghostel--active-preedit-overlay-p
                     (symbol-value symbol)
                     buffer)))
             ai-code-backends-infra-ghostel-native-preedit-overlay-symbols)
            (ai-code-backends-infra-ghostel--point-preedit-overlays-p
             buffer)
            (ai-code-backends-infra-ghostel--point-preedit-properties-p
             buffer))))))

(defun ai-code-backends-infra-ghostel--note-input-activity ()
  "Record recent user input for Ghostel redraw protection."
  (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
    (setq ai-code-backends-infra-ghostel--last-input-activity-time
          (float-time))))

(defun ai-code-backends-infra-ghostel--recent-input-active-p
    (&optional buffer)
  "Return non-nil when BUFFER recently received user input."
  (let ((buffer (or buffer (current-buffer))))
    (and ai-code-backends-infra-ghostel-inhibit-redraw-after-input
         (buffer-live-p buffer)
         (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer)
         (with-current-buffer buffer
           (and ai-code-backends-infra-ghostel--last-input-activity-time
                (< (- (float-time)
                      ai-code-backends-infra-ghostel--last-input-activity-time)
                   ai-code-backends-infra-ghostel-input-redraw-inhibit-delay))))))

(defun ai-code-backends-infra-ghostel--redraw-inhibited-p
    (&optional buffer)
  "Return non-nil when BUFFER should defer redraw or linkification."
  (let ((buffer (or buffer (current-buffer))))
    (or (ai-code-backends-infra-ghostel--native-preedit-active-p buffer)
        (ai-code-backends-infra-ghostel--recent-input-active-p buffer))))

(defun ai-code-backends-infra-ghostel--install-preedit-redraw-inhibition ()
  "Install native preedit redraw inhibition for the current Ghostel buffer."
  (when (or ai-code-backends-infra-ghostel-inhibit-redraw-during-native-preedit
            ai-code-backends-infra-ghostel-inhibit-redraw-after-input)
    (add-hook 'ghostel-inhibit-redraw-functions
              #'ai-code-backends-infra-ghostel--redraw-inhibited-p nil t)
    (add-hook 'ai-code-session-link-inhibit-functions
              #'ai-code-backends-infra-ghostel--redraw-inhibited-p nil t))
  (when ai-code-backends-infra-ghostel-inhibit-redraw-after-input
    (add-hook 'pre-command-hook
              #'ai-code-backends-infra-ghostel--note-input-activity nil t)
    (add-hook 'post-command-hook
              #'ai-code-backends-infra-ghostel--note-input-activity nil t)))

(defun ai-code-backends-infra-ghostel--link-properties-at (position)
  "Return preserved link properties at POSITION."
  (let (properties)
    (dolist (property ai-code-backends-infra-ghostel--preserved-link-properties)
      (when-let* ((value (get-text-property position property)))
        (setq properties (plist-put properties property value))))
    properties))

(defun ai-code-backends-infra-ghostel--link-properties-p (properties)
  "Return non-nil when PROPERTIES describe a clickable session link."
  (or (plist-get properties 'help-echo)
      (plist-get properties 'ai-code-session-link)
      (plist-get properties 'ai-code-session-hover-link)))

(defun ai-code-backends-infra-ghostel--cache-preserved-link-spans
    (start end)
  "Cache clickable link spans between START and END for redraw restoration."
  (when (and (ai-code-backends-infra-ghostel--ai-session-buffer-p)
             (integer-or-marker-p start)
             (integer-or-marker-p end))
    (setq start (max (point-min) start)
          end (min (point-max) end))
    (when (< start end)
      (let ((position start)
            spans)
        (while (< position end)
          (let* ((next (or (next-property-change position nil end) end))
                 (properties
                  (ai-code-backends-infra-ghostel--link-properties-at
                   position)))
            (when (ai-code-backends-infra-ghostel--link-properties-p
                   properties)
              (push (list :start position
                          :end next
                          :text
                          (buffer-substring-no-properties position next)
                          :properties properties)
                    spans))
            (setq position next)))
        (when spans
          (setq ai-code-backends-infra-ghostel--preserved-link-spans
                (nreverse spans)))))))

(defun ai-code-backends-infra-ghostel--link-span-properties-present-p
    (start end properties)
  "Return non-nil when PROPERTIES are already present from START to END."
  (let ((position start)
        (present t))
    (while (and present (< position end))
      (let ((remaining properties))
        (while (and present remaining)
          (let ((property (pop remaining))
                (value (pop remaining)))
            (unless (equal (get-text-property position property) value)
              (setq present nil)))))
      (setq position (or (next-property-change position nil end) end)))
    present))

(defun ai-code-backends-infra-ghostel--restore-preserved-link-spans
    (&optional start end)
  "Restore cached clickable links after a Ghostel redraw.
START and END optionally bound the redraw region."
  (when (and ai-code-backends-infra-ghostel--preserved-link-spans
             (ai-code-backends-infra-ghostel--ai-session-buffer-p))
    (let ((region-start (and (integer-or-marker-p start) start))
          (region-end (and (integer-or-marker-p end) end))
          (inhibit-read-only t)
          (inhibit-modification-hooks t))
      (dolist (span ai-code-backends-infra-ghostel--preserved-link-spans)
        (let ((span-start (plist-get span :start))
              (span-end (plist-get span :end))
              (text (plist-get span :text))
              (properties (plist-get span :properties)))
          (when (and (integer-or-marker-p span-start)
                     (integer-or-marker-p span-end)
                     (<= (point-min) span-start)
                     (<= span-start span-end)
                     (<= span-end (point-max))
                     (or (not region-start) (>= span-end region-start))
                     (or (not region-end) (<= span-start region-end))
                     (equal (buffer-substring-no-properties
                             span-start span-end)
                            text)
                     (not
                      (ai-code-backends-infra-ghostel--link-span-properties-present-p
                       span-start span-end properties)))
            (add-text-properties span-start span-end properties)))))))

(defun ai-code-backends-infra-ghostel--linkified-candidate-at-p (position)
  "Return non-nil when POSITION already belongs to a clickable link."
  (ai-code-backends-infra-ghostel--link-properties-p
   (ai-code-backends-infra-ghostel--link-properties-at position)))

(defun ai-code-backends-infra-ghostel--region-needs-session-linkify-p
    (start end)
  "Return non-nil when START to END has unlinked session-link candidates."
  (let ((position start)
        needs-linkify)
    (save-excursion
      (goto-char start)
      (while (and (not needs-linkify)
                  (re-search-forward
                   ai-code-backends-infra-ghostel--session-link-candidate-regexp
                   end t))
        (setq position (match-beginning 0))
        (unless (ai-code-backends-infra-ghostel--linkified-candidate-at-p
                 position)
          (setq needs-linkify t))))
    needs-linkify))

(defun ai-code-backends-infra-ghostel--linkify-session-region
    (start end)
  "Linkify START to END without refreshing already preserved links."
  (ai-code-backends-infra-ghostel--restore-preserved-link-spans start end)
  (when (ai-code-backends-infra-ghostel--region-needs-session-linkify-p
         start end)
    (ai-code-session-link--linkify-session-region start end))
  (ai-code-backends-infra-ghostel--cache-preserved-link-spans start end))

(defun ai-code-backends-infra-ghostel--around-schedule-link-detection
    (orig-fn &optional begin end)
  "Call ORIG-FN after restoring links before a redraw scan.
BEGIN and END are the Ghostel link-detection bounds."
  (ai-code-backends-infra-ghostel--restore-preserved-link-spans begin end)
  (funcall orig-fn begin end))

(defun ai-code-backends-infra-ghostel--around-run-queued-link-detection
    (orig-fn buffer)
  "Call ORIG-FN for BUFFER and cache Ghostel link spans afterward."
  (let (begin end)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
          (setq begin ghostel--plain-link-detection-begin
                end ghostel--plain-link-detection-end))))
    (prog1 (funcall orig-fn buffer)
      (when (and (buffer-live-p buffer)
                 (integer-or-marker-p begin)
                 (integer-or-marker-p end))
        (with-current-buffer buffer
          (ai-code-backends-infra-ghostel--cache-preserved-link-spans
           begin end))))))

(defun ai-code-backends-infra-ghostel--install-link-preservation-advice ()
  "Install Ghostel redraw link preservation advice."
  (when (and (fboundp 'ghostel--schedule-link-detection)
             (not (advice-member-p
                   #'ai-code-backends-infra-ghostel--around-schedule-link-detection
                   'ghostel--schedule-link-detection)))
    (advice-add 'ghostel--schedule-link-detection
                :around
                #'ai-code-backends-infra-ghostel--around-schedule-link-detection))
  (when (and (fboundp 'ghostel--run-queued-plain-link-detection)
             (not
              (advice-member-p
               #'ai-code-backends-infra-ghostel--around-run-queued-link-detection
               'ghostel--run-queued-plain-link-detection)))
    (advice-add
     'ghostel--run-queued-plain-link-detection
     :around
     #'ai-code-backends-infra-ghostel--around-run-queued-link-detection)))

(defun ai-code-backends-infra-ghostel--linkify-visible-image-previews
    (buffer window)
  "Linkify session links and image previews in BUFFER shown by WINDOW."
  (when (and (buffer-live-p buffer)
             (window-live-p window)
             (eq (window-buffer window) buffer))
    (if (ai-code-backends-infra-ghostel--redraw-inhibited-p buffer)
        (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
         window
         (list ai-code-session-link--linkify-inhibited-retry-delay))
      (with-current-buffer buffer
        (ai-code-backends-infra-ghostel--prune-visible-image-linkify-timers)
        (when-let* ((region
                     (ai-code-backends-infra-ghostel--visible-image-region
                      window)))
          (ai-code-backends-infra-ghostel--linkify-session-region
           (car region)
           (cdr region))
          (when (ai-code-backends-infra-ghostel--trusted-local-session-p)
            (ai-code-session-link--linkify-strict-image-preview-region
             (car region)
             (cdr region)))
          (ai-code-backends-infra-ghostel--cache-preserved-link-spans
           (car region)
           (cdr region)))))))

(defun ai-code-backends-infra-ghostel--cancel-visible-image-linkify-timers
    (window)
  "Cancel visible image preview scan timers registered for WINDOW."
  (when-let* ((entry
               (assq window
                     ai-code-backends-infra-ghostel--visible-image-linkify-timers)))
    (dolist (timer (cdr entry))
      (when (timerp timer)
        (cancel-timer timer)))
    (setq ai-code-backends-infra-ghostel--visible-image-linkify-timers
          (assq-delete-all
           window
           ai-code-backends-infra-ghostel--visible-image-linkify-timers))))

(defun ai-code-backends-infra-ghostel--prune-visible-image-linkify-timers ()
  "Remove visible image preview scan timer entries for dead windows."
  (setq ai-code-backends-infra-ghostel--visible-image-linkify-timers
        (cl-remove-if-not
         (lambda (entry)
           (and (consp entry)
                (window-live-p (car entry))))
         ai-code-backends-infra-ghostel--visible-image-linkify-timers)))

(defun ai-code-backends-infra-ghostel--register-visible-image-linkify-timers
    (window timers)
  "Register visible image preview scan TIMERS for WINDOW."
  (push (cons window timers)
        ai-code-backends-infra-ghostel--visible-image-linkify-timers))

(defun ai-code-backends-infra-ghostel--linkify-image-preview-region
    (buffer start end)
  "Linkify session links and image previews in BUFFER between START and END.
The region is bounded so Ghostel redraw/link-detection advice cannot
accidentally scan an entire scrollback buffer."
  (when (and (buffer-live-p buffer)
             (integer-or-marker-p start)
             (integer-or-marker-p end))
    (with-current-buffer buffer
      (when (and (ai-code-backends-infra-ghostel--ai-session-buffer-p)
                 (not
                  (ai-code-backends-infra-ghostel--redraw-inhibited-p
                   buffer)))
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
                  (ai-code-backends-infra-ghostel--linkify-session-region
                   start end)
                  (when (ai-code-backends-infra-ghostel--trusted-local-session-p)
                    (ai-code-session-link--linkify-strict-image-preview-region
                     start end))
                  (ai-code-backends-infra-ghostel--cache-preserved-link-spans
                   start end))))))))))

(defun ai-code-backends-infra-ghostel-schedule-visible-image-linkify
    (window &optional delays)
  "Schedule visible session link and image preview linkification for WINDOW.
Optional DELAYS overrides
`ai-code-backends-infra-ghostel-visible-image-linkify-delays'."
  (when (and (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (let ((buffer (window-buffer window)))
      (when (ai-code-backends-infra-ghostel--ai-session-buffer-p buffer)
        (with-current-buffer buffer
          (ai-code-backends-infra-ghostel--prune-visible-image-linkify-timers)
          (ai-code-backends-infra-ghostel--cancel-visible-image-linkify-timers
           window)
          (let ((timers
                 (mapcar
                  (lambda (delay)
                    (run-at-time
                     delay nil
                     #'ai-code-backends-infra-ghostel--linkify-visible-image-previews
                     buffer window))
                  (or delays
                      ai-code-backends-infra-ghostel-visible-image-linkify-delays))))
            (ai-code-backends-infra-ghostel--register-visible-image-linkify-timers
             window timers)))))))

(defun ai-code-backends-infra-ghostel-schedule-visible-image-linkify-for-buffer
    (&optional buffer delays)
  "Schedule visible session link and image preview linkification for BUFFER.
Optional DELAYS overrides the default visible image linkification delays."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (dolist (window (get-buffer-window-list buffer nil t))
        (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
         window delays)))))

(defun ai-code-backends-infra-ghostel--window-scroll (window _display-start)
  "Schedule visible session link and image preview linkification for WINDOW."
  (ai-code-backends-infra-ghostel-schedule-visible-image-linkify
   window
   (list ai-code-backends-infra-ghostel-scroll-image-linkify-delay)))

(defun ai-code-backends-infra-ghostel--after-readonly-command (&rest _args)
  "Schedule visible session link and image preview linkification."
  (when (ai-code-backends-infra-ghostel--ai-session-buffer-p)
    (ai-code-backends-infra-ghostel-schedule-visible-image-linkify-for-buffer
     (current-buffer))))

(defun ai-code-backends-infra-ghostel--after-redispatch-scroll-event (event)
  "Schedule visible session link and image preview linkification after EVENT."
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
      (when (and start end (buffer-live-p buffer))
        (with-current-buffer buffer
          (let* ((queued-detection-start
                  (if (markerp start) (marker-position start) start))
                 (live-viewport-start
                  (or (ai-code-backends-infra-ghostel--live-viewport-start)
                      queued-detection-start))
                 (configured-position-function
                  ai-code-session-link--image-preview-position-function))
            (when-let* ((stable-region
                         (ai-code-backends-infra-ghostel--newly-stable-image-region
                          live-viewport-start)))
              (ai-code-backends-infra-ghostel--linkify-image-preview-region
               buffer (car stable-region) (cdr stable-region)))
            (let ((ai-code-session-link--image-preview-position-function
                   (lambda (preview-start preview-end)
                     (and (<= preview-end live-viewport-start)
                          (or (not configured-position-function)
                              (funcall configured-position-function
                                       preview-start preview-end))))))
              (ai-code-backends-infra-ghostel--linkify-image-preview-region
               buffer start end))))))))

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
   (append (and (ai-code-backends-infra-ghostel--trusted-local-session-p)
                ai-code-backends-infra-ghostel-kitty-graphics-mediums)
           (and (boundp 'ghostel-kitty-graphics-mediums)
                ghostel-kitty-graphics-mediums))))

(defun ai-code-backends-infra-ghostel--configure-image-support ()
  "Configure Ghostel image support for the current AI Code session."
  (when (boundp 'ghostel-kitty-graphics-mediums)
    (setq-local ghostel-kitty-graphics-mediums
                (ai-code-backends-infra-ghostel--effective-kitty-graphics-mediums))))

(defun ai-code-backends-infra-ghostel--redraw-output-p (output)
  "Return non-nil when OUTPUT looks like a TUI redraw frame."
  (let* ((output (or output ""))
         (clear-count (1- (length (split-string output "\033\\[K"))))
         (cr-count (cl-count ?\15 output))
         (escape-count (cl-count ?\033 output))
         (output-length (length output))
         (escape-density (if (> output-length 0)
                             (/ (float escape-count) output-length)
                           0)))
    (or (string-match-p ai-code-backends-infra-ghostel--redraw-regexp output)
        (>= cr-count 2)
        (and (> escape-density 0.3) (>= clear-count 2)))))

(defun ai-code-backends-infra-ghostel--animated-status-key (output)
  "Return a plain-text animated status key for OUTPUT, or nil."
  (let ((plain (ai-code-session-link--recent-output-plain-text output)))
    (when (and (not (string= plain ""))
               (string-match-p "\\bWorking\\b" plain))
      plain)))

(defun ai-code-backends-infra-ghostel--animated-status-output-p
    (output)
  "Return non-nil when OUTPUT redraws an animated status row."
  (and (ai-code-backends-infra-ghostel--redraw-output-p output)
       (ai-code-backends-infra-ghostel--animated-status-key output)))

(defun ai-code-backends-infra-ghostel--skip-sgr-color-params (codes)
  "Skip extended SGR color parameters from CODES."
  (pcase (car codes)
    ("5" (nthcdr 2 codes))
    ("2" (nthcdr 4 codes))
    (_ (cdr codes))))

(defun ai-code-backends-infra-ghostel--sgr-code-number (code)
  "Return the SGR number for CODE, treating an empty parameter as reset."
  (string-to-number (if (string= code "") "0" code)))

(defun ai-code-backends-infra-ghostel--sgr-effect (codes)
  "Return the final foreground and intensity effect of SGR CODES."
  (let ((dim nil)
        (normal nil)
        (explicit-foreground nil)
        (foreground-reset nil)
        (foreground-state ai-code-backends-infra-ghostel--foreground-state)
        (remaining codes))
    (while remaining
      (let* ((code (pop remaining))
             (number (ai-code-backends-infra-ghostel--sgr-code-number code)))
        (cond
         ((= number 0)
          (setq dim nil
                normal t
                foreground-reset t
                foreground-state nil))
         ((= number 2)
          (setq dim t
                normal nil))
         ((= number 22)
          (setq dim nil
                normal t))
         ((= number 39)
          (setq foreground-reset t
                foreground-state nil))
         ((or (<= 30 number 37)
              (<= 90 number 97))
          (setq explicit-foreground t
                foreground-state 'explicit))
         ((= number 38)
          (setq explicit-foreground t
                foreground-state 'explicit)
          (setq remaining
                (ai-code-backends-infra-ghostel--skip-sgr-color-params
                 remaining)))
         ((= number 48)
          (setq remaining
                (ai-code-backends-infra-ghostel--skip-sgr-color-params
                 remaining))))))
    (list dim normal explicit-foreground foreground-reset foreground-state)))

(defun ai-code-backends-infra-ghostel--normalize-dim-sgr (output)
  "Render SGR dim text with the standard ANSI gray foreground in OUTPUT."
  (let ((start 0)
        (parts nil)
        (output (or output "")))
    (while (string-match "\e\\[\\([0-9;]*\\)m" output start)
      (let* ((match-beginning (match-beginning 0))
             (match-end (match-end 0))
             (sequence (match-string 0 output))
             (params (match-string 1 output))
             (codes (if (string= params "") '("0") (split-string params ";")))
             (was-injected
              (eq ai-code-backends-infra-ghostel--foreground-state
                  'injected)))
        (cl-destructuring-bind
            (dim normal explicit-foreground foreground-reset foreground-state)
            (ai-code-backends-infra-ghostel--sgr-effect codes)
          (push (substring output start match-beginning) parts)
          (push
           (cond
            ((and dim (null foreground-state))
             (setq ai-code-backends-infra-ghostel--foreground-state 'injected
                   ai-code-backends-infra-ghostel--dim-foreground-active t)
             (format "\e[%sm" (mapconcat #'identity (append codes '("90")) ";")))
            ((and normal
                  was-injected
                  (not foreground-reset)
                  (not explicit-foreground))
             (setq ai-code-backends-infra-ghostel--foreground-state nil
                   ai-code-backends-infra-ghostel--dim-foreground-active nil)
             (format "\e[%sm" (mapconcat #'identity (append codes '("39")) ";")))
            (t
             (setq ai-code-backends-infra-ghostel--foreground-state
                   foreground-state
                   ai-code-backends-infra-ghostel--dim-foreground-active
                   (eq foreground-state 'injected))
             sequence))
           parts)
          (setq start match-end))))
    (push (substring output start) parts)
    (apply #'concat (nreverse parts))))

(defun ai-code-backends-infra-ghostel--render-output
    (buffer orig-filter process output)
  "Render OUTPUT for PROCESS in BUFFER and run AI Code bookkeeping.
ORIG-FILTER is Ghostel's original process filter."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((output (ai-code-backends-infra-ghostel--normalize-dim-sgr
                      output))
             (animated-status-output
              (ai-code-backends-infra-ghostel--animated-status-output-p
               output)))
        (when orig-filter
          (funcall orig-filter process output))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (ai-code-backends-infra--output-meaningful-p output)
              (ai-code-backends-infra--note-meaningful-output))
            (unless animated-status-output
              (ai-code-session-link--schedule-linkify-recent-output
               buffer
               output
               ai-code-backends-infra-ghostel--linkify-redraw-delay))))))))

(defun ai-code-backends-infra-ghostel--flush-render-queue
    (buffer orig-filter process)
  "Render queued Ghostel output for BUFFER using ORIG-FILTER and PROCESS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ai-code-backends-infra-ghostel--render-timer nil)
      (when ai-code-backends-infra-ghostel--render-queue
        (let ((output ai-code-backends-infra-ghostel--render-queue))
          (setq ai-code-backends-infra-ghostel--render-queue nil)
          (ai-code-backends-infra-ghostel--render-output
           buffer orig-filter process output))))))

(defun ai-code-backends-infra-ghostel--handle-process-output
    (buffer orig-filter process output)
  "Handle Ghostel PROCESS OUTPUT for BUFFER through ORIG-FILTER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (and ai-code-backends-infra-ghostel-anti-flicker
               (or ai-code-backends-infra-ghostel--render-queue
                   (ai-code-backends-infra-ghostel--redraw-output-p output)))
          (progn
            (setq ai-code-backends-infra-ghostel--render-queue
                  (concat ai-code-backends-infra-ghostel--render-queue output))
            (when ai-code-backends-infra-ghostel--render-timer
              (cancel-timer ai-code-backends-infra-ghostel--render-timer))
            (setq ai-code-backends-infra-ghostel--render-timer
                  (run-at-time
                   ai-code-backends-infra-ghostel-render-delay nil
                   #'ai-code-backends-infra-ghostel--flush-render-queue
                   buffer
                   orig-filter
                   process)))
        (ai-code-backends-infra-ghostel--render-output
         buffer orig-filter process output)))))

(defun ai-code-backends-infra-ghostel--wrap-process-filter
    (buffer process)
  "Wrap PROCESS output for the AI Code Ghostel session in BUFFER."
  (when (and (buffer-live-p buffer)
             (processp process)
             (not
              (process-get
               process
               ai-code-backends-infra-ghostel--process-filter-wrapped-property)))
    (let ((orig-filter (process-filter process)))
      (process-put
       process
       ai-code-backends-infra-ghostel--process-filter-wrapped-property
       t)
      (set-process-filter
       process
       (lambda (proc output)
         (let ((target (or (ignore-errors (process-buffer proc))
                           buffer)))
           (when (buffer-live-p target)
             (ai-code-backends-infra-ghostel--handle-process-output
              target orig-filter proc output))))))))

(defun ai-code-backends-infra-ghostel--enable-ime-integration ()
  "Enable Ghostel IME integration for the current AI Code session."
  (when (and ai-code-backends-infra-ghostel-enable-ime-integration
             (require 'ghostel-ime nil t)
             (fboundp 'ghostel-ime-mode))
    (ghostel-ime-mode 1)))

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
  (setq-local ai-code-session-link--image-preview-position-function
              #'ai-code-backends-infra-ghostel--stable-image-preview-position-p)
  (setq-local ghostel-set-title-function nil)
  (setq-local ghostel-kill-buffer-on-exit nil)
  (ai-code-backends-infra-ghostel--configure-image-support)
  (ai-code-backends-infra-ghostel--enable-ime-integration)
  (ai-code-backends-infra-ghostel--install-preedit-redraw-inhibition)
  (ai-code-backends-infra-ghostel--install-link-preservation-advice)
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
          (ai-code-backends-infra-ghostel--wrap-process-filter buffer proc))
        (cons buffer proc)))))

(provide 'ai-code-backends-infra-ghostel)
;;; ai-code-backends-infra-ghostel.el ends here
