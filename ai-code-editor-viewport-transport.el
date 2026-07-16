;;; ai-code-editor-viewport-transport.el --- Viewport PTY transport  -*- lexical-binding: t; -*-

;; Author: realazy
;; SPDX-License-Identifier: Apache-2.0

;; Keywords: tools, convenience

;;; Commentary:
;; Authenticated PTY framing, editor helper generation, and session environment
;; setup for AI CLI editor viewports.  Terminal-specific adapters provide only
;; their frame prefix and request callback.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function ai-code-editor-viewport--open-request
                  "ai-code-editor-viewport" (source-buffer payload))

(defvar ai-code-editor-viewport-enabled)

(defcustom ai-code-editor-viewport-max-request-size (* 1024 1024)
  "Maximum encoded size in bytes for one terminal editor request."
  :type 'integer
  :group 'ai-code)

(defconst ai-code-editor-viewport--protocol-prefix
  "\e]6973;ai-code-editor;"
  "Base prefix for editor requests emitted through a managed terminal PTY.")

(defconst ai-code-editor-viewport--protocol-suffix
  "\a"
  "Suffix for editor requests emitted through a managed terminal PTY.")

(defconst ai-code-editor-viewport--frame-prefix-environment-variable
  "AI_CODE_EDITOR_VIEWPORT_FRAME_PREFIX"
  "Environment variable containing the helper's terminal frame prefix.")

(defvar ai-code-editor-viewport--protocol-token
  (secure-hash
   'sha256
   (format "%s:%s:%s:%s"
           (emacs-pid) (float-time) (random) (user-uid)))
  "Per-Emacs token authenticating terminal editor request frames.")

(defvar-local ai-code-editor-viewport--protocol-pending ""
  "Partial terminal editor protocol frame awaiting more process output.")

(defvar ai-code-editor-viewport--helper-file nil
  "Path to the generated CLI editor helper.")

;;;; Terminal transport

(defun ai-code-editor-viewport--frame-prefix ()
  "Return the authenticated prefix for terminal editor request frames."
  (concat ai-code-editor-viewport--protocol-prefix
          ai-code-editor-viewport--protocol-token
          ";"))

(defun ai-code-editor-viewport-frame-token ()
  "Return the token terminal adapters use to authenticate editor requests."
  ai-code-editor-viewport--protocol-token)

(defun ai-code-editor-viewport--partial-prefix-length (string)
  "Return the length of STRING's suffix matching the protocol prefix."
  (let* ((prefix (ai-code-editor-viewport--frame-prefix))
         (maximum (min (1- (length prefix)) (length string))))
    (cl-loop for length from maximum downto 1
             when (string-suffix-p (substring prefix 0 length) string)
             return length
             finally return 0)))

(defun ai-code-editor-viewport--parse-output (pending output)
  "Parse terminal OUTPUT after PENDING protocol data.
Return a plist with visible `:output', incomplete `:pending', and decoded
  base64 `:payloads'."
  (let* ((data (concat pending output))
         (prefix (ai-code-editor-viewport--frame-prefix))
         (suffix ai-code-editor-viewport--protocol-suffix)
         (position 0)
         (visible nil)
         (payloads nil)
         incomplete
         frame-start)
    (while (and (not incomplete)
                (setq frame-start (string-search prefix data position)))
      (push (substring data position frame-start) visible)
      (let* ((payload-start (+ frame-start (length prefix)))
             (frame-end (string-search suffix data payload-start)))
        (if frame-end
            (progn
              (push (substring data payload-start frame-end) payloads)
              (setq position (+ frame-end (length suffix))))
          (setq pending (substring data frame-start)
                position (length data)
                incomplete t))))
    (unless incomplete
      (let* ((remainder (substring data position))
             (partial-length
              (ai-code-editor-viewport--partial-prefix-length remainder)))
        (push (substring remainder 0 (- (length remainder) partial-length))
              visible)
        (setq pending
              (if (> partial-length 0)
                  (substring remainder (- partial-length))
                ""))))
    (list :output (apply #'concat (nreverse visible))
          :pending pending
          :payloads (nreverse payloads))))

(defun ai-code-editor-viewport--schedule-request (source-buffer payload)
  "Schedule editor PAYLOAD for SOURCE-BUFFER when it is within limits."
  (if (> (string-bytes payload) ai-code-editor-viewport-max-request-size)
      (progn
        (message "Discarded oversized AI Code editor request")
        nil)
    (run-at-time 0 nil
                 #'ai-code-editor-viewport--open-request
                 source-buffer payload)
    t))

(defun ai-code-editor-viewport-filter-output (process output)
  "Remove editor requests from PROCESS OUTPUT and schedule their viewports."
  (let ((buffer (ignore-errors (process-buffer process))))
    (if (not (buffer-live-p buffer))
        output
      (with-current-buffer buffer
        (let ((parsed
               (ai-code-editor-viewport--parse-output
                ai-code-editor-viewport--protocol-pending output)))
          (let ((pending (plist-get parsed :pending)))
            (if (> (string-bytes pending)
                   ai-code-editor-viewport-max-request-size)
                (progn
                  (setq ai-code-editor-viewport--protocol-pending "")
                  (message "Discarded oversized AI Code editor request"))
              (setq ai-code-editor-viewport--protocol-pending pending)))
          (dolist (payload (plist-get parsed :payloads))
            (ai-code-editor-viewport--schedule-request buffer payload))
          (plist-get parsed :output))))))

(defun ai-code-editor-viewport-handle-request
    (source-buffer token payload)
  "Schedule SOURCE-BUFFER editor PAYLOAD when TOKEN authenticates it."
  (cond
   ((or (not (stringp token))
        (not (string= token ai-code-editor-viewport--protocol-token))
        (not (stringp payload))
        (not (buffer-live-p source-buffer)))
    nil)
   (t
    (ai-code-editor-viewport--schedule-request
     source-buffer payload))))

(defun ai-code-editor-viewport--helper-content ()
  "Return the terminal editor helper script content."
  (let ((status-template
         (expand-file-name "ai-code-editor-status-XXXXXX"
                           temporary-file-directory)))
    (concat
     "#!/bin/sh\n"
     "submit=0\n"
     "[ \"${1-}\" = \"--ai-code-submit\" ] && submit=1 && shift\n"
     "[ \"$#\" -gt 0 ] || exit 1\n"
     "status_file=$(mktemp "
     (shell-quote-argument status-template)
     ") || exit 1\n"
     "cleanup() { rm -f \"$status_file\"; }\n"
     "trap cleanup 0\n"
     "trap 'exit 1' 1 2 15\n"
     "payload=$(\n"
     "  {\n"
     "    printf '%s\\0' \"$status_file\"\n"
     "    printf '%s\\0' \"${PWD-}\"\n"
     "    printf '%s\\0' \"$submit\"\n"
     "    printf '%s\\0' \"$@\"\n"
     "  } | base64 | tr -d '\\r\\n'\n"
     ") || exit 1\n"
     "frame_prefix=${"
     ai-code-editor-viewport--frame-prefix-environment-variable
     "-}\n"
     "[ -n \"$frame_prefix\" ] || exit 1\n"
     "printf '%s%s\\007' \"$frame_prefix\" \"$payload\""
     " > /dev/tty || exit 1\n"
     "parent_pid=$PPID\n"
     "timeout=${AI_CODE_EDITOR_VIEWPORT_TIMEOUT:-3600}\n"
     "case \"$timeout\" in\n"
     "  ''|*[!0-9]*) exit 1 ;;\n"
     "esac\n"
     "attempts=0\n"
     "while [ ! -s \"$status_file\" ]; do\n"
     "  kill -0 \"$parent_pid\" 2>/dev/null || exit 1\n"
     "  attempts=$((attempts + 1))\n"
     "  [ \"$attempts\" -lt \"$timeout\" ] || exit 1\n"
     "  sleep 1\n"
     "done\n"
     "IFS= read -r result < \"$status_file\" || exit 1\n"
     "[ \"$result\" = \"0\" ]\n")))

(defun ai-code-editor-viewport--cleanup-helper ()
  "Delete the generated CLI editor helper, if any."
  (when (and ai-code-editor-viewport--helper-file
             (file-exists-p ai-code-editor-viewport--helper-file))
    (delete-file ai-code-editor-viewport--helper-file))
  (setq ai-code-editor-viewport--helper-file nil))

(defun ai-code-editor-viewport--ensure-helper ()
  "Return the executable used as the CLI editor command."
  (let ((content (ai-code-editor-viewport--helper-content)))
    (unless (and ai-code-editor-viewport--helper-file
                 (file-executable-p ai-code-editor-viewport--helper-file)
                 (with-temp-buffer
                   (insert-file-contents ai-code-editor-viewport--helper-file)
                   (equal (buffer-string) content)))
      (ai-code-editor-viewport--cleanup-helper)
      (setq ai-code-editor-viewport--helper-file
            (make-temp-file "ai-code-editor-"))
      (with-temp-file ai-code-editor-viewport--helper-file
        (insert content))
      (set-file-modes ai-code-editor-viewport--helper-file #o700))
    ai-code-editor-viewport--helper-file))

(add-hook 'kill-emacs-hook #'ai-code-editor-viewport--cleanup-helper)

;;;; Session environment

(defun ai-code-editor-viewport--supported-p ()
  "Return non-nil when this host can run the terminal editor helper."
  (and (file-executable-p "/bin/sh")
       (file-exists-p "/dev/tty")
       (executable-find "base64")
       (executable-find "mktemp")))

(defun ai-code-editor-viewport--environment-entry-p (entry names)
  "Return non-nil when environment ENTRY sets one of NAMES."
  (cl-some (lambda (name)
             (string-prefix-p (concat name "=") entry))
           names))

(defun ai-code-editor-viewport-environment (environment &optional frame-prefix)
  "Return ENVIRONMENT configured to edit through a terminal viewport.
FRAME-PREFIX, when non-nil, selects an adapter-specific terminal frame.
General editor requests submit restored input; Git editor requests only save."
  (if (or (not ai-code-editor-viewport-enabled)
          (not (ai-code-editor-viewport--supported-p)))
      environment
    (let* ((helper (ai-code-editor-viewport--ensure-helper))
           (helper-command (shell-quote-argument helper))
           (submit-command (concat helper-command " --ai-code-submit"))
           (prefix (or frame-prefix
                       (ai-code-editor-viewport--frame-prefix))))
      (append
       (list (concat "EDITOR=" submit-command)
             (concat "VISUAL=" submit-command)
             (concat "GIT_EDITOR=" helper-command)
             (concat "GIT_SEQUENCE_EDITOR=" helper-command)
             (concat
              ai-code-editor-viewport--frame-prefix-environment-variable
              "=" prefix))
       (cl-remove-if
        (lambda (entry)
          (ai-code-editor-viewport--environment-entry-p
           entry '("EDITOR" "VISUAL" "GIT_EDITOR"
                   "GIT_SEQUENCE_EDITOR"
                   "AI_CODE_EDITOR_VIEWPORT_FRAME_PREFIX")))
        environment)))))

(provide 'ai-code-editor-viewport-transport)
;;; ai-code-editor-viewport-transport.el ends here
