;;; ai-code-editor-viewport.el --- Edit AI CLI files in Emacs  -*- lexical-binding: t; -*-

;; Author: realazy
;; SPDX-License-Identifier: Apache-2.0

;; Keywords: tools, convenience

;;; Commentary:
;; Route external editor requests from native AI CLI terminal sessions into an
;; Emacs viewport.  The session environment points to a generated helper that
;; emits an authenticated control frame through its PTY.  Terminal adapters
;; either intercept the raw frame before rendering or dispatch it through a
;; buffer-local callback, then open the request in the originating session's
;; window.  The associated-buffer handoff follows the viewport interaction idea
;; from https://github.com/xenodium/agent-shell while preserving each file's
;; normal major mode for editing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ai-code-editor-viewport-attachments)
(require 'ai-code-editor-viewport-transport)

;;;; Customization and state

(defcustom ai-code-editor-viewport-enabled t
  "Whether native AI CLI editor requests should open in Emacs."
  :type 'boolean
  :group 'ai-code)

(defcustom ai-code-editor-viewport-window-placement 'below
  "Where to display an editor viewport relative to its originating session.
Use `below' to keep the session visible above the viewport.  Use `replace' to
show the viewport in the originating session window.  If `below' cannot make
room for another window, the editor request fails instead of replacing the
session window."
  :type '(choice (const :tag "Below the current window" below)
                 (const :tag "Replace the session window" replace))
  :group 'ai-code)

(defcustom ai-code-editor-viewport-submit-delay 0.2
  "Seconds to wait before submitting input after a viewport is saved.
This gives the terminal TUI time to return from its external editor and restore
the input view before AI Code sends the return key."
  :type 'number
  :group 'ai-code)

(defface ai-code-editor-viewport-source-hint-face
  '((t :inherit shadow :slant italic))
  "Face for the originating TUI's disabled-input message."
  :group 'ai-code)

(defface ai-code-editor-viewport-header-key-face
  '((t :inherit bold))
  "Face for keyboard shortcuts in the editor viewport header."
  :group 'ai-code)

(defvar-local ai-code-editor-viewport--outcome nil
  "Outcome of the active editor viewport: `finished' or `canceled'.")

(defvar-local ai-code-editor-viewport--previous-header-line nil
  "Header line shown before editor viewport mode was enabled.")

(defvar-local ai-code-editor-viewport--source-directory nil
  "Working directory of the CLI session associated with this viewport.")

(defvar-local ai-code-editor-viewport--submit-function nil
  "Function that submits restored TUI input for this source buffer.")

(defvar-local ai-code-editor-viewport-source-cursor-function nil
  "Function returning the live terminal cursor position for this source.")

;;;; Viewport mode

(defvar ai-code-editor-viewport-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ai-code-editor-viewport-finish)
    (define-key map (kbd "C-c C-k") #'ai-code-editor-viewport-cancel)
    (define-key map (kbd "C-g") #'ai-code-editor-viewport-cancel)
    (define-key map [remap yank] #'ai-code-editor-viewport-yank)
    (define-key map [remap clipboard-yank] #'ai-code-editor-viewport-yank)
    map)
  "Keymap for `ai-code-editor-viewport-mode'.")

(defconst ai-code-editor-viewport--header-hints
  '((ai-code-editor-viewport-finish "submit")
    (ai-code-editor-viewport-cancel "cancel" :all-bindings t)
    (ai-code-editor-viewport-yank "paste text, files, or images"))
  "Commands and descriptions shown in an editor viewport header.")

(defun ai-code-editor-viewport--header-command-keys
    (command all-bindings)
  "Return keys for COMMAND in the viewport map.
Join every binding when ALL-BINDINGS is non-nil; otherwise return the first."
  (if all-bindings
      (when-let* ((bindings
                   (where-is-internal
                    command ai-code-editor-viewport-mode-map)))
        (mapconcat #'key-description bindings "/"))
    (when-let* ((binding
                 (where-is-internal
                  command ai-code-editor-viewport-mode-map t)))
      (key-description binding))))

(defun ai-code-editor-viewport--source-hint-message ()
  "Return a source-input hint derived from the current viewport bindings."
  (let ((finish-keys
         (ai-code-editor-viewport--header-command-keys
          #'ai-code-editor-viewport-finish nil))
        (cancel-keys
         (ai-code-editor-viewport--header-command-keys
          #'ai-code-editor-viewport-cancel t)))
    (string-join
     (delq nil
           (list (and finish-keys (format "%s: submit" finish-keys))
                 (and cancel-keys (format "%s: cancel" cancel-keys))))
     ", ")))

(defun ai-code-editor-viewport--header-hint (hint)
  "Return a styled header chunk for HINT, or nil when it is unbound."
  (pcase-let ((`(,command ,description . ,properties) hint))
    (when-let* ((keys
                 (ai-code-editor-viewport--header-command-keys
                  command (plist-get properties :all-bindings))))
      (concat
       (propertize keys 'face 'ai-code-editor-viewport-header-key-face)
       ": " description))))

(defun ai-code-editor-viewport--header-line ()
  "Return the styled header line for an editor viewport."
  (concat
   " "
   (mapconcat #'identity
              (delq nil
                    (mapcar #'ai-code-editor-viewport--header-hint
                            ai-code-editor-viewport--header-hints))
              "  ")
   " "))

(define-minor-mode ai-code-editor-viewport-mode
  "Edit a file requested by an AI CLI in a dedicated viewport.

\\{ai-code-editor-viewport-mode-map}"
  :lighter " AI Edit"
  :keymap ai-code-editor-viewport-mode-map
  (if ai-code-editor-viewport-mode
      (progn
        (setq ai-code-editor-viewport--previous-header-line header-line-format)
        (ai-code-editor-viewport-attachments-enable)
        (setq header-line-format
              (ai-code-editor-viewport--header-line)))
    (setq header-line-format ai-code-editor-viewport--previous-header-line)
    (setq ai-code-editor-viewport--previous-header-line nil)
    (ai-code-editor-viewport-attachments-disable)))

(defun ai-code-editor-viewport--source-cursor-position ()
  "Return the live cursor position for the current terminal source buffer."
  (let ((candidate
         (and (functionp ai-code-editor-viewport-source-cursor-function)
              (ignore-errors
                (funcall ai-code-editor-viewport-source-cursor-function)))))
    (if (and (or (integerp candidate)
                 (and (markerp candidate)
                      (eq (marker-buffer candidate) (current-buffer))))
             (<= (point-min) candidate (point-max)))
        candidate
      (point))))

(defun ai-code-editor-viewport--draft-match-column
    (draft cursor-offset source-line source-column)
  "Locate DRAFT's cursor line in SOURCE-LINE.
CURSOR-OFFSET is the offset in DRAFT and SOURCE-COLUMN is its terminal
column.  Return the source column where the draft line begins."
  (when (and (stringp draft)
             (integerp cursor-offset)
             (<= 0 cursor-offset (length draft)))
    (let* ((prefix (substring draft 0 cursor-offset))
           (draft-line-start (or (string-match "[^\n]*\\'" prefix) 0))
           (draft-line-end (or (string-match "\n" draft cursor-offset)
                               (length draft)))
           (draft-line (substring draft draft-line-start draft-line-end))
           (draft-column (- cursor-offset draft-line-start))
           (match-column (- source-column draft-column)))
      (when (and (<= 0 match-column)
                 (<= (+ match-column (length draft-line))
                     (length source-line))
                 (string= draft-line
                          (substring source-line
                                     match-column
                                     (+ match-column
                                        (length draft-line)))))
        match-column))))

(defun ai-code-editor-viewport--disable-source-input
    (source-buffer &optional draft cursor-offset)
  "Visually disable the current input line in SOURCE-BUFFER.
When DRAFT and CURSOR-OFFSET are available, use them to find the input
boundary without assuming a particular TUI prompt format.
Return the temporary overlay, or nil when SOURCE-BUFFER is no longer live."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (save-excursion
        (goto-char (ai-code-editor-viewport--source-cursor-position))
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (source-position (point))
               (source-line
                (buffer-substring-no-properties line-start line-end))
               (match-column
                (ai-code-editor-viewport--draft-match-column
                 draft cursor-offset source-line
                 (- source-position line-start)))
               (property-start
                (or (previous-single-property-change
                     (min line-end (1+ source-position))
                     'face nil line-start)
                    (previous-single-property-change
                     (min line-end (1+ source-position))
                     'font-lock-face nil line-start)))
               (content-start
                (save-excursion
                  (goto-char
                   (cond
                    (match-column (+ line-start match-column))
                    ((and property-start (> property-start line-start))
                     property-start)
                    (t source-position)))
                  (skip-chars-backward " \t" line-start)
                  (point)))
               (source-window (get-buffer-window source-buffer t))
               (visible-width
                (when (window-live-p source-window)
                  (max
                   0
                   (- (window-body-width source-window)
                      (string-width
                       (buffer-substring-no-properties
                        line-start content-start))))))
               (source-width
                (max
                 (or visible-width 0)
                 (string-width
                  (buffer-substring-no-properties content-start line-end))))
               (source-face
                (seq-some
                 (lambda (position)
                   (or (get-char-property position 'face)
                       (get-char-property position 'font-lock-face)))
                 (delete-dups
                  (delq nil
                        (list (and (> content-start line-start)
                                   (1- content-start))
                              (and (< line-start line-end) line-start)
                              (and (< content-start line-end)
                                   content-start))))))
               (message-face
                (cond
                 ((null source-face)
                  'ai-code-editor-viewport-source-hint-face)
                 ((and (listp source-face)
                       (not (keywordp (car source-face))))
                  (cons 'ai-code-editor-viewport-source-hint-face
                        source-face))
                 (t
                  (list 'ai-code-editor-viewport-source-hint-face
                        source-face))))
               (message-text
                (format (if (eq ai-code-editor-viewport-window-placement
                                'replace)
                            " Editing in current window — %s"
                          " Editing in viewport below — %s")
                        (ai-code-editor-viewport--source-hint-message)))
               (message
                (propertize
                 (concat message-text
                         (make-string
                          (max 0 (- source-width
                                    (string-width message-text)))
                          ?\s))
                 'face message-face))
               (overlay
                (make-overlay content-start line-end source-buffer)))
          (if (= content-start line-end)
              (overlay-put overlay 'after-string message)
            (overlay-put overlay 'display message))
          (overlay-put overlay 'priority 1000)
          (overlay-put overlay 'help-echo
                       "Input is disabled while the editor viewport is active")
          (overlay-put overlay 'ai-code-editor-viewport-source-input t)
          overlay)))))

;;;; Editing lifecycle

(defun ai-code-editor-viewport-finish ()
  "Finish the current AI CLI editor viewport and save its file."
  (interactive)
  (unless ai-code-editor-viewport-mode
    (user-error "Not in an AI CLI editor viewport"))
  (setq ai-code-editor-viewport--outcome 'finished)
  (exit-recursive-edit))

(defun ai-code-editor-viewport-cancel ()
  "Cancel the current AI CLI editor viewport without saving changes."
  (interactive)
  (unless ai-code-editor-viewport-mode
    (user-error "Not in an AI CLI editor viewport"))
  (setq ai-code-editor-viewport--outcome 'canceled)
  (exit-recursive-edit))

(defun ai-code-editor-viewport--replace-window (buffer window)
  "Display BUFFER by replacing WINDOW and return its display state."
  (let ((previous-buffer (window-buffer window)))
    (set-window-buffer window buffer)
    (select-window window)
    (list :window window
          :previous-buffer previous-buffer
          :created-window nil)))

(defun ai-code-editor-viewport--next-side-slot (anchor-window)
  "Return a side-window slot geometrically below ANCHOR-WINDOW."
  (let* ((frame (window-frame anchor-window))
         (side (window-parameter anchor-window 'window-side))
         (anchor-slot (window-parameter anchor-window 'window-slot))
         (slots
          (seq-keep
           (lambda (window)
             (when (eq (window-parameter window 'window-side) side)
               (let ((slot (window-parameter window 'window-slot)))
                 (and (numberp slot) slot))))
           (window-list frame 'nomini))))
    (1+ (apply #'max (cons (if (numberp anchor-slot) anchor-slot 0)
                           slots)))))

(defun ai-code-editor-viewport--display-below-action (buffer anchor-window)
  "Display BUFFER below ANCHOR-WINDOW and return the resulting window."
  (let ((side (window-parameter anchor-window 'window-side)))
    (with-selected-window anchor-window
      (if (memq side '(left right))
          (display-buffer
           buffer
           `((display-buffer-in-side-window)
             (side . ,side)
             (slot . ,(ai-code-editor-viewport--next-side-slot
                       anchor-window))
             (inhibit-same-window . t)))
        (display-buffer
         buffer
         '((display-buffer-below-selected)
           (inhibit-same-window . t)))))))

(defun ai-code-editor-viewport--display-below (buffer anchor-window)
  "Display BUFFER below ANCHOR-WINDOW and return its display state."
  (let* ((windows-before
          (mapcar
           (lambda (window)
             (cons window (window-buffer window)))
           (window-list (window-frame anchor-window) 'nomini)))
         (viewport-window
          (ai-code-editor-viewport--display-below-action
           buffer anchor-window)))
    (let ((previous (and (window-live-p viewport-window)
                         (assq viewport-window windows-before))))
      (if (and (window-live-p viewport-window)
               (not (eq viewport-window anchor-window))
               (eq (window-frame viewport-window)
                   (window-frame anchor-window))
               (>= (nth 1 (window-edges viewport-window))
                   (nth 3 (window-edges anchor-window))))
          (progn
            (select-window viewport-window)
            (list :window viewport-window
                  :previous-buffer (cdr previous)
                  :created-window (not previous)))
        (when (and (window-live-p viewport-window)
                   (eq (window-frame viewport-window)
                       (window-frame anchor-window))
                   (eq (window-buffer viewport-window) buffer))
          (if previous
              (set-window-buffer viewport-window (cdr previous))
            (quit-window nil viewport-window)))
        (user-error
         "Cannot display AI Code editor viewport below the current window")))))

(defun ai-code-editor-viewport--display (buffer source-buffer)
  "Display BUFFER as a viewport associated with SOURCE-BUFFER.
Return a plist that records the window and its previous buffer."
  (let* ((source-window (and (buffer-live-p source-buffer)
                             (get-buffer-window source-buffer t)))
         (anchor-window (if (window-live-p source-window)
                            source-window
                          (selected-window))))
    (pcase ai-code-editor-viewport-window-placement
      ('below
       (ai-code-editor-viewport--display-below buffer anchor-window))
      ('replace
       (ai-code-editor-viewport--replace-window buffer anchor-window))
      (_
       (user-error "Unknown AI Code viewport placement: %S"
                   ai-code-editor-viewport-window-placement)))))

(defun ai-code-editor-viewport--restore-window (display-state buffer)
  "Restore DISPLAY-STATE after editing BUFFER."
  (let ((window (plist-get display-state :window))
        (previous-buffer (plist-get display-state :previous-buffer))
        (created-window (plist-get display-state :created-window)))
    (when (and (window-live-p window)
               (eq (window-buffer window) buffer))
      (cond
       (created-window
       (quit-window nil window))
       ((buffer-live-p previous-buffer)
        (set-window-buffer window previous-buffer))
        (t
         (quit-window nil window))))))

(defun ai-code-editor-viewport--buffer-text (buffer)
  "Return BUFFER's accessible text without properties."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ai-code-editor-viewport--source-session-name (source-buffer)
  "Return the user-facing session name for SOURCE-BUFFER."
  (if (buffer-live-p source-buffer)
      (let ((name (buffer-name source-buffer)))
        (if (and (string-prefix-p "*" name)
                 (string-suffix-p "*" name))
            (substring name 1 -1)
          name))
    "CLI"))

(defun ai-code-editor-viewport--source-lines-match-draft-p
    (source-start draft-lines)
  "Return non-nil when DRAFT-LINES appear at SOURCE-START in this buffer."
  (save-excursion
    (goto-char source-start)
    (let ((remaining draft-lines)
          (matched t))
      (while (and matched remaining)
        (let* ((draft-line (pop remaining))
               (source-line
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
          (setq matched
                (if (string-empty-p draft-line)
                    (string-blank-p source-line)
                  (and (string-search draft-line source-line) t))))
        (when (and matched remaining)
          (let ((previous-line-start (line-beginning-position)))
            (forward-line 1)
            (when (= (line-beginning-position) previous-line-start)
              (setq matched nil)))))
      matched)))

(defun ai-code-editor-viewport--draft-cursor-offset-at-source
    (draft cursor-position)
  "Map CURSOR-POSITION in this source buffer to an offset within DRAFT."
  (save-excursion
    (goto-char cursor-position)
    (let* ((source-line-start (line-beginning-position))
           (source-line
            (buffer-substring-no-properties
             source-line-start (line-end-position)))
           (cursor-column (- cursor-position source-line-start))
           (draft-lines (split-string draft "\n" nil))
           (draft-offset 0)
           candidates)
      (cl-loop
       for draft-line in draft-lines
       for draft-index from 0
       do
       (when-let* ((draft-start
                    (if (string-empty-p draft-line)
                        (and (string-blank-p source-line)
                             cursor-column)
                      (string-search draft-line source-line))))
         (when (<= draft-start cursor-column
                   (+ draft-start (length draft-line)))
           (let ((source-start
                  (save-excursion
                    (goto-char source-line-start)
                    (when (zerop (forward-line (- draft-index)))
                      (point)))))
             (when (and source-start
                        (ai-code-editor-viewport--source-lines-match-draft-p
                         source-start draft-lines))
               (push (+ draft-offset (- cursor-column draft-start))
                     candidates)))))
       (setq draft-offset (+ draft-offset (length draft-line) 1)))
      (when (= (length candidates) 1)
        (car candidates)))))

(defun ai-code-editor-viewport--source-draft-cursor-offset
    (source-buffer draft)
  "Return SOURCE-BUFFER's cursor offset within DRAFT, when inferable."
  (when (and (buffer-live-p source-buffer)
             (not (string-empty-p draft)))
    (let ((visible-draft (string-remove-suffix "\n" draft)))
      (unless (string-empty-p visible-draft)
        (with-current-buffer source-buffer
          (let ((cursor-position
                 (ai-code-editor-viewport--source-cursor-position)))
            (when (and (integer-or-marker-p cursor-position)
                       (<= (point-min) cursor-position (point-max)))
              (ai-code-editor-viewport--draft-cursor-offset-at-source
               visible-draft cursor-position))))))))

(defun ai-code-editor-viewport--make-buffer (file source-buffer)
  "Return per-request editing state for FILE and SOURCE-BUFFER."
  (let* ((existing-base-buffer (find-buffer-visiting file))
         (base-buffer (or existing-base-buffer (find-file-noselect file)))
         (owned-base-buffer (unless existing-base-buffer base-buffer))
         (snapshot (ai-code-editor-viewport--buffer-text base-buffer))
         (name (generate-new-buffer-name
                (format "Edit: %s"
                        (ai-code-editor-viewport--source-session-name
                         source-buffer))))
         (viewport-buffer
          (with-current-buffer base-buffer
            (let ((buffer-file-name nil)
                  (buffer-file-truename nil)
                  (buffer-auto-save-file-name nil))
              (clone-buffer name nil)))))
    (with-current-buffer viewport-buffer
      (setq ai-code-editor-viewport--source-directory
            (if (buffer-live-p source-buffer)
                (buffer-local-value 'default-directory source-buffer)
              default-directory))
      (set-buffer-modified-p nil))
    (list :base-buffer base-buffer
          :owned-base-buffer owned-base-buffer
          :viewport-buffer viewport-buffer
          :snapshot snapshot)))

(defun ai-code-editor-viewport--commit-buffer
    (base-buffer viewport-buffer snapshot)
  "Save VIEWPORT-BUFFER changes to BASE-BUFFER if it still matches SNAPSHOT."
  (unless (buffer-live-p base-buffer)
    (user-error "The file buffer was closed while its viewport was active"))
  (unless (equal snapshot
                 (ai-code-editor-viewport--buffer-text base-buffer))
    (user-error "The file changed while its viewport was active"))
  (let ((contents
         (ai-code-editor-viewport-attachments-serialize-buffer
          viewport-buffer))
        change-group)
    (with-current-buffer base-buffer
      (setq change-group (prepare-change-group))
      (unwind-protect
          (progn
            (activate-change-group change-group)
            (save-restriction
              (widen)
              (erase-buffer)
              (insert contents))
            (save-buffer)
            (accept-change-group change-group)
            (setq change-group nil))
        (when change-group
          (cancel-change-group change-group))))))

(defun ai-code-editor-viewport--edit-file (file source-buffer &optional line column)
  "Edit FILE in a viewport associated with SOURCE-BUFFER.
LINE is one-based and COLUMN is zero-based, matching editor arguments."
  (let* ((state (ai-code-editor-viewport--make-buffer file source-buffer))
         (base-buffer (plist-get state :base-buffer))
         (owned-base-buffer (plist-get state :owned-base-buffer))
         (buffer (plist-get state :viewport-buffer))
         (snapshot (plist-get state :snapshot))
         (source-cursor-offset
          (ai-code-editor-viewport--source-draft-cursor-offset
           source-buffer snapshot))
         (display-state nil)
         (source-input-overlay nil)
         (finished nil))
    (unwind-protect
        (with-current-buffer buffer
          (setq ai-code-editor-viewport--outcome nil)
          (ai-code-editor-viewport-mode 1)
          (cond
           (line
            (goto-char (point-min))
            (forward-line (1- line))
            (when column
              (move-to-column column)))
           (source-cursor-offset
            (goto-char
             (min (point-max)
                  (+ (point-min) source-cursor-offset)))))
          (setq display-state
                (ai-code-editor-viewport--display buffer source-buffer))
          (unwind-protect
              (progn
                (setq source-input-overlay
                      (ai-code-editor-viewport--disable-source-input
                       source-buffer snapshot source-cursor-offset))
                (recursive-edit)
                (when (eq ai-code-editor-viewport--outcome 'finished)
                  (ai-code-editor-viewport--commit-buffer
                   base-buffer buffer snapshot)
                  (setq finished t)))
            (when (overlayp source-input-overlay)
              (delete-overlay source-input-overlay))
            (ai-code-editor-viewport-mode -1)
            (ai-code-editor-viewport--restore-window
             display-state buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (and (buffer-live-p owned-base-buffer)
                 (not (buffer-modified-p owned-base-buffer)))
        (kill-buffer owned-base-buffer)))
    finished))

(defun ai-code-editor-viewport--parse-file-arguments (directory arguments)
  "Parse editor ARGUMENTS relative to DIRECTORY.
Return plists containing :file, :line, and :column entries."
  (let ((accept-options t)
        (next-line nil)
        (next-column nil)
        files)
    (dolist (argument arguments)
      (cond
       ((and accept-options (string= argument "--"))
        (setq accept-options nil))
       ((and accept-options
             (string-match
              "\\`+\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\'"
              argument))
        (setq next-line (string-to-number (match-string 1 argument))
              next-column (and (match-string 2 argument)
                               (string-to-number (match-string 2 argument)))))
       ((and accept-options (string-prefix-p "-" argument)))
       (t
        (push (list :file (expand-file-name argument directory)
                    :line next-line
                    :column next-column)
              files)
        (setq next-line nil
              next-column nil))))
    (nreverse files)))

(defun ai-code-editor-viewport--edit-files (source-buffer directory file-arguments)
  "Edit FILE-ARGUMENTS for SOURCE-BUFFER relative to DIRECTORY.
Return non-nil only when every file was saved and finished."
  (when-let* ((files
               (ai-code-editor-viewport--parse-file-arguments
                directory file-arguments)))
    (cl-every
     (lambda (file-request)
       (ai-code-editor-viewport--edit-file
        (plist-get file-request :file)
        source-buffer
        (plist-get file-request :line)
        (plist-get file-request :column)))
     files)))

;;;; Terminal request protocol

(defun ai-code-editor-viewport--decode-request (payload)
  "Decode terminal editor PAYLOAD into a request plist.
The plist records the response file, directory, submit intent, and arguments."
  (let* ((decoded
          (decode-coding-string
           (base64-decode-string payload)
           'utf-8
           t))
         (fields (split-string decoded "\0" nil)))
    (when (and fields (string-empty-p (car (last fields))))
      (setq fields (butlast fields)))
    (unless (>= (length fields) 4)
      (error "Malformed AI Code editor request"))
    (unless (member (nth 2 fields) '("0" "1"))
      (error "Invalid AI Code editor submit intent"))
    (list :status-file (nth 0 fields)
          :directory (nth 1 fields)
          :submit-p (string= (nth 2 fields) "1")
          :arguments (nthcdr 3 fields))))

(defun ai-code-editor-viewport--valid-status-file-p (file)
  "Return non-nil when FILE is a helper-created response file."
  (and (stringp file)
       (file-name-absolute-p file)
       (file-regular-p file)
       (not (file-symlink-p file))
       (string-prefix-p "ai-code-editor-status-"
                        (file-name-nondirectory file))
       (file-in-directory-p
        (file-truename file)
        (file-name-as-directory
         (file-truename temporary-file-directory)))))

(defun ai-code-editor-viewport--write-status (file status)
  "Write integer STATUS to the helper response FILE."
  (unless (ai-code-editor-viewport--valid-status-file-p file)
    (error "Unsafe AI Code editor response file: %s" file))
  (with-temp-file file
    (insert (number-to-string status) "\n")))

(defun ai-code-editor-viewport--requested-content-nonblank-p
    (directory arguments)
  "Return non-nil when editor ARGUMENTS name nonblank content in DIRECTORY."
  (seq-some
   (lambda (request)
     (condition-case nil
         (with-temp-buffer
           (insert-file-contents (plist-get request :file))
           (not (string-empty-p (string-trim (buffer-string)))))
       (file-error nil)))
   (ai-code-editor-viewport--parse-file-arguments directory arguments)))

(defun ai-code-editor-viewport--submit-source-buffer (source-buffer)
  "Submit restored TUI input in SOURCE-BUFFER, when it is still live."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (when (functionp ai-code-editor-viewport--submit-function)
        (condition-case err
            (funcall ai-code-editor-viewport--submit-function)
          (error
           (message "AI Code editor submit failed: %s"
                    (error-message-string err))))))))

(defun ai-code-editor-viewport--schedule-submit (source-buffer)
  "Schedule submission of restored TUI input in SOURCE-BUFFER."
  (when (and (buffer-live-p source-buffer)
             (buffer-local-value
              'ai-code-editor-viewport--submit-function source-buffer))
    (run-at-time (max 0 ai-code-editor-viewport-submit-delay)
                 nil
                 #'ai-code-editor-viewport--submit-source-buffer
                 source-buffer)))

(defun ai-code-editor-viewport--open-request (source-buffer payload)
  "Open terminal editor PAYLOAD for SOURCE-BUFFER in a viewport.
PAYLOAD contains a response file, working directory, submit intent, and
editor arguments.
Return non-nil after saving every requested file."
  (let (status-file completed succeeded submit-p response-written)
    (unwind-protect
        (condition-case err
            (let* ((request
                    (ai-code-editor-viewport--decode-request payload))
                   (request-directory (plist-get request :directory))
                   (directory
                    (or (and (not (string-empty-p request-directory))
                             request-directory)
                        default-directory))
                   (arguments (plist-get request :arguments)))
              (setq status-file (plist-get request :status-file))
              (setq succeeded
                    (ai-code-editor-viewport--edit-files
                     source-buffer directory arguments))
              (setq submit-p
                    (and (plist-get request :submit-p)
                         succeeded
                         (ai-code-editor-viewport--requested-content-nonblank-p
                          directory arguments)))
              (setq completed t))
          ((error quit)
           (message "AI Code editor viewport failed: %s"
                    (error-message-string err))))
      (when status-file
        (condition-case err
            (progn
              (ai-code-editor-viewport--write-status
               status-file (if completed 0 1))
              (setq response-written t))
          (error
           (message "AI Code editor response failed: %s"
                    (error-message-string err))))))
    (when (and submit-p response-written)
      (ai-code-editor-viewport--schedule-submit source-buffer))
    succeeded))

(provide 'ai-code-editor-viewport)
;;; ai-code-editor-viewport.el ends here
