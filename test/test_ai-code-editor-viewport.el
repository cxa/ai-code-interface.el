;;; test_ai-code-editor-viewport.el --- Tests for editor viewport  -*- lexical-binding: t; -*-

;; Author: realazy
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for editing files requested by native AI CLI sessions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-editor-viewport)

(cl-defmacro ai-code-editor-viewport-test--with-buffer
    ((buffer name) &rest body)
  "Bind BUFFER to a temporary buffer named from NAME while running BODY."
  (declare (indent 1) (debug ((symbolp form) body)))
  `(with-temp-buffer
     (rename-buffer (generate-new-buffer-name ,name))
     (let ((,buffer (current-buffer)))
       ,@body)))

(ert-deftest test-ai-code-editor-viewport--mode-uses-one-yank-key ()
  "Viewport users should paste every supported clipboard type with `C-y'."
  (with-temp-buffer
    (ai-code-editor-viewport-mode 1)
    (should (eq (key-binding (kbd "C-y"))
                #'ai-code-editor-viewport-yank))
    (should-not (key-binding (kbd "C-c C-y")))))

(ert-deftest test-ai-code-editor-viewport--mode-binds-c-g-to-cancel ()
  "The standard quit key should cancel an active editor viewport cleanly."
  (with-temp-buffer
    (ai-code-editor-viewport-mode 1)
    (should (eq (key-binding (kbd "C-g"))
                #'ai-code-editor-viewport-cancel))))

(ert-deftest test-ai-code-editor-viewport--mode-advertises-smart-yank ()
  "The viewport header should describe the single smart paste command."
  (with-temp-buffer
    (ai-code-editor-viewport-mode 1)
    (should
     (equal header-line-format
            (concat
             " C-c C-c: submit  C-g/C-c C-k: cancel"
             "  C-y: paste text, files, or images ")))))

(ert-deftest test-ai-code-editor-viewport--mode-styles-header-shortcuts ()
  "The viewport header should leave layout to Emacs and bold shortcut keys."
  (with-temp-buffer
    (ai-code-editor-viewport-mode 1)
    (let ((header header-line-format))
      (dolist (key '("C-c C-c" "C-g/C-c C-k" "C-y"))
        (let ((start (string-match (regexp-quote key) header)))
          (should start)
          (should
           (eq (get-text-property start 'face header)
               'ai-code-editor-viewport-header-key-face))))
      (dotimes (index (length header))
        (should-not (get-text-property index 'display header)))
      (should-not
       (get-text-property (string-match-p ": submit" header) 'face header))
      (should
       (eq (face-attribute
            'ai-code-editor-viewport-header-key-face :inherit nil t)
           'bold)))))

(ert-deftest test-ai-code-editor-viewport--mode-header-uses-current-bindings ()
  "The viewport header should derive key hints from the current mode map."
  (let ((ai-code-editor-viewport-mode-map
         (copy-keymap ai-code-editor-viewport-mode-map)))
    (define-key ai-code-editor-viewport-mode-map (kbd "C-c C-c") nil)
    (define-key ai-code-editor-viewport-mode-map
                (kbd "C-c s") #'ai-code-editor-viewport-finish)
    (with-temp-buffer
      (ai-code-editor-viewport-mode 1)
      (should (string-match-p "C-c s: submit" header-line-format))
      (should-not (string-match-p "C-c C-c: submit" header-line-format)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-finish-saves-file ()
  "Finishing a viewport edit should save the file and report success."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-source*")
    (let* ((directory (make-temp-file "ai-code-editor-viewport-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (goto-char (point-max))
                         (insert " changed")
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (should-not (get-file-buffer file))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) "original changed"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-preserves-existing-file-buffer ()
  "Editing should not kill a file buffer that was already visiting the file."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-existing-source*")
    (let* ((directory (make-temp-file "ai-code-editor-existing-buffer-" t))
           (file (expand-file-name "prompt.md" directory))
           existing-buffer)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (setq existing-buffer (find-file-noselect file))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (should (buffer-live-p existing-buffer))
            (should (eq (get-file-buffer file) existing-buffer)))
        (when (buffer-live-p existing-buffer)
          (kill-buffer existing-buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-submit-separates-image-and-text ()
  "Submitting should separate an image reference from adjacent text."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[image spacing]*")
    (let* ((directory (make-temp-file "ai-code-editor-image-spacing-" t))
           (file (expand-file-name "prompt.md" directory))
           (image-file (expand-file-name "photo.png" directory))
           (second-image-file (expand-file-name "diagram.png" directory))
           (preview '(image :type png :data "preview")))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" directory))
            (with-temp-file file)
            (with-temp-file image-file
              (insert "png"))
            (with-temp-file second-image-file
              (insert "png"))
            (with-current-buffer source-buffer
              (setq-local default-directory directory))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'gui-get-selection)
                       (lambda (_selection target)
                         (pcase target
                           ('TARGETS '(text/uri-list))
                           ('text/uri-list
                            (mapconcat
                             (lambda (path) (concat "file://" path))
                             (list image-file second-image-file)
                             "\n")))))
                      ((symbol-function 'display-images-p)
                       (lambda (&rest _args) t))
                      ((symbol-function 'image-supported-file-p)
                       (lambda (_file) t))
                      ((symbol-function 'create-image)
                       (lambda (&rest _args) preview))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (call-interactively (key-binding (kbd "C-y")))
                         (goto-char (point-min))
                         (search-forward "\n\n")
                         (delete-region (match-beginning 0) (match-end 0))
                         (goto-char (point-max))
                         (insert "after")
                         (goto-char (point-min))
                         (insert "before")
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string)
                             "before @photo.png @diagram.png after"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-names-buffer-for-session ()
  "A viewport should identify its source session without a temporary filename."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[sloth:main]*")
    (let* ((directory (make-temp-file "ai-code-editor-buffer-name-" t))
           (file (expand-file-name ".tmpABC123.md" directory))
           viewport-name)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq viewport-name (buffer-name))
                         (ai-code-editor-viewport-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (should (equal viewport-name "Edit: codex[sloth:main]")))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-disables-source-input ()
  "An active viewport should replace the source input only visually."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[disabled source]*")
    (let* ((directory (make-temp-file "ai-code-editor-disabled-" t))
           (file (expand-file-name "prompt.md" directory))
           input-position
           source-text)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (with-current-buffer source-buffer
              (insert "history\n› Summarize recent commits"
                      (make-string 80 ?\s)
                      "\n")
              (goto-char (point-min))
              (search-forward "Summarize")
              (setq input-position (match-beginning 0)
                    source-text (buffer-string))
              (goto-char input-position))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (with-current-buffer source-buffer
                           (let ((display
                                  (get-char-property
                                   input-position 'display)))
                             (should
                              (string-prefix-p
                               (concat
                                " Editing in viewport below —"
                                " C-c C-c: submit,"
                                " C-g/C-c C-k: cancel")
                               (substring-no-properties display)))
                             (should (= (string-width display) 105)))
                           (should (equal (buffer-string) source-text)))
                         (ai-code-editor-viewport-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (with-current-buffer source-buffer
              (should-not (get-char-property input-position 'display))
              (should (equal (buffer-string) source-text))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--disable-source-input-uses-container-face ()
  "The disabled-input hint should derive any prompt's container background."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*eat[styled source]*")
    (let ((container-face '(:background "gray20"))
          (input-face '(:background "dark green")))
      (insert (propertize "λ" 'face container-face)
              (propertize " prompt" 'face input-face))
      (goto-char (+ (point-min) 2))
      (let* ((overlay
              (ai-code-editor-viewport--disable-source-input source-buffer))
             (display (get-char-property (point) 'display)))
        (unwind-protect
            (progn
              (should (= (overlay-start overlay) (1+ (point-min))))
              (should
               (equal (get-text-property 0 'face display)
                      (list 'ai-code-editor-viewport-source-hint-face
                            container-face))))
          (delete-overlay overlay))))))

(ert-deftest test-ai-code-editor-viewport--disable-source-input-fills-window ()
  "The source hint background should fill the visible input width."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ghostel[filled source hint]*")
    (let ((container-face '(:background "gray20")))
      (insert (propertize "›" 'face container-face) " prompt")
      (goto-char (+ (point-min) 2))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _args) (selected-window)))
                ((symbol-function 'window-body-width)
                 (lambda (&rest _args) 100)))
        (let* ((overlay
                (ai-code-editor-viewport--disable-source-input source-buffer))
               (display (get-char-property (point) 'display)))
          (unwind-protect
              (progn
                (should (= (string-width display) 99))
                (should
                 (equal (get-text-property (1- (length display))
                                           'face display)
                        (list 'ai-code-editor-viewport-source-hint-face
                              container-face))))
            (delete-overlay overlay)))))))

(ert-deftest test-ai-code-editor-viewport--disable-source-input-describes-replace-window ()
  "The source hint should describe a viewport replacing its source window."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[replace hint]*")
    (insert "› prompt")
    (goto-char (point-min))
    (forward-char 2)
    (let* ((ai-code-editor-viewport-window-placement 'replace)
           (overlay
            (ai-code-editor-viewport--disable-source-input source-buffer))
           (display (get-char-property (point) 'display)))
      (unwind-protect
          (should
           (equal (substring-no-properties display)
                  (concat
                   " Editing in current window —"
                   " C-c C-c: submit, C-g/C-c C-k: cancel")))
        (delete-overlay overlay)))))

(ert-deftest test-ai-code-editor-viewport--disable-source-input-uses-current-bindings ()
  "The source hint should reflect the viewport mode's current bindings."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[custom source hint]*")
    (insert "› prompt")
    (goto-char (point-min))
    (forward-char 2)
    (let ((ai-code-editor-viewport-mode-map
           (copy-keymap ai-code-editor-viewport-mode-map)))
      (define-key ai-code-editor-viewport-mode-map (kbd "C-c C-c") nil)
      (define-key ai-code-editor-viewport-mode-map
                  (kbd "C-c s")
                  #'ai-code-editor-viewport-finish)
      (let* ((overlay
              (ai-code-editor-viewport--disable-source-input source-buffer))
             (display (get-char-property (point) 'display)))
        (unwind-protect
            (progn
              (should
               (string-match-p "C-c s: submit"
                               (substring-no-properties display)))
              (should-not
               (string-match-p "C-c C-c: submit"
                               (substring-no-properties display))))
          (delete-overlay overlay))))))

(ert-deftest test-ai-code-editor-viewport--edit-files-cancel-rolls-back-file ()
  "Canceling a viewport edit should roll back changes and report cancellation."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-cancel-source*")
    (let* ((directory (make-temp-file "ai-code-editor-viewport-cancel-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (erase-buffer)
                         (insert "discard me")
                         (ai-code-editor-viewport-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) "original"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--open-request-preserves-arguments ()
  "A terminal request should decode its status, directory, and arguments."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[path with spaces]*")
    (let* ((status-file (make-temp-file "ai-code-editor-status-"))
           (directory "/tmp/project with spaces/")
           (arguments '("+12:3" "draft's prompt.md"))
           (fields (append (list status-file directory "1") arguments))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t))
           captured)
      (unwind-protect
          (cl-letf (((symbol-function 'ai-code-editor-viewport--edit-files)
                     (lambda (source seen-directory seen-arguments)
                       (setq captured
                             (list source seen-directory seen-arguments))
                       t)))
            (should (ai-code-editor-viewport--open-request
                     source-buffer payload))
            (should (equal captured
                           (list source-buffer directory arguments)))
            (with-temp-buffer
              (insert-file-contents status-file)
              (should (equal (buffer-string) "0\n"))))
        (when (file-exists-p status-file)
          (delete-file status-file))))))

(ert-deftest test-ai-code-editor-viewport--open-request-finish-submits-source ()
  "Finishing an editor request should submit the restored terminal input."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[submit editor]*")
    (let* ((directory (make-temp-file "ai-code-editor-submit-" t))
           (status-file (make-temp-file "ai-code-editor-status-"))
           (file (expand-file-name "prompt.md" directory))
           (fields (list status-file directory "1" file))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t))
           (ai-code-editor-viewport-submit-delay 0)
           submitted-buffer)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (with-current-buffer source-buffer
              (setq-local
               ai-code-editor-viewport--submit-function
               (lambda ()
                 (setq submitted-buffer (current-buffer)))))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (goto-char (point-max))
                         (insert " ready")
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat function &rest arguments)
                         (apply function arguments)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--open-request
                source-buffer payload)))
            (should (eq submitted-buffer source-buffer)))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (when (file-exists-p status-file)
          (delete-file status-file))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--open-request-general-editor-skips-submit ()
  "A general editor request should save without submitting terminal input."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[git editor]*")
    (let* ((directory (make-temp-file "ai-code-editor-general-" t))
           (status-file (make-temp-file "ai-code-editor-status-"))
           (file (expand-file-name "COMMIT_EDITMSG" directory))
           (fields (list status-file directory "0" file))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t))
           scheduled)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "Commit message"))
            (with-current-buffer source-buffer
              (setq-local ai-code-editor-viewport--submit-function #'ignore))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--edit-files)
                       (lambda (&rest _args) t))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _arguments)
                         (setq scheduled t))))
              (should
               (ai-code-editor-viewport--open-request source-buffer payload))
              (should-not scheduled)))
        (when (file-exists-p status-file)
          (delete-file status-file))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--open-request-blank-skips-submit ()
  "Finishing with only whitespace should not submit the restored input."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[blank editor]*")
    (let* ((directory (make-temp-file "ai-code-editor-blank-" t))
           (status-file (make-temp-file "ai-code-editor-status-"))
           (file (expand-file-name "prompt.md" directory))
           (fields (list status-file directory "1" file))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t))
           (ai-code-editor-viewport-submit-delay 0)
           scheduled
           submitted)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "draft"))
            (with-current-buffer source-buffer
              (setq-local ai-code-editor-viewport--submit-function
                          (lambda () (setq submitted t))))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (erase-buffer)
                         (insert " \n\t ")
                         (ai-code-editor-viewport-finish)))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _arguments)
                         (setq scheduled t)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should
               (ai-code-editor-viewport--open-request
                source-buffer payload)))
            (should-not scheduled)
            (should-not submitted)
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) " \n\t ")))
            (with-temp-buffer
              (insert-file-contents status-file)
              (should (equal (buffer-string) "0\n"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (when (file-exists-p status-file)
          (delete-file status-file))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--open-request-cancel-returns-cleanly ()
  "Canceling should restore the TUI without saving or submitting input."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[cancel editor]*")
    (let* ((status-file (make-temp-file "ai-code-editor-status-"))
           (fields (list status-file default-directory "1" "prompt.md"))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t))
           (ai-code-editor-viewport-submit-delay 0)
           scheduled
           submitted)
      (unwind-protect
          (progn
            (with-current-buffer source-buffer
              (setq-local ai-code-editor-viewport--submit-function
                          (lambda () (setq submitted t))))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--edit-files)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _arguments)
                         (setq scheduled t))))
              (should-not
               (ai-code-editor-viewport--open-request source-buffer payload))
              (should-not scheduled)
              (should-not submitted)
              (with-temp-buffer
                (insert-file-contents status-file)
                (should (equal (buffer-string) "0\n")))))
        (when (file-exists-p status-file)
          (delete-file status-file))))))

(ert-deftest test-ai-code-editor-viewport--open-request-error-reports-failure ()
  "A real viewport error should still make the editor helper fail."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[broken editor]*")
    (let* ((status-file (make-temp-file "ai-code-editor-status-"))
           (fields (list status-file default-directory "1" "prompt.md"))
           (payload
            (base64-encode-string
             (concat (mapconcat #'identity fields "\0") "\0")
             t)))
      (unwind-protect
          (cl-letf (((symbol-function 'ai-code-editor-viewport--edit-files)
                     (lambda (&rest _args)
                       (error "Broken editor"))))
            (should-not
             (ai-code-editor-viewport--open-request source-buffer payload))
            (with-temp-buffer
              (insert-file-contents status-file)
              (should (equal (buffer-string) "1\n"))))
        (when (file-exists-p status-file)
          (delete-file status-file))))))

(ert-deftest test-ai-code-editor-viewport--edit-files-positions-point ()
  "An editor-style +LINE:COLUMN argument should position the viewport."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-position-source*")
    (let* ((directory (make-temp-file "ai-code-editor-position-" t))
           (file (expand-file-name "prompt.md" directory))
           seen-position)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "first\nsecond line\n"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq seen-position
                               (list (line-number-at-pos) (current-column)))
                         (ai-code-editor-viewport-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list "+2:3" "prompt.md"))))
            (should (equal seen-position '(2 3))))
        (dolist (visited-file (list file (expand-file-name "+2:3" directory)))
          (when-let* ((buffer (get-file-buffer visited-file)))
            (set-buffer-modified-p nil)
            (kill-buffer buffer)))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--valid-status-file-p-is-scoped ()
  "Only regular helper status files in Emacs's temp directory are valid."
  (let* ((directory (make-temp-file "ai-code-editor-status-dir-" t))
         (other-directory (make-temp-file "ai-code-editor-other-dir-" t))
         (temporary-file-directory directory)
         (valid-file (make-temp-file "ai-code-editor-status-"))
         (wrong-name (make-temp-file "unrelated-status-"))
         (outside-file
          (make-temp-file
           (expand-file-name "ai-code-editor-status-" other-directory))))
    (unwind-protect
        (progn
          (should
           (ai-code-editor-viewport--valid-status-file-p valid-file))
          (should-not
           (ai-code-editor-viewport--valid-status-file-p wrong-name))
          (should-not
           (ai-code-editor-viewport--valid-status-file-p outside-file)))
      (delete-directory directory t)
      (delete-directory other-directory t))))

(ert-deftest test-ai-code-editor-viewport--display-defaults-below-source-window ()
  "A viewport should open below its visible source window by default."
  (should (eq (default-value 'ai-code-editor-viewport-window-placement)
              'below))
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-below-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-below-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer source-buffer)
        (let* ((source-window (selected-window))
               (display-state
                (ai-code-editor-viewport--display
                 viewport-buffer source-buffer))
               (viewport-window (plist-get display-state :window)))
          (should (window-live-p viewport-window))
          (should-not (eq viewport-window source-window))
          (should (eq (window-buffer source-window) source-buffer))
          (should (eq (window-buffer viewport-window) viewport-buffer))
          (should (>= (nth 1 (window-edges viewport-window))
                      (nth 3 (window-edges source-window))))
          (ai-code-editor-viewport--restore-window
           display-state viewport-buffer)
          (should (window-live-p source-window))
          (should-not (window-live-p viewport-window)))))))

(ert-deftest test-ai-code-editor-viewport--display-opens-below-side-window ()
  "A viewport should open below an AI session shown in a side window."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-side-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-side-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (let* ((window-sides-slots '(nil nil nil nil))
               (source-window
                (display-buffer-in-side-window
                 source-buffer
                 '((side . right)
                   (slot . 0)
                   (window-width . 40)
                   (preserve-size . (t . nil))
                   (window-parameters
                    . ((no-delete-other-windows . t)
                       (window-size-fixed . width))))))
               (display-state
                (ai-code-editor-viewport--display
                 viewport-buffer source-buffer))
               (viewport-window (plist-get display-state :window)))
          (should (window-live-p viewport-window))
          (should-not (eq viewport-window source-window))
          (should (eq (window-buffer source-window) source-buffer))
          (should (eq (window-buffer viewport-window) viewport-buffer))
          (should (eq (window-parameter viewport-window 'window-side) 'right))
          (should (>= (nth 1 (window-edges viewport-window))
                      (nth 3 (window-edges source-window))))
          (ai-code-editor-viewport--restore-window
           display-state viewport-buffer)
          (should (window-live-p source-window))
          (should-not (window-live-p viewport-window)))))))

(ert-deftest test-ai-code-editor-viewport--display-below-errors-without-space ()
  "Below placement should never replace the source window as a fallback."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-no-space-source*")
    (ai-code-editor-viewport-test--with-buffer
        (viewport-buffer "*ai-code-editor-no-space-viewport*")
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer source-buffer)
        (let ((ai-code-editor-viewport-window-placement 'below))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (&rest _args) nil)))
            (should-error
             (ai-code-editor-viewport--display
              viewport-buffer source-buffer)
             :type 'user-error))
          (should (eq (window-buffer (selected-window)) source-buffer)))))))

(ert-deftest test-ai-code-editor-viewport--display-replace-restores-hidden-source-window ()
  "Replacement placement should restore the user's previous window buffer."
  (ai-code-editor-viewport-test--with-buffer
      (user-buffer "*ai-code-editor-user*")
    (ai-code-editor-viewport-test--with-buffer
        (source-buffer "*ai-code-editor-hidden-source*")
      (ai-code-editor-viewport-test--with-buffer
          (viewport-buffer "*ai-code-editor-hidden-viewport*")
        (switch-to-buffer user-buffer)
        (let* ((ai-code-editor-viewport-window-placement 'replace)
               (display-state
               (ai-code-editor-viewport--display
                viewport-buffer source-buffer)))
          (should (eq (window-buffer (selected-window)) viewport-buffer))
          (ai-code-editor-viewport--restore-window
           display-state viewport-buffer)
          (should (eq (window-buffer (selected-window)) user-buffer)))))))

(ert-deftest test-ai-code-editor-viewport--edit-file-isolates-request-state ()
  "Nested editor requests for one file should use independent viewport buffers."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-isolation-source*")
    (let* ((directory (make-temp-file "ai-code-editor-isolation-" t))
           (file (expand-file-name "prompt.md" directory))
           first-viewport
           second-viewport
           inner-result
           (depth 0))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq depth (1+ depth))
                         (if (= depth 1)
                             (progn
                               (setq first-viewport (current-buffer))
                               (setq inner-result
                                     (ai-code-editor-viewport--edit-file
                                      file source-buffer))
                               (should ai-code-editor-viewport-mode)
                               (ai-code-editor-viewport-finish))
                           (setq second-viewport (current-buffer))
                           (ai-code-editor-viewport-cancel)))))
              (should (ai-code-editor-viewport--edit-file file source-buffer)))
            (should-not inner-result)
            (should-not (eq first-viewport second-viewport)))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-restores-source-cursor-position ()
  "A viewport should open at the source TUI's cursor within its draft."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[cursor position]*")
    (let* ((directory (make-temp-file "ai-code-editor-cursor-" t))
           (file (expand-file-name "prompt.md" directory))
           viewport-point)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "alpha beta gamma"))
            (with-current-buffer source-buffer
              (insert "history\n› alpha beta gamma" (make-string 20 ?\s))
              (goto-char (point-min))
              (search-forward "alpha "))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq viewport-point (point))
                         (should (looking-at-p "beta"))
                         (ai-code-editor-viewport-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file))))
            (should (= viewport-point 7)))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-restores-multiline-cursor-position ()
  "A viewport should map a source cursor within a multiline draft."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[multiline cursor]*")
    (let* ((directory (make-temp-file "ai-code-editor-multiline-cursor-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "first line\nsecond line\nthird"))
            (with-current-buffer source-buffer
              (insert "› first line\n  second line\n  third")
              (goto-char (point-min))
              (search-forward "second "))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (should (= (point) 19))
                         (should (looking-at-p "line"))
                         (ai-code-editor-viewport-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file)))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-disambiguates-repeated-draft-lines ()
  "A viewport should use source context when draft lines repeat."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[repeated draft lines]*")
    (let* ((directory (make-temp-file "ai-code-editor-repeated-lines-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "same\nsame"))
            (with-current-buffer source-buffer
              (insert "› same\n  same")
              (goto-char (point-min))
              (forward-line 1)
              (search-forward "sa"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (should (= (point) 8))
                         (should (looking-at-p "me"))
                         (ai-code-editor-viewport-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file)))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-restores-cursor-on-empty-draft-line ()
  "A viewport should restore a cursor on an empty multiline draft line."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[empty draft line]*")
    (let* ((directory (make-temp-file "ai-code-editor-empty-line-" t))
           (file (expand-file-name "prompt.md" directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "first\n\nthird"))
            (with-current-buffer source-buffer
              (insert "› first\n  \n  third")
              (goto-char (point-min))
              (forward-line 1)
              (end-of-line))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (should (= (point) 7))
                         (should (looking-at-p "\nthird"))
                         (ai-code-editor-viewport-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file)))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-files-uses-source-cursor-function ()
  "A viewport should use its terminal adapter's live cursor function."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*codex[ghostel cursor]*")
    (let* ((directory (make-temp-file "ai-code-editor-ghostel-cursor-" t))
           (file (expand-file-name "prompt.md" directory))
           terminal-cursor)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "alpha beta gamma"))
            (with-current-buffer source-buffer
              (insert "› alpha beta gamma" (make-string 20 ?\s)
                      "\nterminal status")
              (goto-char (point-min))
              (search-forward "alpha ")
              (setq terminal-cursor (point))
              (setq-local ai-code-editor-viewport-source-cursor-function
                          (lambda () terminal-cursor))
              (goto-char (point-max)))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (should (= (point) 7))
                         (should (looking-at-p "beta"))
                         (ai-code-editor-viewport-cancel)))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil)))
              (should-not
               (ai-code-editor-viewport--edit-files
                source-buffer directory (list file)))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest test-ai-code-editor-viewport--edit-file-cancel-does-not-leak-through-nested-save ()
  "Canceling one request should not let another request save its edits."
  (ai-code-editor-viewport-test--with-buffer
      (source-buffer "*ai-code-editor-transactions-source*")
    (let* ((directory (make-temp-file "ai-code-editor-transactions-" t))
           (file (expand-file-name "prompt.md" directory))
           inner-result
           (depth 0))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "original"))
            (cl-letf (((symbol-function 'ai-code-editor-viewport--display)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'exit-recursive-edit)
                       (lambda () nil))
                      ((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq depth (1+ depth))
                         (goto-char (point-max))
                         (if (= depth 1)
                             (progn
                               (insert " OUTER")
                               (setq inner-result
                                     (ai-code-editor-viewport--edit-file
                                      file source-buffer))
                               (ai-code-editor-viewport-cancel))
                           (insert " INNER")
                           (ai-code-editor-viewport-finish)))))
              (should-not
               (ai-code-editor-viewport--edit-file file source-buffer)))
            (should inner-result)
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) "original INNER"))))
        (when-let* ((buffer (get-file-buffer file)))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(provide 'test_ai-code-editor-viewport)
;;; test_ai-code-editor-viewport.el ends here
