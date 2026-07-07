;;; test_ai-code-session-link.el --- Tests for ai-code-session-link -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tests for shared session link helper functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-code-session-link)

(defvar ai-code-backends-infra--session-directory)
(defvar ai-code-backends-infra--session-terminal-backend)

(ert-deftest ai-code-session-link-test-toggle-defaults-enabled ()
  "Session linkification should be enabled by default."
  (should (boundp 'ai-code-session-link-enabled))
  (should ai-code-session-link-enabled))

(ert-deftest ai-code-session-link-test-normalize-file-removes-session-prefixes ()
  "Normalization should trim whitespace and remove session-only prefixes."
  (should (equal (ai-code-session-link--normalize-file " @src/Foo.java ")
                 "src/Foo.java"))
  (should (equal (ai-code-session-link--normalize-file "file:///tmp/project/Foo.java")
                 "/tmp/project/Foo.java"))
  (should (equal (ai-code-session-link--normalize-file "file://localhost/tmp/project/Foo.java")
                 "/tmp/project/Foo.java"))
  (should (equal (ai-code-session-link--normalize-file "file:/tmp/project/Foo.java")
                 "/tmp/project/Foo.java"))
  (should (equal (ai-code-session-link--normalize-file
                  "file:///tmp/project/image-\nwrapped.png")
                 "/tmp/project/image-wrapped.png"))
  (should (equal (ai-code-session-link--normalize-file
                  "<file:///tmp/project/My%20Image.png>")
                 "/tmp/project/My Image.png"))
  (should (equal (ai-code-session-link--normalize-file
                  "./screens/My\\ Image.png")
                 "./screens/My Image.png"))
  (should-not (ai-code-session-link--normalize-file "   ")))

(ert-deftest ai-code-session-link-test-project-files-expands-relative-project-entries ()
  "Project file enumeration should return absolute paths."
  (let* ((root (make-temp-file "ai-code-session-link-project-files-" t))
         (file (expand-file-name "src/Foo.java" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "class Foo {}\n"))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt _dir)
                       'mock-project))
                    ((symbol-function 'project-root)
                     (lambda (_project)
                       root))
                    ((symbol-function 'project-files)
                     (lambda (_project &optional _dirs)
                       '("src/Foo.java"))))
            (should (equal (ai-code-session-link--project-files root)
                           (list file)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-matching-project-files-supports-relative-and-basename ()
  "Matching should support both relative paths and unique basenames."
  (let* ((root (make-temp-file "ai-code-session-link-matching-files-" t))
         (file (expand-file-name "src/Foo.java" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "class Foo {}\n"))
          (should (equal (ai-code-session-link--matching-project-files "./src/Foo.java" root)
                         (list file)))
          (should (equal (ai-code-session-link--matching-project-files "Foo.java" root)
                         (list file))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-file-and-url ()
  "Linkify project files, existing local files, and URLs."
  (let* ((root (make-temp-file "ai-code-session-links-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir))
         (outside-file (expand-file-name "Elsewhere.java" temporary-file-directory)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (with-temp-file outside-file
            (insert "class Elsewhere {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert (format "src/FileABC.java\nsrc/FileABC.java:42\nsrc/FileABC.java:L42-L60\nsrc/FileABC.java#L42-L60\nsrc/FileABC.java:42:7\n%s\nhttps://example.com/path\n"
                            outside-file))
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "src/FileABC\\.java")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java"))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))
            (search-forward-regexp "src/FileABC\\.java:42")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:42"))
            (search-forward-regexp "src/FileABC\\.java:L42-L60")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:L42-L60"))
            (search-forward-regexp "src/FileABC\\.java#L42-L60")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java#L42-L60"))
            (search-forward-regexp "src/FileABC\\.java:42:7")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:42:7"))
            (search-forward-regexp (regexp-quote outside-file))
            (let ((outside-pos (match-beginning 0)))
              (should-not (ai-code-session-link--in-project-file-p outside-file root))
              (should (equal (get-text-property outside-pos 'ai-code-session-link)
                             outside-file))
              (should (eq (get-text-property outside-pos 'face) 'link)))
            (search-forward-regexp "https://example\\.com/path")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "https://example.com/path"))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))
            (erase-buffer)
            (insert "Visit https://example.com/docs, please.")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "https://example\\.com/docs")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "https://example.com/docs"))
            (goto-char (match-end 0))
            (should-not (get-text-property (point) 'ai-code-session-link))))
      (when (file-exists-p outside-file)
        (delete-file outside-file))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-matches-unique-project-basename ()
  "Linkify basename references when they uniquely match a project file."
  (let* ((root (make-temp-file "ai-code-session-links-basename-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "Foo.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class Foo {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "Foo.java:42\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "Foo\\.java:42")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "Foo.java:42"))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-matches-uppercase-project-basename ()
  "Linkify uppercase or mixed-case basename references for project files."
  (let* ((root (make-temp-file "ai-code-session-links-uppercase-basename-" t))
         (src-dir (expand-file-name "src" root))
         (c-file (expand-file-name "Foo.C" src-dir))
         (readme-file (expand-file-name "README.MD" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file c-file
            (insert "int main(void) { return 0; }\n"))
          (with-temp-file readme-file
            (insert "# README\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "Foo.C:12\nREADME.MD\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "Foo\\.C:12")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "Foo.C:12"))
            (search-forward-regexp "README\\.MD")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "README.MD"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-symbol-near-file-link-across-lines ()
  "Linkify a nearby code symbol after a file link across line breaks."
  (let* ((root (make-temp-file "ai-code-session-links-symbol-nearby-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "UserService.java" src-dir))
         (symbol-text "UserService.processRequest()"))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class UserService {\n  void processRequest() {}\n}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "See src/UserService.java:2\n")
            (insert symbol-text)
            (insert "\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward symbol-text)
            (let ((symbol-pos (- (point) (length symbol-text))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             symbol-text))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/UserService.java:2"))
              (should (eq (lookup-key (get-text-property symbol-pos 'keymap) [mouse-1])
                          'ai-code-session-link-navigate-symbol-at-mouse))
              (should (eq (lookup-key (get-text-property symbol-pos 'keymap) (kbd "RET"))
                          'ai-code-session-link-navigate-symbol-at-point))
              (should (eq (get-text-property symbol-pos 'face) 'link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-prefilters-java-camelcase-symbols ()
  "Linkify nearby bare CamelCase symbols for Java files."
  (let* ((root (make-temp-file "ai-code-session-links-java-camelcase-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "UserService.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class UserService {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/UserService.java:1\nUserService\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "UserService")
            (let ((symbol-pos (- (point) (length "UserService"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "UserService"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/UserService.java:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-rejects-simple-java-capitalized-symbols ()
  "Do not linkify nearby bare symbols with only one uppercase character."
  (let* ((root (make-temp-file "ai-code-session-links-java-capitalized-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "Builder.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class Builder {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/Builder.java:1\nBuilder\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "Builder")
            (let ((symbol-pos (- (point) (length "Builder"))))
              (should-not (get-text-property symbol-pos 'ai-code-session-symbol-link))
              (should-not (get-text-property symbol-pos 'ai-code-session-link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-rejects-adjacent-uppercase-symbols ()
  "Do not linkify nearby bare symbols whose uppercase letters are only adjacent."
  (let* ((root (make-temp-file "ai-code-session-links-adjacent-uppercase-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "xml_parser.py" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class XMLParser:\n    pass\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/xml_parser.py:1\nXMLParser\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "XMLParser")
            (let ((symbol-pos (- (point) (length "XMLParser"))))
              (should-not (get-text-property symbol-pos 'ai-code-session-symbol-link))
              (should-not (get-text-property symbol-pos 'ai-code-session-link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-allows-camelcase-symbols-outside-java ()
  "Linkify nearby bare CamelCase symbols without restricting them to Java files."
  (let* ((root (make-temp-file "ai-code-session-links-camelcase-generic-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "builder.py" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class RequestBuilder:\n    pass\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/builder.py:1\nRequestBuilder\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "RequestBuilder")
            (let ((symbol-pos (- (point) (length "RequestBuilder"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "RequestBuilder"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/builder.py:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-prefilters-python-snake-case-symbols ()
  "Linkify nearby bare snake_case symbols for Python files."
  (let* ((root (make-temp-file "ai-code-session-links-python-snake-case-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "user_service.py" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "def process_request():\n    return None\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/user_service.py:1\nprocess_request\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "process_request")
            (let ((symbol-pos (- (point) (length "process_request"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "process_request"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/user_service.py:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-allows-snake-case-symbols-outside-python ()
  "Linkify nearby bare snake_case symbols without restricting them to Python files."
  (let* ((root (make-temp-file "ai-code-session-links-snake-case-generic-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "Builder.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class Builder {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/Builder.java:1\nprocess_request\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "process_request")
            (let ((symbol-pos (- (point) (length "process_request"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "process_request"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/Builder.java:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-prefilters-elisp-hyphen-symbols ()
  "Linkify code-like Elisp symbols while skipping nearby prose hyphen words."
  (let* ((root (make-temp-file "ai-code-session-links-elisp-symbols-" t))
         (lisp-dir (expand-file-name "lisp" root))
         (file (expand-file-name "feature.el" lisp-dir)))
    (unwind-protect
        (progn
          (make-directory lisp-dir t)
          (with-temp-file file
            (insert "(setq-local foo t)\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "lisp/feature.el:1\nsetq-local\nfollow-up\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "setq-local")
            (let ((symbol-pos (- (point) (length "setq-local"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "setq-local"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "lisp/feature.el:1")))
            (search-forward "follow-up")
            (let ((prose-pos (- (point) (length "follow-up"))))
              (should-not (get-text-property prose-pos 'ai-code-session-symbol-link))
              (should-not (get-text-property prose-pos 'ai-code-session-link)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-symbols-cross-blank-lines ()
  "Linkify nearby symbols even when a blank line appears after the file link."
  (let* ((root (make-temp-file "ai-code-session-links-blank-line-symbols-" t))
         (lisp-dir (expand-file-name "lisp" root))
         (file (expand-file-name "feature.el" lisp-dir)))
    (unwind-protect
        (progn
          (make-directory lisp-dir t)
          (with-temp-file file
            (insert "(defvar ai-code-session-link-enabled t)\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "lisp/feature.el:1\n\nai-code-session-link-enabled\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "ai-code-session-link-enabled")
            (let ((symbol-pos (- (point) (length "ai-code-session-link-enabled"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "ai-code-session-link-enabled"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "lisp/feature.el:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-uses-fixed-large-symbol-window ()
  "Linkify later nearby symbols within the fixed 512-char and 3-line budget."
  (let* ((root (make-temp-file "ai-code-session-links-extended-symbols-" t))
         (lisp-dir (expand-file-name "lisp" root))
         (file (expand-file-name "feature.el" lisp-dir))
         (later-symbol "ai-code-session-link--linkify-session-region"))
    (unwind-protect
        (progn
          (make-directory lisp-dir t)
          (with-temp-file file
            (insert "(setq-local foo t)\n")
            (insert "(defun ai-code-session-link--linkify-session-region () nil)\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "lisp/feature.el:1\n")
            (dotimes (_ 1)
              (insert "plain prose words only\n"))
            (insert (make-string 80 ?x))
            (insert "\n")
            (insert later-symbol)
            (insert "\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward later-symbol)
            (let ((symbol-pos (- (point) (length later-symbol))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             later-symbol))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "lisp/feature.el:1")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-avoids-eager-file-resolution-for-nearby-symbols ()
  "Linkify nearby symbols without resolving file paths during redraw."
  (let* ((root (make-temp-file "ai-code-session-links-symbol-perf-" t))
         (resolved-paths
          `(("src/user_service.py" . ,(expand-file-name "src/user_service.py" root))
            ("src/next_file.py" . ,(expand-file-name "src/next_file.py" root))))
         (resolve-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'ai-code-session-link--resolve-session-file)
                   (lambda (path)
                     (cl-incf resolve-count)
                     (cdr (assoc path resolved-paths)))))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/user_service.py:1\nprocess_request\nsrc/next_file.py:1\nRequestBuilder\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "process_request")
            (let ((symbol-pos (- (point) (length "process_request"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "process_request"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/user_service.py:1")))
            (search-forward "RequestBuilder")
            (let ((symbol-pos (- (point) (length "RequestBuilder"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "RequestBuilder"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "src/next_file.py:1")))
            (should (zerop resolve-count))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-avoids-project-files-for-basename-links ()
  "Avoid project file enumeration while linkifying basename references."
  (let* ((root (make-temp-file "ai-code-session-links-project-cache-" t))
         (project-files-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current)
                   (lambda (&optional _maybe-prompt _dir)
                     'mock-project))
                  ((symbol-function 'project-root)
                   (lambda (_project)
                     root))
                  ((symbol-function 'project-files)
                   (lambda (_project &optional _dirs)
                     (cl-incf project-files-count)
                     '("src/UserService.java" "src/Builder.java"))))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "UserService.java:1\nUserService\nBuilder.java:1\nRequestBuilder\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (search-forward "UserService")
            (let ((symbol-pos (- (point) (length "UserService"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "UserService"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "UserService.java:1")))
            (forward-line 2)
            (search-forward "RequestBuilder")
            (let ((symbol-pos (- (point) (length "RequestBuilder"))))
              (should (equal (get-text-property symbol-pos 'ai-code-session-symbol-link)
                             "RequestBuilder"))
              (should (equal (get-text-property symbol-pos 'ai-code-session-link)
                             "Builder.java:1")))
            (should (zerop project-files-count))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-skips-project-files-across-passes ()
  "Repeated relinkify passes should avoid project file enumeration."
  (let* ((root (make-temp-file "ai-code-session-links-project-cache-passes-" t))
         (project-files-count 0)
         (ai-code-session-link-enabled t))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current)
                   (lambda (&optional _maybe-prompt _dir)
                     'mock-project))
                  ((symbol-function 'project-root)
                   (lambda (_project)
                     root))
                  ((symbol-function 'project-files)
                   (lambda (_project &optional _dirs)
                     (cl-incf project-files-count)
                     '("src/UserService.java"))))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "UserService.java:1\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (should (zerop project-files-count))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test--open-file-link-resolves-basename-on-demand ()
  "Basename file links should still resolve when the user activates them."
  (let* ((root (make-temp-file "ai-code-session-links-basename-open-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "Foo.java" src-dir))
         source-buffer)
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class Foo {}\nnext line\n"))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (path)
                       (setq source-buffer (find-file-noselect path))
                       (set-buffer source-buffer)
                       source-buffer)))
            (let ((default-directory root))
              (let ((ai-code-backends-infra--session-directory root))
                (should (ai-code-session-link--open-file-link "Foo.java:2"))
                (should (buffer-live-p source-buffer))
                (with-current-buffer source-buffer
                  (should (equal (buffer-file-name) file))
                  (should (= (line-number-at-pos) 2)))))))
      (when (and source-buffer (buffer-live-p source-buffer))
        (kill-buffer source-buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-skips-unchanged-property-churn ()
  "Repeated linkify should not churn properties for unchanged session text."
  (let* ((root (make-temp-file "ai-code-session-links-stable-region-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir))
         (ai-code-session-link-enabled t))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/FileABC.java:42\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (let ((add-count 0)
                  (remove-count 0)
                  (orig-add (symbol-function 'add-text-properties))
                  (orig-remove (symbol-function 'remove-text-properties)))
              (cl-letf (((symbol-function 'add-text-properties)
                         (lambda (start end props &optional object)
                           (cl-incf add-count)
                           (funcall orig-add start end props object)))
                        ((symbol-function 'remove-text-properties)
                         (lambda (start end props &optional object)
                           (cl-incf remove-count)
                           (funcall orig-remove start end props object))))
                (ai-code-session-link--linkify-session-region (point-min) (point-max)))
              (should (zerop add-count))
              (should (zerop remove-count)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-navigate-symbol-at-point-falls-back-to-associated-file ()
  "Symbol navigation should fall back to the nearby file and move to the symbol."
  (let* ((root (make-temp-file "ai-code-session-links-symbol-nav-" t))
         (lisp-dir (expand-file-name "lisp" root))
         (file (expand-file-name "feature.el" lisp-dir))
         source-buffer navigated-buffer navigated-point)
    (unwind-protect
        (progn
          (make-directory lisp-dir t)
          (with-temp-file file
            (insert "(setq-local foo t)\n"))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (path)
                       (setq source-buffer (find-file-noselect path))
                       (set-buffer source-buffer)
                       source-buffer))
                    ((symbol-function 'xref-find-definitions)
                     (lambda (_identifier)
                       (error "Xref unavailable")))
                    ((symbol-function 'message)
                     (lambda (&rest _args) nil)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (insert "lisp/feature.el:1\nsetq-local\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (goto-char (point-min))
              (search-forward "setq-local")
              (goto-char (- (point) (length "setq-local")))
              (should (ai-code-session-link-navigate-symbol-at-point))
              (setq navigated-buffer (current-buffer)
                    navigated-point (point))))
          (should (buffer-live-p source-buffer))
          (should (eq navigated-buffer source-buffer))
          (with-current-buffer source-buffer
            (should (equal (buffer-file-name) file))
            (should (= navigated-point (point)))
            (should (looking-at "setq-local"))))
      (when (and source-buffer (buffer-live-p source-buffer))
        (kill-buffer source-buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-navigate-symbol-at-point-falls-back-to-helm-gtags ()
  "Symbol navigation should try helm-gtags after xref fails."
  (let* ((root (make-temp-file "ai-code-session-links-symbol-gtags-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "UserService.java" src-dir))
         source-buffer gtags-symbol)
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class UserService {\n  void processRequest() {}\n}\n"))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (path)
                       (setq source-buffer (find-file-noselect path))
                       (set-buffer source-buffer)
                       source-buffer))
                    ((symbol-function 'xref-find-definitions)
                     (lambda (_identifier)
                       (error "Xref unavailable")))
                    ((symbol-function 'helm-gtags-find-tag)
                     (lambda (identifier)
                       (setq gtags-symbol identifier)
                       t))
                    ((symbol-function 'message)
                     (lambda (&rest _args) nil)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (insert "src/UserService.java:2\nUserService.processRequest()\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (goto-char (point-min))
              (search-forward "UserService.processRequest()")
              (goto-char (- (point) (length "UserService.processRequest()")))
              (should (ai-code-session-link-navigate-symbol-at-point))))
          (should (buffer-live-p source-buffer))
          (should (equal gtags-symbol "processRequest")))
      (when (and source-buffer (buffer-live-p source-buffer))
        (kill-buffer source-buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-supports-existing-local-file-and-directory ()
  "Linkify existing local file and directory paths, but not missing ones."
  (let* ((root (make-temp-file "ai-code-session-links-local-paths-" t))
         (local-file (expand-file-name "tmp/LocalFile.txt" root))
         (local-dir (expand-file-name "tmp/local-directory" root))
         (missing-file (expand-file-name "tmp/MissingFile.txt" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory local-file) t)
          (make-directory local-dir t)
          (with-temp-file local-file
            (insert "local file\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory
                        (expand-file-name "project" root))
            (insert (format "%s:12\n%s\n%s:9\n"
                            local-file
                            local-dir
                            missing-file))
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp (concat (regexp-quote local-file) ":12"))
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           (format "%s:12" local-file)))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))
            (search-forward-regexp (regexp-quote local-dir))
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           local-dir))
            (should (eq (get-text-property (match-beginning 0) 'face) 'link))
            (search-forward-regexp (concat (regexp-quote missing-file) ":9"))
            (should-not (get-text-property (match-beginning 0) 'ai-code-session-link))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-read-only-text ()
  "Linkification should work on read-only terminal output."
  (let* ((root (make-temp-file "ai-code-session-links-read-only-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/FileABC.java:42\nhttps://example.com/path\n")
            (add-text-properties (point-min) (point-max) '(read-only t))
            (should
             (condition-case nil
                 (progn
                   (ai-code-session-link--linkify-session-region (point-min) (point-max))
                   t)
               (text-read-only nil)))
            (goto-char (point-min))
            (search-forward-regexp "src/FileABC\\.java:42")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:42"))
            (search-forward-regexp "https://example\\.com/path")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "https://example.com/path"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-linkify-session-region-adds-visible-session-link-properties ()
  "Session links should expose visible link styling and unified navigation data."
  (let* ((root (make-temp-file "ai-code-visible-session-links-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (insert "src/FileABC.java:42\nhttps://example.com/path\n")
            (ai-code-session-link--linkify-session-region (point-min) (point-max))
            (goto-char (point-min))
            (search-forward-regexp "src/FileABC\\.java:42")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "src/FileABC.java:42"))
            (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'link))
            (should (eq (lookup-key (get-text-property (match-beginning 0) 'keymap) [mouse-1])
                        'ai-code-session-navigate-link-at-mouse))
            (should (eq (lookup-key (get-text-property (match-beginning 0) 'keymap) [mouse-2])
                        'ai-code-session-navigate-link-at-mouse))
            (search-forward-regexp "https://example\\.com/path")
            (should (equal (get-text-property (match-beginning 0) 'ai-code-session-link)
                           "https://example.com/path"))
            (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'link))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-disabled-toggle-skips-linkify-and-scheduling ()
  "Disabled session linkification should skip properties and timer scheduling."
  (should (boundp 'ai-code-session-link-enabled))
  (let* ((root (make-temp-file "ai-code-session-links-disabled-" t))
         (src-dir (expand-file-name "src" root))
         (file (expand-file-name "FileABC.java" src-dir)))
    (unwind-protect
        (progn
          (make-directory src-dir t)
          (with-temp-file file
            (insert "class FileABC {}\n"))
          (let ((ai-code-session-link-enabled nil))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (insert "src/FileABC.java:42\nhttps://example.com/path\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (goto-char (point-min))
              (search-forward-regexp "src/FileABC\\.java:42")
              (should-not (get-text-property (match-beginning 0) 'ai-code-session-link))
              (search-forward-regexp "https://example\\.com/path")
              (should-not (get-text-property (match-beginning 0) 'ai-code-session-link)))
            (with-temp-buffer
              (setq ai-code-session-link--pending-tail-width 0
                    ai-code-session-link--linkify-timer nil)
              (ai-code-session-link--schedule-linkify-recent-output
               (current-buffer)
               "src/FileABC.java:42")
              (should-not ai-code-session-link--linkify-timer)
              (should (zerop ai-code-session-link--pending-tail-width)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-adds-overlay ()
  "Ghostel sessions should preview local image file links."
  (let* ((root (make-temp-file "ai-code-session-image-preview-" t))
         (image-file (expand-file-name "screenshot.png" root))
         (created-image nil))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (setq created-image (cons data args))
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "Saved screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (goto-char (point-min))
              (search-forward "screenshot.png")
              (let* ((link-start (match-beginning 0))
                     (link-end (match-end 0))
                     (preview-overlays
                      (cl-remove-if-not
                       (lambda (overlay)
                         (overlay-get overlay 'ai-code-session-image-preview))
                       (overlays-in (point-min) (point-max)))))
                (should (equal (get-text-property (match-beginning 0)
                                                  'ai-code-session-link)
                               "screenshot.png"))
                (should (= (length preview-overlays) 1))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))
                (should (= (overlay-start (car preview-overlays))
                           link-start))
                (should (= (overlay-end (car preview-overlays))
                           link-end))
                (should (equal (car created-image) "fake image bytes"))
                (should-not (equal (car created-image) image-file))
                (should (nth 2 created-image))
                (should (member :max-width created-image))
                (should-not (member :max-height created-image))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-unicode-trailing-punctuation-categories ()
  "Image preview suffix detection should use Unicode punctuation categories."
  (should (ai-code-session-link--unicode-trailing-punctuation-p ?\u3002))
  (should (ai-code-session-link--unicode-trailing-punctuation-p ?\u061f))
  (should (ai-code-session-link--unicode-trailing-punctuation-p ?\uff09))
  (should-not (ai-code-session-link--unicode-trailing-punctuation-p ?/))
  (should-not (ai-code-session-link--unicode-trailing-punctuation-p ?a)))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-follows-trailing-punctuation ()
  "Image previews should appear after sentence punctuation following a path."
  (let* ((root (make-temp-file "ai-code-session-image-preview-punctuation-" t))
         (image-file (expand-file-name "screenshot.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args))))
            (dolist (suffix (list "." (string ?\u3002)))
              (with-temp-buffer
                (setq-local ai-code-backends-infra--session-directory root)
                (setq-local ai-code-backends-infra--session-terminal-backend
                            'ghostel)
                (insert "Saved " image-file suffix "\n")
                (ai-code-session-link--linkify-session-region
                 (point-min) (point-max))
                (goto-char (point-min))
                (search-forward image-file)
                (let* ((link-start (match-beginning 0))
                       (link-end (match-end 0))
                       (preview-overlays
                        (cl-remove-if-not
                         (lambda (overlay)
                           (overlay-get overlay 'ai-code-session-image-preview))
                         (overlays-in (point-min) (point-max))))
                       (overlay (car preview-overlays)))
                  (should (= (length preview-overlays) 1))
                  (should (= (overlay-start overlay) link-start))
                  (should (= (overlay-end overlay) (1+ link-end)))
                  (should (equal (overlay-get
                                  overlay
                                  'ai-code-session-image-display-text)
                                 (concat image-file suffix)))
                  (should (equal (overlay-get
                                  overlay
                                  'ai-code-session-image-link-text)
                                 image-file))
                  (should-not
                   (ai-code-session-link--image-preview-refresh-needed-p
                    (point-min) (point-max))))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-remote-ghostel-generic-linkify-skips-file-stats ()
  "Remote Ghostel generic linkification should not stat TRAMP candidates."
  (let ((remote-root "/mock-remote/tmp/project/")
        (file-exists-called nil)
        (create-image-called nil))
    (cl-letf (((symbol-function 'display-images-p)
               (lambda (&optional _display) t))
              ((symbol-function 'file-remote-p)
               (lambda (file &rest _args)
                 (and (stringp file)
                      (string-prefix-p "/mock-remote/" file))))
              ((symbol-function 'file-exists-p)
               (lambda (_file)
                 (setq file-exists-called t)
                 nil))
              ((symbol-function 'create-image)
               (lambda (&rest _args)
                 (setq create-image-called t)
                 'image)))
      (with-temp-buffer
        (setq-local ai-code-backends-infra--session-directory
                    remote-root)
        (setq-local ai-code-backends-infra--session-terminal-backend
                    'ghostel)
        (setq-local default-directory remote-root)
        (insert "Saved ./remote.png\n")
        (ai-code-session-link--linkify-session-region
         (point-min) (point-max))
        (should-not file-exists-called)
        (should-not create-image-called)
        (should-not
         (cl-some
          (lambda (overlay)
            (overlay-get overlay 'ai-code-session-image-preview))
          (overlays-in (point-min) (point-max))))))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-wrapped-bare-path ()
  "A bare local image path hard-wrapped across terminal rows should preview.
Tools such as Claude print bare paths (no `file://' prefix); when the path
is long enough Ghostel wraps it, splitting the token across rows.  The
detection regexp must stitch those rows back together."
  (let* ((root (make-temp-file "ai-code-session-image-preview-wrap-" t))
         (image-file
          (expand-file-name
           "a-very-long-wrapped-screenshot-name-for-terminal-testing.png"
           root))
         (split (/ (length image-file) 2))
         (head (substring image-file 0 split))
         (tail (substring image-file split))
         (created-image nil))
    ;; Guard against an accidental split inside a space run; the temp path
    ;; has none, but keep the invariant explicit for future readers.
    (should-not (string-match-p "[ \t]" image-file))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (setq created-image (cons data args))
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              ;; Mimic a Ghostel hard wrap: the path breaks mid-token and the
              ;; continuation row carries a leading render gutter.
              (insert (format "Wrote %s\n    %s\n" head tail))
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (let ((preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))
                (should (equal (car created-image) "fake image bytes"))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-keeps-line-gutter ()
  "Image previews should align with the image path line's leading gutter."
  (let* ((root (make-temp-file "ai-code-session-image-preview-indent-" t))
         (image-file (expand-file-name "screenshot.png" root)))
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
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "    | /tmp/nope.txt\n")
              (insert "    | screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (let* ((preview-overlays
                      (cl-remove-if-not
                       (lambda (overlay)
                         (overlay-get overlay 'ai-code-session-image-preview))
                       (overlays-in (point-min) (point-max))))
                     (after-string (overlay-get (car preview-overlays)
                                                'after-string)))
                (should (= (length preview-overlays) 1))
                (should (string-prefix-p "\n    " after-string))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-absorbs-clicks ()
  "Clicking the inline image preview should not open the file link."
  (let* ((root (make-temp-file "ai-code-session-image-preview-click-" t))
         (image-file (expand-file-name "screenshot.png" root)))
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
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "Saved screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (let* ((preview-overlays
                      (cl-remove-if-not
                       (lambda (overlay)
                         (overlay-get overlay 'ai-code-session-image-preview))
                       (overlays-in (point-min) (point-max))))
                     (overlay (car preview-overlays))
                     (after-string (overlay-get overlay 'after-string))
                     (image-index (cl-position-if
                                   (lambda (index)
                                     (get-text-property index 'display after-string))
                                   (number-sequence 0 (1- (length after-string)))))
                     (preview-keymap
                      (and image-index
                           (get-text-property image-index 'keymap after-string))))
                (should (= (length preview-overlays) 1))
                (goto-char (point-min))
                (search-forward "screenshot.png")
                (should (eq (lookup-key (get-text-property (match-beginning 0)
                                                           'keymap)
                                        [mouse-1])
                            'ai-code-session-navigate-link-at-mouse))
                (should preview-keymap)
                (should (eq (lookup-key preview-keymap [mouse-1])
                            'ai-code-session-link--ignore-image-preview-event))
                (should-not (eq (lookup-key preview-keymap [mouse-1])
                                'ai-code-session-navigate-link-at-mouse))
                (should-not (overlay-get overlay 'keymap))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-recreates-missing-overlay ()
  "Image previews should be recreated when link text is already linked."
  (let* ((root (make-temp-file "ai-code-session-image-preview-recreate-" t))
         (image-file (expand-file-name "screenshot.png" root)))
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
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "Saved screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (dolist (overlay (overlays-in (point-min) (point-max)))
                (when (overlay-get overlay 'ai-code-session-image-preview)
                  (delete-overlay overlay)))
              (goto-char (point-min))
              (search-forward "screenshot.png")
              (should (get-text-property (match-beginning 0)
                                         'ai-code-session-link))
              (ai-code-session-link--linkify-file-region (point-min) (point-max))
              (should
               (= (length
                   (cl-remove-if-not
                    (lambda (overlay)
                      (overlay-get overlay 'ai-code-session-image-preview))
                    (overlays-in (point-min) (point-max))))
                  1)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-refreshes-unchanged-history ()
  "Image previews should refresh for unchanged Ghostel history text."
  (let* ((root (make-temp-file "ai-code-session-image-preview-history-" t))
         (image-file (expand-file-name "history.png" root)))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (with-temp-buffer
            (setq-local ai-code-backends-infra--session-directory root)
            (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
            (insert "Saved history.png\n")
            (cl-letf (((symbol-function 'display-images-p)
                       (lambda (&optional _display) nil)))
              (ai-code-session-link--linkify-session-region (point-min) (point-max)))
            (goto-char (point-min))
            (search-forward "history.png")
            (should (get-text-property (match-beginning 0)
                                       'ai-code-session-link))
            (should-not
             (cl-some
              (lambda (overlay)
                (overlay-get overlay 'ai-code-session-image-preview))
              (overlays-in (point-min) (point-max))))
            (cl-letf (((symbol-function 'display-images-p)
                       (lambda (&optional _display) t))
                      ((symbol-function 'create-image)
                       (lambda (file &rest args)
                         (list :image file :args args))))
              (ai-code-session-link--linkify-session-region (point-min) (point-max)))
            (should
             (= (length
                 (cl-remove-if-not
                  (lambda (overlay)
                    (overlay-get overlay 'ai-code-session-image-preview))
                  (overlays-in (point-min) (point-max))))
                1))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-keeps-repeated-history-stable ()
  "Unchanged Ghostel history should not duplicate live image previews."
  (let* ((root (make-temp-file "ai-code-session-image-preview-repeat-" t))
         (image-file (expand-file-name "history.png" root))
         (create-count 0))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (file &rest args)
                       (setq create-count (1+ create-count))
                       (list :image file :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "Saved history.png\n")
              (ai-code-session-link--linkify-session-region
               (point-min) (point-max))
              (should (= create-count 1))
              (ai-code-session-link--linkify-session-region
               (point-min) (point-max))
              (should (= create-count 1))
              (should
               (= (length
                   (cl-remove-if-not
                    (lambda (overlay)
                      (overlay-get overlay 'ai-code-session-image-preview))
                    (overlays-in (point-min) (point-max))))
                  1)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-strict-visible-scan ()
  "Strict visible image scanning should preview local images without project scans."
  (let* ((root (make-temp-file "ai-code-session-image-preview-visible-" t))
         (image-file (expand-file-name "visible.png" root))
         project-scan-count)
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args)))
                    ((symbol-function 'ai-code-session-link--project-files)
                     (lambda (&rest _args)
                       (setq project-scan-count (1+ (or project-scan-count 0)))
                       nil)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "Saved to: " image-file "\n")
              (setq buffer-read-only t)
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (should-not project-scan-count)
              (should
               (= (length
                   (cl-remove-if-not
                    (lambda (overlay)
                      (overlay-get overlay 'ai-code-session-image-preview))
                    (overlays-in (point-min) (point-max))))
                  1)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-strict-visible-wrap ()
  "Strict visible image scanning should handle simple wrapped file URLs."
  (let* ((root (make-temp-file "ai-code-session-image-preview-visible-wrap-" t))
         (dir (expand-file-name "generated_images/session" root))
         (image-file (expand-file-name "ig_wrapped_image.png" dir))
         (split-index (- (length image-file)
                         (length (file-name-nondirectory image-file)))))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "file://")
              (insert (substring image-file 0 split-index))
              (insert "\n  ")
              (let ((segment-start (point)))
                (insert (substring image-file split-index))
                (insert "\n")
                (setq buffer-read-only t)
                (ai-code-session-link--linkify-strict-image-preview-region
                 (point-min) (point-max))
                (let ((preview-overlays
                       (cl-remove-if-not
                        (lambda (overlay)
                          (overlay-get overlay 'ai-code-session-image-preview))
                        (overlays-in (point-min) (point-max)))))
                  (should (= (length preview-overlays) 1))
                  (should (equal (get-text-property
                                  segment-start
                                  'ai-code-session-link)
                                 (concat "file://" image-file)))
                  (should (equal (overlay-get (car preview-overlays)
                                              'ai-code-session-image-file)
                                 image-file)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-file-url-wrap ()
  "Ghostel image previews should handle terminal-wrapped file:// URLs."
  (let* ((root (make-temp-file "ai-code-session-image-preview-url-" t))
         (dir (expand-file-name "generated_images/019f3849-01c6-70d2-9306-f19721bfa0f4" root))
         (image-file (expand-file-name
                      "ig_0316e59b09ff2b45016a4be031a5b8819ab73e1af52a3e7bd2.png"
                      dir))
         (split-index (- (length image-file)
                         (length (file-name-nondirectory image-file))))
         (create-image-data nil))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (setq create-image-data data)
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "  file://")
              (insert (substring image-file 0 split-index))
              (insert "\n")
              (insert "    ")
              (insert (substring image-file split-index))
              (insert "\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (let ((preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (should (equal create-image-data "fake image bytes"))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-strict-visible-wrap-bare-path ()
  "Strict visible scanning should rejoin a wrapped BARE absolute path.
Regression: `ai-code-session-link--strict-previous-line-prefix' used a
malformed negated character class (a `]' escaped with a backslash) that
closed the class early, so it only matched lines ending in a literal `]'.
Bare wrapped paths -- as printed by tools such as Claude, which omit the
`file://' prefix -- were therefore never recovered from the previous row."
  (let* ((root (make-temp-file
                "ai-code-session-image-preview-visible-wrap-bare-" t))
         (dir (expand-file-name
               "generated/2d806356-90b2-4e71-82fa-30359ee62190" root))
         (image-file (expand-file-name "ghostel-wrapped-bare-image.png" dir))
         ;; Break right before the file name so the continuation row is a
         ;; relative fragment (no leading slash) -- this is what forces the
         ;; previous-line prefix recovery path (the buggy branch).
         (split-index (- (length image-file)
                         (length (file-name-nondirectory image-file)))))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              ;; Bare path (no file:// prefix) hard-wrapped across two rows,
              ;; each carrying a leading render gutter.
              (insert "  ")
              (insert (substring image-file 0 split-index))
              (insert "\n    ")
              (insert (substring image-file split-index))
              (insert "\n")
              (setq buffer-read-only t)
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (let ((preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-strict-visible-wrap-three-rows ()
  "Strict visible scanning should rejoin bare paths wrapped over three rows."
  (let* ((root (make-temp-file
                "ai-code-session-image-preview-visible-wrap-three-" t))
         (dir (expand-file-name "sloth-layout-check" root))
         (image-file (expand-file-name "sloth-window-after-53660.png" dir))
         (needle "sloth-layout-check/sloth-window-after-53660.png")
         (needle-start (string-match (regexp-quote needle) image-file))
         (path-prefix (substring image-file 0 needle-start)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (data &rest args)
                       (list :image data :args args))))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend
                          'ghostel)
              (insert "  ")
              (insert path-prefix)
              (insert "sloth-layout-\n    check/sloth-window-after-\n    53660.png\n")
              (setq buffer-read-only t)
              (ai-code-session-link--linkify-strict-image-preview-region
               (point-min) (point-max))
              (let ((preview-overlays
                     (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'ai-code-session-image-preview))
                      (overlays-in (point-min) (point-max)))))
                (should (= (length preview-overlays) 1))
                (should (equal (overlay-get (car preview-overlays)
                                            'ai-code-session-image-file)
                               image-file))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-image-preview-max-dimensions ()
  "Preview caps: integer customs are hard caps; nil height is uncapped."
  ;; An explicit integer custom is used verbatim, regardless of any window.
  (let ((ai-code-session-link-ghostel-image-preview-max-width 400)
        (ai-code-session-link-ghostel-image-preview-max-height 300))
    (with-temp-buffer
      (should (equal (ai-code-session-link--image-preview-max-dimensions "")
                     '(400 . 300)))))
  ;; With nil customs and no window showing the buffer, only the fixed width
  ;; fallback applies.  Height remains uncapped so screenshots can fill width.
  (let ((ai-code-session-link-ghostel-image-preview-max-width nil)
        (ai-code-session-link-ghostel-image-preview-max-height nil))
    (with-temp-buffer
      (should (equal (ai-code-session-link--image-preview-max-dimensions "  ")
                     (cons ai-code-session-link--image-preview-fallback-max-width
                           nil))))))

(ert-deftest ai-code-session-link-test-ghostel-image-preview-common-reference-forms ()
  "Ghostel image previews should handle common local image reference forms."
  (let* ((root (make-temp-file "ai-code-session-image-preview-forms-" t))
         (image-dir (expand-file-name "generated images" root))
         (spaced-image (expand-file-name "My Image.png" image-dir))
         (relative-dir (expand-file-name "relative output" root))
         (relative-image (expand-file-name "Relative Image.png" relative-dir))
         (plain-image (expand-file-name "plain.png" root)))
    (unwind-protect
        (progn
          (make-directory image-dir t)
          (make-directory relative-dir t)
          (dolist (file (list spaced-image relative-image plain-image))
            (with-temp-file file
              (insert "fake image bytes")))
          (cl-labels
              ((preview-file-for
                (output)
                (let (create-image-called)
                  (cl-letf (((symbol-function 'display-images-p)
                             (lambda (&optional _display) t))
                            ((symbol-function 'create-image)
                             (lambda (data &rest args)
                               (setq create-image-called t)
                               (list :image data :args args))))
                    (with-temp-buffer
                      (setq-local ai-code-backends-infra--session-directory root)
                      (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
                      (insert output)
                      (ai-code-session-link--linkify-session-region
                       (point-min) (point-max))
                      (let* ((preview-overlays
                              (cl-remove-if-not
                               (lambda (overlay)
                                 (overlay-get overlay 'ai-code-session-image-preview))
                               (overlays-in (point-min) (point-max))))
                             (preview-count (length preview-overlays))
                             (preview-file
                              (and create-image-called
                                   (overlay-get (car preview-overlays)
                                                'ai-code-session-image-file))))
                        (cons preview-file preview-count)))))))
            (let ((cases
                   (list
                    (list (format "![preview](file://%s)\n"
                                  (replace-regexp-in-string " " "%20" spaced-image))
                          spaced-image)
                    (list (format "[[file:%s][preview]]\n" plain-image)
                          plain-image)
                    (list (format "file://localhost%s\n" plain-image)
                          plain-image)
                    (list (format "Saved to: \"%s\"\n" spaced-image)
                          spaced-image)
                    (list (format "Saved to: %s\n"
                                  (replace-regexp-in-string " " "\\\\ " spaced-image))
                          spaced-image)
                    (list "![preview](relative output/Relative Image.png)\n"
                          relative-image))))
              (dolist (case cases)
                (let ((result (preview-file-for (car case))))
                  (should (equal (car result) (cadr case)))
                  (should (= (cdr result) 1)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-image-preview-skips-non-ghostel-session ()
  "Image preview overlays should not be added for non-Ghostel terminals."
  (let* ((root (make-temp-file "ai-code-session-image-preview-skip-" t))
         (image-file (expand-file-name "screenshot.png" root))
         (create-image-called nil))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "fake image bytes"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (&rest _args)
                       (setq create-image-called t)
                       'image)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'vterm)
              (insert "Saved screenshot.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (should-not create-image-called)
              (should-not
               (cl-some
                (lambda (overlay)
                  (overlay-get overlay 'ai-code-session-image-preview))
                (overlays-in (point-min) (point-max)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-image-preview-respects-size-limit ()
  "Image previews should skip files over the configured byte limit."
  (let* ((root (make-temp-file "ai-code-session-image-preview-size-" t))
         (image-file (expand-file-name "large.png" root))
         (create-image-called nil)
         (ai-code-session-link-ghostel-image-preview-max-bytes 1))
    (unwind-protect
        (progn
          (with-temp-file image-file
            (insert "too large"))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (&rest _args)
                       (setq create-image-called t)
                       'image)))
            (with-temp-buffer
              (setq-local ai-code-backends-infra--session-directory root)
              (setq-local ai-code-backends-infra--session-terminal-backend 'ghostel)
              (insert "Saved large.png\n")
              (ai-code-session-link--linkify-session-region (point-min) (point-max))
              (should-not create-image-called))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest ai-code-session-link-test-schedule-linkify-recent-output-skips-plain-prose ()
  "Plain prose output should not schedule hot-path session relinkification."
  (let ((ai-code-session-link-enabled t))
    (with-temp-buffer
      (setq ai-code-session-link--pending-tail-width 0
            ai-code-session-link--linkify-timer nil)
      (ai-code-session-link--schedule-linkify-recent-output
       (current-buffer)
       "Working on the next step now.\n")
      (should-not ai-code-session-link--linkify-timer)
      (should (zerop ai-code-session-link--pending-tail-width)))))

(provide 'test_ai-code-session-link)

;;; test_ai-code-session-link.el ends here
