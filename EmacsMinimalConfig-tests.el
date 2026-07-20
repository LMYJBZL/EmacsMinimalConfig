;;; EmacsMinimalConfig-tests.el --- Regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defmacro my/test-with-text (text &rest body)
  "Create a temporary buffer containing TEXT, then evaluate BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,text)
     ,@body))

(ert-deftest my/tex-delete-pair-at-buffer-start ()
  (my/test-with-text "\\left(   \\right)"
    (goto-char (+ (point-min) (length "\\left(")))
    (my/TeX--delete-pair)
    (should (equal (buffer-string) ""))))

(ert-deftest my/tex-delete-pair-at-buffer-end ()
  (my/test-with-text "\\left(   \\right)"
    (goto-char (point-max))
    (my/TeX--delete-pair)
    (should (equal (buffer-string) ""))))

(ert-deftest my/tex-delete-pair-respects-narrowing ()
  (my/test-with-text "prefix\\left(   \\right)suffix"
    (narrow-to-region (1+ (length "prefix"))
                      (- (point-max) (length "suffix")))
    (goto-char (+ (point-min) (length "\\left(")))
    (my/TeX--delete-pair)
    (widen)
    (should (equal (buffer-string) "prefixsuffix"))))

(ert-deftest my/tex-delete-left-only-when-content-is-nonblank ()
  (my/test-with-text "\\left(x\\right)"
    (goto-char (+ (point-min) (length "\\left(")))
    (my/TeX--delete-pair)
    (should (equal (buffer-string) "x\\right)"))))

(ert-deftest my/tex-delete-right-only-when-content-is-nonblank ()
  (my/test-with-text "\\left(x\\right)"
    (goto-char (point-max))
    (my/TeX--delete-pair)
    (should (equal (buffer-string) "\\left(x"))))

(ert-deftest my/tex-delete-unmatched-left-symbol ()
  (my/test-with-text "\\left("
    (goto-char (point-max))
    (my/TeX--delete-pair)
    (should (equal (buffer-string) ""))))

(ert-deftest my/tex-delete-does-not-pair-mismatched-levels ()
  (my/test-with-text "\\left(   \\Bigr)"
    (goto-char (+ (point-min) (length "\\left(")))
    (my/TeX--delete-pair)
    (should (equal (buffer-string) "   \\Bigr)"))))

(ert-deftest my/tex-delete-does-not-search-past-content ()
  (my/test-with-text "\\left(x \\right)"
    (goto-char (+ (point-min) (length "\\left(")))
    (my/TeX--delete-pair)
    (should (equal (buffer-string) "x \\right)"))))

(ert-deftest my/tex-delete-falls-back-to-normal-backspace ()
  (my/test-with-text "abc"
    (goto-char (point-max))
    (my/TeX--delete-pair)
    (should (equal (buffer-string) "ab"))))

(ert-deftest my/whitespace-cleanup-is-buffer-local-in-text-modes ()
  (with-temp-buffer
    (text-mode)
    (should (memq #'whitespace-cleanup before-save-hook))
    (should (local-variable-p 'before-save-hook))))

(ert-deftest my/latex-mode-enables-core-features ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/emacs-minimal-config-test.tex")
    (insert "\\documentclass{article}\n\\begin{document}\n$x$\n\\end{document}\n")
    (LaTeX-mode)
    (should (bound-and-true-p LaTeX-math-mode))
    (should (bound-and-true-p reftex-mode))
    (should (bound-and-true-p yas-minor-mode))
    (should (bound-and-true-p flymake-mode))
    (should (bound-and-true-p visual-line-mode))
    (should (memq #'whitespace-cleanup before-save-hook))
    (should (eq (local-key-binding (kbd "TAB")) #'my/latex-tab))
    (should (eq (local-key-binding (kbd "C-c C-f")) #'my/TeX-font-completing-read))
    (should (eq (local-key-binding (kbd "C-c f")) #'TeX-font))
    (should (equal TeX-output-dir ".LaTeXOut/"))))

(ert-deftest my/auctex-copy-pdf-same-path-is-safe ()
  (let* ((dir (make-temp-file "auctex-copy-same-" t))
         (default-directory dir)
         (source (expand-file-name "test.pdf" dir))
         (TeX-output-dir "."))
    (unwind-protect
        (progn
          (with-temp-file source (insert "pdf"))
          (my/auctex-copy-pdf-to-master-dir source)
          (should (file-exists-p source)))
      (delete-directory dir t))))

(ert-deftest my/auctex-copy-pdf-to-default-directory ()
  (let* ((target-dir (make-temp-file "auctex-copy-target-" t))
         (output-dir (expand-file-name ".LaTeXOut" target-dir))
         (source (expand-file-name "test.pdf" output-dir))
         (default-directory target-dir)
         (TeX-output-dir ".LaTeXOut/"))
    (unwind-protect
        (progn
          (make-directory output-dir)
          (with-temp-file source (insert "pdf"))
          (my/auctex-copy-pdf-to-master-dir source)
          (should (file-exists-p (expand-file-name "test.pdf" target-dir))))
      (delete-directory target-dir t))))

(ert-deftest my/auctex-copy-pdf-ignores-non-output-pdf ()
  (let* ((target-dir (make-temp-file "auctex-copy-ignore-" t))
         (source-dir (make-temp-file "auctex-copy-other-" t))
         (source (expand-file-name "test.pdf" source-dir))
         (default-directory target-dir)
         (TeX-output-dir ".LaTeXOut/"))
    (unwind-protect
        (progn
          (with-temp-file source (insert "pdf"))
          (my/auctex-copy-pdf-to-master-dir source)
          (should-not (file-exists-p (expand-file-name "test.pdf" target-dir))))
      (delete-directory source-dir t)
      (delete-directory target-dir t))))

(provide 'EmacsMinimalConfig-tests)
;;; EmacsMinimalConfig-tests.el ends here
