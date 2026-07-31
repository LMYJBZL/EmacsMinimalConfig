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

(ert-deftest my/latex-vrb-source-candidates-find-output-dir-master ()
  (let* ((dir (make-temp-file "latex-vrb-source-" t))
         (output-dir (expand-file-name ".LaTeXOut" dir))
         (tex-file (expand-file-name "slides.tex" dir))
         (vrb-file (expand-file-name "slides.vrb" output-dir)))
    (unwind-protect
        (progn
          (make-directory output-dir)
          (with-temp-file tex-file (insert "\\begin{frame}\ntext\n\\end{frame}\n"))
          (with-temp-file vrb-file (insert "text\n"))
          (should (equal (my/latex-vrb-source-candidates vrb-file)
                         (list tex-file))))
      (delete-directory dir t))))

(ert-deftest my/latex-vrb-redirect-opens-tex-near-matching-line ()
  (let* ((dir (make-temp-file "latex-vrb-redirect-" t))
         (output-dir (expand-file-name ".LaTeXOut" dir))
         (tex-file (expand-file-name "slides.tex" dir))
         (vrb-file (expand-file-name "slides.vrb" output-dir)))
    (unwind-protect
        (progn
          (make-directory output-dir)
          (with-temp-file tex-file
            (insert "\\begin{frame}\nplain text\n\\draw (0,0) -- (1,1);\n\\end{frame}\n"))
          (with-temp-file vrb-file
            (insert "plain text\n\\draw (0,0) -- (1,1);\n"))
          (let ((vrb-buffer (find-file-noselect vrb-file)))
            (unwind-protect
                (with-current-buffer vrb-buffer
                  (goto-char (point-min))
                  (forward-line 1)
                  (my/latex-redirect-vrb-source-buffer)
                  (should (file-equal-p (buffer-file-name) tex-file))
                  (should (looking-at-p "\\\\draw")))
              (kill-buffer vrb-buffer))))
      (dolist (buffer (buffer-list))
        (when-let ((file (buffer-file-name buffer)))
          (when (file-in-directory-p file dir)
            (kill-buffer buffer))))
      (delete-directory dir t))))

(ert-deftest my/latex-load-provided-style-hooks-loads-private-style ()
  (let ((style-dir (make-temp-file "latex-style-hooks-" t)))
    (unwind-protect
        (let ((style-file (expand-file-name "mytestpkg.el" style-dir)))
          (with-temp-file style-file
            (insert ";; -*- lexical-binding: t; -*-\n"
                    "(TeX-add-style-hook\n"
                    " \"mytestpkg\"\n"
                    " (lambda ()\n"
                    "   (LaTeX-add-environments '(\"mytestenv\" LaTeX-env-args [\"argument\"] 0)))\n"
                    " :latex)\n"))
          (with-temp-buffer
            (LaTeX-mode)
            (let ((TeX-style-path (cons style-dir TeX-style-path))
                  (LaTeX-provided-package-options '(("mytestpkg" "")))
                  (LaTeX-provided-class-options nil))
              (my/latex-load-provided-style-hooks t)
              (should
               (member "mytestenv"
                       (cl-loop for group in LaTeX-environment-list
                                append (mapcar (lambda (entry)
                                                 (if (consp entry)
                                                     (car entry)
                                                   entry))
                                               group)))))))
      (delete-directory style-dir t))))

(ert-deftest my/latex-provided-style-hook-names-adds-beamer-for-ctexbeamer ()
  (let ((LaTeX-provided-class-options '(("ctexbeamer" "t")))
        (LaTeX-provided-package-options '(("Theorems26" "language=zh"))))
    (should (member "ctexbeamer" (my/latex-provided-style-hook-names)))
    (should (member "Theorems26" (my/latex-provided-style-hook-names)))
    (should (member "beamer" (my/latex-provided-style-hook-names)))))

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
