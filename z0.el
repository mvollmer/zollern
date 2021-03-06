;; Emacs bits for editing Z0 code

(setq auto-mode-alist (cons '("\\.z0\\'" . z0-mode) auto-mode-alist))

(defconst z0-font-lock-keywords
  '(("^(\\(def\\w*\\)\\>[ \t]*(?\\(\\sw+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ("^(\\(code\\|data\\)"
     (1 font-lock-keyword-face))
    ("\\?\\sw+" . font-lock-variable-name-face)
    ("^\\sw+" . font-lock-constant-face)))

(define-derived-mode z0-mode scheme-mode "Z0"
  "Major mode for editing Z0 code."
  (set (make-local-variable 'lisp-indent-function) 'z0-indent-function)
  (set (make-local-variable 'font-lock-defaults)
       '((z0-font-lock-keywords
          z0-font-lock-keywords z0-font-lock-keywords)
         nil t (("+-*/.<>=!?$%_&~^:|" . "w") (?# . "w 14"))
         beginning-of-defun
         (font-lock-mark-block-function . mark-defun)
         (parse-sexp-lookup-properties . t)
         (font-lock-extra-managed-props syntax-table))))

(defun z0-indent-function (indent-point state)
  (goto-char (1+ (elt state 1)))
  (cond ((and (looking-at-p "[ \t\n]*\\(def\\w*\\|code\\|data\\)")
              (= (elt state 0) 1))
         (goto-char indent-point)
         (if (looking-at-p "[ \t]*\\s_")
             0
           2))
        (t
         (scheme-indent-function indent-point state))))
