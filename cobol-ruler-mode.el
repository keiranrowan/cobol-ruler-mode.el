(eval-when-compile
  (require 'wid-edit))
(require 'scroll-bar)
(require 'fringe)

(defgroup cobol-ruler-mode nil
  "Display a cobol ruler in the header line."
  :version "1.0"
  :group 'convenience)

(defcustom cobol-ruler-mode-basic-char ?\ 
  "Basic column character."
  :type 'integer)

(defcustom cobol-ruler-mode-filler-char ?\+ 
  "Basic column character."
  :type 'integer)

(defcustom cobol-ruler-mode-comment-char ?\.
  "Character displayed in indent columns."
  :type 'integer)

(defcustom cobol-ruler-mode-indicator-char ?\-
  "Character displayed in indicator column."
  :type 'integer)

(defcustom cobol-ruler-mode-A-char ?A
  "Character displayed in A column."
  :type 'integer)

(defcustom cobol-ruler-mode-B-char ?B
  "Character displayed in B column."
  :type 'integer)

(defface cobol-ruler-mode-default
  '((((type tty))
     (:inherit default
               :background "grey64"
               :foreground "black"
               ))
    (t
     (:inherit default
               :background "grey76"
               :foreground "black"
               :box (:color "grey76"
                            :line-width 1
                            :style released-button)
               )))
  "Default face used by the cobol ruler.")

(defface cobol-ruler-mode-pad
  '((((type tty))
     (:inherit cobol-ruler-mode-default
               :background "grey50"
               ))
    (t
     (:inherit cobol-ruler-mode-default
               :background "grey64"
               )))
  "Face used to pad inactive cobol ruler areas.")

(defface cobol-ruler-mode-margins
  '((t
     (:inherit cobol-ruler-mode-default
               :foreground "white"
               )))
  "Face used to highlight margin areas.")

(defface cobol-ruler-mode-fringes
  '((t
     (:inherit cobol-ruler-mode-default
               :foreground "green"
               )))
  "Face used to highlight fringes areas.")

(defface cobol-ruler-mode-column-number
  '((t
     (:inherit cobol-ruler-mode-default
               :foreground "black"
               )))
  "Face used to highlight number graduations.")

(defface cobol-ruler-mode-comment-column
  '((t
     (:inherit cobol-ruler-mode-default
               :foreground "black"
               )))
  "Face used to highlight indent columns.")

(defface cobol-ruler-mode-indicator-column
  '((t
     (:inherit cobol-ruler-mode-default
               :foreground "black"
               )))
  "Face used to highlight indicator column.")

(defface cobol-ruler-mode-A-column
  '((t
     (:inherit cobol-ruler-mode-default
               :foreground "black"
               )))
  "Face used to highlight A column.")

(defface cobol-ruler-mode-B-column
  '((t
     (:inherit cobol-ruler-mode-default
               :foreground "black"
               )))
  "Face used to highlight B column.")

(defface cobol-ruler-mode-pgmid-column
  '((t
     (:inherit cobol-ruler-mode-default
               :foreground "black"
               )))
  "Face used to highlight program-id columns.")

(defface cobol-ruler-mode-number-column
  '((t
     (:inherit cobol-ruler-mode-default
               :foreground "black"
               )))
  "Face used to highlight number column.")

(defvar cobol-ruler-mode-header-line-format-old nil
  "Hold previous value of `header-line-format'.")

(defvar cobol-ruler-mode-ruler-function 'cobol-ruler-mode-ruler
  "Function to call to return ruler header line format.
This variable is expected to be made buffer-local by modes.")

(defconst cobol-ruler-mode-header-line-format
  '(:eval (funcall cobol-ruler-mode-ruler-function))
  "`header-line-format' used in ruler mode.
Call `ruler-mode-ruler-function' to compute the ruler value.")

;;;###autoload
(defvar-local cobol-ruler-mode nil
  "Non-nil if Cobol Ruler mode is enabled.
Use the command `cobol-ruler-mode' to change this variable.")

(defun cobol-ruler--save-header-line-format ()
  "Install the header line format for Ruler mode.
Unless Ruler mode is already enabled, save the old header line
format first."
  (when (and (not cobol-ruler-mode)
	     (local-variable-p 'header-line-format)
	     (not (local-variable-p 'cobol-ruler-mode-header-line-format-old)))
    (setq-local cobol-ruler-mode-header-line-format-old
                header-line-format))
  (setq header-line-format cobol-ruler-mode-header-line-format))

;;;###autoload
(define-minor-mode cobol-ruler-mode
  "Toggle display of ruler in header line (COBOL Ruler mode)."
  :group 'cobol-ruler-mode
  :variable (cobol-ruler-mode
	     . (lambda (enable)
		 (when enable
		   (cobol-ruler--save-header-line-format))
		 (setq cobol-ruler-mode enable)))
  (if cobol-ruler-mode
      (add-hook 'post-command-hook 'force-mode-line-update nil t)
    ;; When `cobol-ruler-mode' is off restore previous header line format if
    ;; the current one is the ruler header line format.
    (when (eq header-line-format cobol-ruler-mode-header-line-format)
      (kill-local-variable 'header-line-format)
      (when (local-variable-p 'cobol-ruler-mode-header-line-format-old)
        (setq header-line-format cobol-ruler-mode-header-line-format-old)
        (kill-local-variable 'cobol-ruler-mode-header-line-format-old)))
    (remove-hook 'post-command-hook 'force-mode-line-update t)))

(defsubst cobol-ruler-mode-space (width &rest props)
  "Return a single space string of WIDTH times the normal character width.
Optional argument PROPS specifies other text properties to apply."
  (apply 'propertize " " 'display (list 'space :width width) props))

(defun cobol-ruler-mode-ruler ()
  "Compute and return ruler."
  (let* ((w (/ (* (window-width) (frame-char-width)) (default-font-width)))
		  (m (window-margins))
		  (f (window-fringes))
		  (i (if display-line-numbers
				 (round (line-number-display-width 'columns))
			   0))
		  (j (/ (* (window-hscroll) (frame-char-width)) (default-font-width)))
		  ;; Setup the scrollbar, fringes, and margins areas.
		  (lf (cobol-ruler-mode-space
		       'left-fringe
		       'face 'cobol-ruler-mode-fringes))
		  (rf (cobol-ruler-mode-space
		       'right-fringe
		       'face 'cobol-ruler-mode-fringes))
		  (lm (cobol-ruler-mode-space
		       'left-margin
		       'face 'cobol-ruler-mode-margins))
		  (rm (cobol-ruler-mode-space
		       'right-margin
		       'face 'cobol-ruler-mode-margins))
		  (sb (cobol-ruler-mode-space
		       'scroll-bar
		       'face 'cobol-ruler-mode-pad))
		  ;; Remember the scrollbar vertical type.
		  (sbvt (car (window-current-scroll-bars)))
		  (ruler
		   (propertize
			(if display-line-numbers
				(let* ((lndw (round (line-number-display-width 'columns)))
					   (s (make-string lndw ?\s t)))
				  (concat s (make-string (- w lndw)
										 cobol-ruler-mode-basic-char t)))
			  (make-string w cobol-ruler-mode-basic-char t))
			'face 'cobol-ruler-mode-default))
		  k c)
	(while (< i w)
	  (cond
	   ((and (= (mod j 10) 0) (> j 79))
        (setq m (length c)
              k (- i 1))
        (while (and (> m 0) (>= k 0))
          (aset ruler k (aref c (setq m (1- m))))
          (setq k (1- k))))
	   ;; Filler Columns
	   ((and (> j 7) (< j 11))
	    (aset ruler i cobol-ruler-mode-filler-char)
		(put-text-property
		 i (1+ i) 'face 'cobol-ruler-mode-comment-column
		 ruler))
	   ((and (> j 11) (< j 72))
	    (aset ruler i cobol-ruler-mode-filler-char)
		(put-text-property
		 i (1+ i) 'face 'cobol-ruler-mode-comment-column
		 ruler))
	   ((and (> j 77) (< j 80))
	    (aset ruler i cobol-ruler-mode-filler-char)
		(put-text-property
		 i (1+ i) 'face 'cobol-ruler-mode-comment-column
		 ruler))
	   ;; Comment Columns
	   ((or (= j 0) (= j 1) (= j 2) (= j 3) (= j 4) (= j 5))
		(aset ruler i cobol-ruler-mode-comment-char)
		(put-text-property
		 i (1+ i) 'face 'cobol-ruler-mode-comment-column
		 ruler))
	   ;; Indicator Column
	   ((= j 6)
		(aset ruler i cobol-ruler-mode-indicator-char)
		(put-text-property
		 i (1+ i) 'face 'cobol-ruler-mode-indicator-column
		 ruler))
	   ;; A Column
	   ((= j 7)
		(aset ruler i cobol-ruler-mode-A-char)
		(put-text-property
		 i (1+ i) 'face 'cobol-ruler-mode-A-column
		 ruler))
	   ;; B Columns
	   ((= j 11)
		(aset ruler i cobol-ruler-mode-B-char)
		(put-text-property
		 i (1+ i) 'face 'cobol-ruler-mode-B-column
		 ruler))
	   ;; Pgm-id Columns
	  ((= j 72)
	   (aset ruler i ?\P)
	   (put-text-property
	    i (1+ i) 'face 'cobol-ruler-mode-B-column
	    ruler))
	  ((= j 73)
	   (aset ruler i ?\g)
	   (put-text-property
	    i (1+ i) 'face 'cobol-ruler-mode-B-column
	    ruler))
	  ((= j 74)
	   (aset ruler i ?\m)
	   (put-text-property
	    i (1+ i) 'face 'cobol-ruler-mode-B-column
	    ruler))
	  ((= j 75)
	   (aset ruler i ?\-)
	   (put-text-property
	    i (1+ i) 'face 'cobol-ruler-mode-B-column
	    ruler))
	  ((= j 76)
	   (aset ruler i ?\i)
	   (put-text-property
	    i (1+ i) 'face 'cobol-ruler-mode-B-column
	    ruler))
	  ((= j 77)
	   (aset ruler i ?d)
	   (put-text-property
	    i (1+ i) 'face 'cobol-ruler-mode-B-column
	    ruler)))
	  (setq i (1+ i)
			j (1+ j)))
	(if (nth 2 (window-fringes))
        ;; fringes outside margins.
        (list "" (and (eq 'left sbvt) sb) lf lm
              ruler rm rf (and (eq 'right sbvt) sb))
      ;; fringes inside margins.
      (list "" (and (eq 'left sbvt) sb) lm lf
            ruler rf rm (and (eq 'right sbvt) sb)))))

(provide 'cobol-ruler-mode)
;;; cobol-ruler-mode ends here
