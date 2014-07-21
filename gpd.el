;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun tab-width ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq tab-stop-list (number-sequence 4 120 4)))

(defun gpd-shared-keys ()
  (local-set-key (kbd "C-<") 'toggle-note)
  (local-set-key (kbd "<RET>") 'newline-and-indent)) 

(defun gpd-mode-keys ()
  (local-set-key (kbd "C-S-n") 'new-todo)
  (local-set-key (kbd "C->") 'mark-todo)
  (local-set-key (kbd "C-S-<down>") 'done-todo)
  (local-set-key (kbd "C-S-<up>") 'done-and-repeat-todo)
  (local-set-key (kbd "C-S-b") 'next-todo))

(defun toggle-note ()
  (interactive)
  (let (filename)
    (setq filename (buffer-file-name))
    (if (string-match "\\.gpd$" filename)
        (let (note-stamp todo)          
          (setq note-stamp (get-note-stamp))
          (setq todo (get-todo))
          (find-file-other-window (concat filename "_Note"))
          (widen)
          (goto-char 0)
          (when (not (search-forward note-stamp nil t 1))
            (new-note note-stamp todo))
          (narrow-to-note note-stamp))
      (when (string-match "\\.gpd_note$" filename)
        (find-file-other-window (substring filename 0 (- (string-width filename) (string-width "_Note"))))))))

(defun narrow-to-note (note-stamp)
  (interactive)
  (goto-char 0)
  (let (note-start note-end)
    (setq note-start (- (search-forward (concat "//" note-stamp "//")) 4 (string-width note-stamp)))
    (setq note-end (search-forward "//End//"))
    (narrow-to-region note-start note-end)
    (backward-char (+ 1 (string-width "//End//")))))



(defun new-note (note-stamp todo)
  (interactive)
  (goto-char 0)
  (insert (concat "//" note-stamp "//\n"))
  (indent-for-tab-command)
  (insert todo)
  (newline)
  (newline)
  (indent-for-tab-command)
  (let (curpoint)
    (setq curpoint (point))
    (insert (concat "\n//End//\n\n"))
    (goto-char curpoint)))

(defun get-line ()
  (interactive)
  (let (p1 p2)
    (setq p1 (line-beginning-position))
    (setq p2 (line-end-position))
    (buffer-substring-no-properties p1 p2)))

(defun get-todo ()
  (interactive)
  (let (line note-stamp)
    (setq line (get-line))
    (setq note-stamp (get-note-stamp))
    (string-match (concat "`(" note-stamp ")") line)
    (chomp (replace-match "" t t line))))

(defun get-note-stamp ()
  (interactive)
  (let (myLine note-start)
    (setq myLine (get-line))
    (setq note-start (string-match "`(.*?)" myLine))
    (if note-start
        (let (note-end)
          (setq note-end (string-match ")" myLine note-start))
          (if note-end
              (substring myLine (+ note-start 2) note-end)
            (new-note-stamp)))
      (new-note-stamp))))

(defun new-note-stamp ()
  (interactive)
  (let (curpoint end-of-line note-stamp)
    (setq curpoint (point))
    (setq end-of-line (line-end-position))
    (goto-char end-of-line)
    (setq note-stamp (format-time-string "%Y.%m.%d.%H.%M"))
    (insert (concat " `(" note-stamp  ")"))
    note-stamp))


(add-hook 'gpd-mode-hook 'gpd-shared-keys)
(add-hook 'gpd-mode-hook 'gpd-mode-keys)
(add-hook 'gpd-mode-hook 'paredit-mode)
(add-hook 'gpd-mode-hook 'tab-width)


(add-hook 'gpd-note-mode-hook 'gpd-shared-keys)
(add-hook 'gpd-note-mode-hook 'paredit-mode)
(add-hook 'gpd-note-mode-hook 'tab-width)

(add-to-list 'auto-mode-alist '("\\.gpd$" . gpd-mode))
(add-to-list 'auto-mode-alist '("\\.gpd_note$" . gpd-note-mode))

(defconst gpd-font-lock-keywords-common
  (list
   '("\\#(.*?)" . font-lock-builtin-face)
   '("\\$(.*?)" . font-lock-constant-face)
   '("~(.*?)" . font-lock-type-face)
   '("`(.*?)" . font-lock-comment-face)
   '("@(.*?)" . font-lock-type-face)
   '("!(.*?)" . font-lock-variable-name-face))
  "Common highlighting expressions for GPD mode")

(defconst gpd-font-lock-keywords-notes
  (append gpd-font-lock-keywords-common
          (list
           '("//.*?//" . font-lock-comment-face))))

(defvar gpd-font-lock-keywords gpd-font-lock-keywords-common
  "Default highlighting expressions for GPD mode.")

(defvar gpd-note-font-lock-keywords gpd-font-lock-keywords-notes)

(defun goto-end-of-header (header)
  (interactive)
  (goto-char 0)
  (search-forward header)
  (search-forward "//end//")
  (beginning-of-line))

(defun new-todo ()
  (interactive)
  (goto-end-of-header "//todo//")
  (backward-char 1)
  (newline-and-indent))

(defun mark-todo ()
  (interactive)
  (send-to-top-of-header "//today//"))

(defun done-todo ()
  (interactive)
  (send-to-top-of-header "//closed//"
                         (concat "~(" (format-time-string "%d/%m/%y %H:%M") ")")))

(defun done-and-repeat-todo ()
  (interactive)
  (done-todo)
  (goto-end-of-header "//todo//")
  (yank)
  (next-todo))

(defun next-todo ()
  (interactive)
  (goto-char 0)
  (search-forward "//today//")
  (forward-char 1))

(defun send-to-top-of-header (header &optional prefix)
  (interactive)
  "Cut the current line, or current text selection."
  (let (p1 p2 today-p myline)
    (setq p1 (line-beginning-position))
    (setq p2 (line-end-position))
    (setq myLine (buffer-substring-no-properties p1 p2))
    (when (not (string-match "//.*?//" myLine))       
      (kill-region (line-beginning-position) (line-beginning-position 2))    
      (goto-char 0)
      (search-forward header)
      (setq today-p (point))
      (end-of-line)
      (insert "\n")
      (if prefix
          (insert (replace-regexp-in-string
                   (regexp-quote (chomp myLine))
                   (concat prefix " " (chomp myLine))
                   myLine))
        (insert myLine))
      (if (> p1 today-p)
          (goto-char (+ 1 p1 (string-width myLine)))
        (goto-char (+ 1 p1))))))



(defun gpd-mode ()
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(gpd-font-lock-keywords))
  (setq major-mode 'gpd-mode)
  (setq mode-name "GPD")
  (run-hooks 'gpd-mode-hook))

(defun chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)

(defun gpd-note-mode ()
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(gpd-note-font-lock-keywords))
  (setq major-mode 'gpd-note-mode)
  (setq mode-name "GPD_Note")
  (run-hooks 'gpd-note-mode-hook))

(provide 'gpd-mode)
(provide 'gpd-note-mode)


