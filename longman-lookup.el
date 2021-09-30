;;; longman-lookup.el --- Lookup a word in Longman Dictionary  -*- lexical-binding: t -*-

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Keywords: tools, extensions
;; URL: "https://github.com/hikmet517/longman-lookup.el"
;; Keywords: convenience

;;; Commentary:
;; Lookup a word in Longman English Dictionary and create an org mode buffer
;; containing definitions.

;;; TODO:
;; * write tests (ERT), find examples
;;   - a word with no entry but ref (pander)
;;   - a word with no sense etc.
;;   - good examples: mind, render, evasive, beyond, meddle, look up, pander


;;; Code:

;;;; Libraries
(require 'url)
(require 'dom)
(require 'org)
(require 'thingatpt)
(require 'subr-x)
(require 'simple)
(require 'rx)


;;;; Variables

(defconst longman-lookup-buffer-format "*ldoce <%s>*"
  "Format for buffer names.")

(defvar-local current-url nil "Current url.")

(defvar-local current-word nil "Current word.")

(defconst longman-base-url "https://www.ldoceonline.com")

(defconst longman-direct-url (concat longman-base-url "/dictionary/"))

(defconst longman-search-url (concat longman-base-url "/search/english/direct/?q="))

(defconst longman-org-link-regexp
  (rx "[" "[" (group (+ not-newline)) "]" "[" (+ not-newline) "]" "]"))

(defvar read-only-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "?" 'describe-mode)
    (define-key map "h" 'describe-mode)
    (define-key map ">" 'end-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map " " 'scroll-up-command)
    (define-key map [?\S-\ ] 'scroll-down-command)
    (define-key map "b" 'longman-lookup-browse)
    (define-key map "e" 'longman-lookup-edit)
    (define-key map "s" 'longman-lookup-save-buffer)
    (define-key map "S" 'longman-lookup-save-buffer-overwrite)
    (define-key map "o" 'longman-lookup-open-file)
    (define-key map [(control ?m)] 'longman-lookup-go-to-link)
    map))


;;;; User options

(defgroup longman-lookup nil
  "Lookup words in Longman English Dictionary."
  :group 'convenience
  :prefix "longman-lookup-"
  :link '(url-link "https://github.com/hikmet517/longman-lookup.el"))

(defcustom longman-lookup-save-dir "~/Documents/words/"
  "Directory to save words."
  :type 'directory
  :group 'longman-lookup)


;;;; Functions

;;;###autoload
(define-derived-mode read-only-org-mode org-mode "Read-Only Org"
  "Major mode used in longman-lookup.

\\{read-only-org-mode-map}"
  (org-show-all)
  (goto-char (point-min))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (when buffer-file-name
    (setq current-word (string-trim-right (file-name-nondirectory buffer-file-name)
                                          "\\.ro\\.org"))
    (setq current-url (concat longman-direct-url (string-replace " " "-" current-word)))))


(when (not (fboundp 'string-replace))
  (fset 'string-replace #'(lambda (reg rep str)
                            (replace-regexp-in-string (regexp-quote reg) rep str))))

(defun longman-lookup--validate-filename (f)
  "Validate filename F by removing illegal characters.
URL `https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions'"
  (string-trim
   (thread-last
       f
     (replace-regexp-in-string "\[/\\\]" "-")
     (replace-regexp-in-string "\[<>:\"\|\?\*\n\]" " ")
     (replace-regexp-in-string "[[:blank:]]+" " "))))

(defun longman-lookup-edit ()
  "Inhibit read-only-org-mode switch to `org-mode'."
  (interactive)
  (let ((name (buffer-name)))
    (when (and (string-prefix-p "*" name)
               (string-suffix-p "*" name))
      (longman-lookup-save-buffer)
      (longman-lookup-open-file)))
  (setq buffer-read-only nil)
  (org-mode))

(defun longman-lookup-save-buffer ()
  "Save the current buffer under `longman-lookup-save-dir'."
  (interactive)
  (when (string-empty-p longman-lookup-save-dir)
    (error "Save directory is empty"))
  (when (not (file-readable-p longman-lookup-save-dir))
    (make-directory longman-lookup-save-dir t))
  (when (null current-word)
    (error "No word in buffer"))
  (let* ((word (longman-lookup--validate-filename (concat current-word ".ro.org")))
         (path (expand-file-name word longman-lookup-save-dir)))
    (if (file-exists-p path)
        (message "File already exists")
      (write-file path nil)
      (read-only-org-mode))))

(defun longman-lookup-save-buffer-overwrite ()
  "Save the current buffer under `longman-lookup-save-dir'."
  (interactive)
  (when (string-empty-p longman-lookup-save-dir)
    (error "Save directory is empty"))
  (when (not (file-readable-p longman-lookup-save-dir))
    (make-directory longman-lookup-save-dir t))
  (when (null current-word)
    (error "No word in buffer"))
  (let* ((word (longman-lookup--validate-filename (concat current-word ".ro.org")))
         (path (expand-file-name word longman-lookup-save-dir)))
    (write-file path nil)
    (read-only-org-mode)))

(defun longman-lookup-open-file ()
  "Open current document from disk, if it is saved."
  (interactive)
  (let* ((word (longman-lookup--validate-filename (concat current-word ".ro.org")))
         (path (expand-file-name word longman-lookup-save-dir)))
    (if (not (file-exists-p path))
        (error "File does not exist")
      (message "Opening file \"%s\"" path)
      (find-file path))))

(defun longman-lookup-browse ()
  "Browser url for current 'read-only org' buffer."
  (interactive)
  (browse-url current-url))

(defun longman-lookup--get-node-text (n)
  "Get text inside node N (escaping &nbsp and multiple spaces)."
  (let ((s (string-replace " " " " (dom-texts n ""))))
    (string-trim (replace-regexp-in-string "[[:blank:]]+" " " s))))

(defun longman-lookup--parse-crossref (node)
  "Handle NODE which has a class CROSSREF, return an org link with → in the beginning."
  (let* ((a-elem (dom-by-tag node 'a))
         (link (concat longman-base-url (cdr (assoc 'href (dom-attributes a-elem)))))
         (text (longman-lookup--get-node-text (car (dom-by-class a-elem "^REFHWD$")))))
    (concat "→ [" "[" link "]" "[" text "]" "]")))

(defun longman-lookup--parse-sense (sense)
  "Parse SENSE nodes."
  (let ((text "")
        (indent ""))
    (dolist (node (dom-non-text-children sense))
      (cond
       ((string= (dom-attr node 'class) "DEF")
        (setq text (concat text "  * " (longman-lookup--get-node-text node) "\n"))
        (setq indent (make-string 4 ?\s)))
       ((string= (dom-attr node 'class) "RELATEDWD")
        (setq text (concat (string-remove-suffix "\n" text)
                           " "
                           (longman-lookup--get-node-text node) "\n")))
       ((string= (dom-attr node 'class) "EXAMPLE")
        (setq text (concat text indent "- " (longman-lookup--get-node-text node) "\n")))
       ((or (string= (dom-attr node 'class) "GramExa")
            (string= (dom-attr node 'class) "ColloExa"))
        (let* ((defnode (or (dom-by-class node "^PROPFORM$")
                            (dom-by-class node "^PROPFORMPREP$")
                            (dom-by-class node "^COLLO$"))))
          (setq text (concat text (format "    - *%s*\n"
                                          (longman-lookup--get-node-text defnode))))
          (setq indent (make-string 6 ?\s))
          (dolist (ex (dom-by-class node "^EXAMPLE$"))
            (setq text (concat text indent "- " (longman-lookup--get-node-text ex) "\n")))))
       ((string= (dom-attr node 'class) "Subsense")
        (setq text (concat text (longman-lookup--parse-sense node))))
       ((string= (dom-attr node 'class) "Crossref")
        (setq text (concat text "  * " (longman-lookup--parse-crossref node) "\n")))))
    text))

(defun longman-lookup--parse-entry (entry)
  "Parse the entry ENTRY."
  (let* ((word (longman-lookup--get-node-text (car (dom-by-class entry "HWD$"))))
         (dictionary (longman-lookup--get-node-text
                      (car (dom-by-class entry "dictionary_intro"))))
         (pos (string-trim (dom-text (car (dom-by-class entry "^POS$")))))
         (entry-header (concat "* "
                               word
                               (unless (string-empty-p pos) (format " (%s)" pos))
                               (when (and
                                      (not (string= dictionary ""))
                                      (not (string= dictionary "From Longman Dictionary of Contemporary English")))
                                 (format " [%s]" (string-remove-prefix "From Longman "
                                                                       dictionary)))
                               ":\n"))
         (entry-text "")
         (senses (append (dom-by-class entry "^Sense$") (dom-by-class entry "^Tail$"))))
    (dolist (sense senses)
      (let ((sense-text (longman-lookup--parse-sense sense)))
        (unless (string-empty-p sense-text)
          (setq entry-text (concat entry-text sense-text)))))
    (if (string-empty-p entry-text)
        nil
      (concat entry-header entry-text))))

(defun longman-lookup--parse-display-cb (status)
  "Callback function for `url-retrieve', check STATUS, parse output, display it."
  ;; fetch
  (let* ((entries-text nil)
         (header nil))
    (let ((err (plist-get status :error)))
      (when err
        (error "Fetch failed")
        (pp err)))
    (unless (current-buffer)
      (error "Fetch failed, no buffer"))

    ;; parse
    (let* ((tree (libxml-parse-html-region (point-min) (point-max)))
           (entries (dom-by-class tree "^dictentry$")))
      (when (null tree)
        (error "Parse failed"))
      (setq header (longman-lookup--get-node-text (car (dom-by-tag tree 'h1))))
      (when (or (null header)
                (null entries)
                (string-prefix-p "Sorry, there are no results for" header)
                (string-prefix-p "Did you mean" header))
        (error "Word not found"))
      (setq entries-text (mapconcat #'longman-lookup--parse-entry entries "")))
    (kill-buffer)

    ;; result buffer
    (let ((buf (get-buffer-create (format longman-lookup-buffer-format header))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert entries-text)
          (read-only-org-mode))
        ;; set local-variables
        (if-let (last-url (plist-get status :redirect))
            (progn
              (setq current-word (car (last (split-string last-url "/"))))
              (setq current-url last-url))
          (setq current-word header)
          (setq current-url (concat longman-direct-url current-word))))
      (display-buffer buf))))

;;;###autoload
(defun longman-lookup (word)
  "Get the definition of WORD from 'ldoceonline.com' and display it in an `org-mode' buffer."
  (interactive (list
                (let* ((w (if (use-region-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))
                            (thing-at-point 'word t)))
                       (ww (when w (string-trim w))))
                  (if (and ww (not (string-empty-p ww)))
                      (read-string (format "Enter word (%s): " ww) nil nil ww)
                    (read-string "Enter word: ")))))
  (let* ((url (concat longman-search-url word)))
    (url-retrieve url #'longman-lookup--parse-display-cb)))

;;;###autoload
(defun longman-lookup-local-first (word)
  "Check local dictionary first, if WORD is not there, call `longman-lookup'."
  (interactive (list
                (let* ((w (if (use-region-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))
                            (thing-at-point 'word t)))
                       (ww (if w (string-trim w) "")))
                  (completing-read "Enter word: "
                                   (mapcar #'(lambda (s)
                                               (string-trim-right s "\\.ro\\.org$"))
                                           (directory-files longman-lookup-save-dir
                                                            nil
                                                            ".+\\.ro\\.org$"))
                                   nil nil nil nil ww))))
  (let* ((valid (longman-lookup--validate-filename (concat word ".ro.org")))
         (path (expand-file-name valid longman-lookup-save-dir)))
    (if (file-exists-p path)
        (find-file-other-window path)
      (longman-lookup word))))

(defun longman-lookup-go-to-link ()
  "Go to link under the cursor."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward longman-org-link-regexp nil t)
      (let ((link (buffer-substring-no-properties (match-beginning 1)
                                                  (match-end 1))))
        (url-retrieve link #'longman-lookup--parse-display-cb)))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ro\\.org\\'" . read-only-org-mode))

(provide 'longman-lookup)
;;; longman-lookup.el ends here
