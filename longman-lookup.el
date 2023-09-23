;;; longman-lookup.el --- Lookup a word in Longman Dictionary  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Hikmet Altıntaş

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Maintainer: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Created: 15 Nov 2020
;; Keywords: convenience
;; URL: https://github.com/hikmet517/longman-lookup
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Lookup a word in Longman English Dictionary and create an org mode buffer
;; containing definitions.

;;; TODO:
;; * write tests (ERT), find examples
;;   - a word with no entry but ref (pander)
;;   - a word with no sense etc.
;;   - good examples: mind, render, evasive, beyond, meddle, look up, pander
;; * inline link class="defRef" example: spectral
;; * add /formal/, ex: saturate
;; * add SIGNPOST (related categories e.g story/film, drawing), ex: plot
;; * handle multiple synonyms, ex: foster
;; * fix 'inadvertent'


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

(defvar-local longman-current-url nil "Current url.")

(defvar-local longman-current-word nil "Current word.")

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
    (define-key map [mouse-1] 'longman-lookup-go-to-link-mouse)
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

(defun longman-lookup--url-from-word (word)
  "Find url from given WORD."
  (concat longman-direct-url
          (thread-last
            word
            (replace-regexp-in-string "[()]" "")
            (replace-regexp-in-string "[ ’]" "-")
            (replace-regexp-in-string "/" "-"))))

;;;###autoload
(define-derived-mode read-only-org-mode org-mode "Read-Only Org"
  "Major mode used in longman-lookup.

\\{read-only-org-mode-map}"
  (org-fold-show-all)
  (goto-char (point-min))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (setq-local mouse-1-click-follows-link nil)
  (when buffer-file-name
    (setq longman-current-word (string-trim-right (file-name-nondirectory buffer-file-name)
                                          "\\.ro\\.org"))
    (setq longman-current-url (longman-lookup--url-from-word longman-current-word))))


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

(defun longman-lookup--word-filename (&optional word)
  "Get filename from WORD."
  (let ((word (or word longman-current-word)))
    (when word
      (let* ((word (longman-lookup--validate-filename (concat word ".ro.org")))
             (word (string-replace "-" " " word)))
        (expand-file-name word longman-lookup-save-dir)))))

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

(defun longman-lookup-save-buffer (&optional overwrite)
  "Save the current buffer under `longman-lookup-save-dir'.

If OVERWRITE is given, overwrite old file."
  (interactive)
  (when (string-empty-p longman-lookup-save-dir)
    (error "Save directory is empty"))
  (when (not (file-readable-p longman-lookup-save-dir))
    (make-directory longman-lookup-save-dir t))
  (when (null longman-current-word)
    (error "No word in buffer"))
  (let ((path (longman-lookup--word-filename)))
    (if (and (not overwrite) (file-exists-p path))
        (message "File already exists")
      (write-file path nil)
      (read-only-org-mode))))

(defun longman-lookup-save-buffer-overwrite ()
  "Save the current buffer under `longman-lookup-save-dir'."
  (interactive)
  (longman-lookup-save-buffer t))

(defun longman-lookup-open-file ()
  "Open current document from disk, if it is saved."
  (interactive)
  (let ((path (longman-lookup--word-filename)))
    (if (not (file-exists-p path))
        (error "File does not exist")
      (message "Opening file \"%s\"" path)
      (find-file path))))

(defun longman-lookup-browse ()
  "Browser url for current read-only-org buffer."
  (interactive)
  (browse-url longman-current-url))

(defun longman-lookup--get-node-text (n)
  "Get text inside node N (escaping &nbsp and multiple spaces)."
  (let ((s (string-replace " " " " (dom-texts n ""))))
    (thread-last
      s
      (replace-regexp-in-string "[[:blank:]]+" " ")
      (string-trim))))

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
       ((string= (dom-attr node 'class) "REGISTERLAB")
        (setq text (concat text "  * /" (longman-lookup--get-node-text node) "/ ")))
       ((string= (dom-attr node 'class) "DEF")
        (when (string-empty-p text)
          (setq text (concat text "  * ")))
        (setq text (concat text (longman-lookup--get-node-text node) "\n"))
        (setq indent (make-string 4 ?\s)))
       ((string= (dom-attr node 'class) "RELATEDWD")
        (let ((node-text (longman-lookup--get-node-text node)))
          (setq text (concat (string-remove-suffix "\n" text)
                             (if (= (aref node-text 0) ?,) "" " ")
                             node-text "\n"))))
       ((string= (dom-attr node 'class) "SYN")
        (setq text (concat (string-remove-suffix "\n" text)
                           " [SYN " (string-trim (dom-text node)) "]\n")))
       ((string= (dom-attr node 'class) "OPP")
        (setq text (concat (string-remove-suffix "\n" text)
                           " [OPP " (string-trim (dom-text node)) "]\n")))
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
              (setq longman-current-word (car (last (split-string last-url "/"))))
              (setq longman-current-url last-url))
          (setq longman-current-word header)
          (setq longman-current-url (longman-lookup--url-from-word longman-current-word))))
      (display-buffer buf))))

;;;###autoload
(defun longman-lookup (word)
  "Get the definition of WORD from ldoce and display it in an `org-mode' buffer."
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
                                           (when (file-exists-p longman-lookup-save-dir)
                                             (directory-files longman-lookup-save-dir
                                                              nil
                                                              ".+\\.ro\\.org$")))
                                   nil nil nil nil ww))))
  (let ((path (longman-lookup--word-filename word)))
    (if (file-exists-p path)
        (find-file-other-window path)
      (longman-lookup word))))

(defun longman-lookup-go-to-link ()
  "Go to link under the cursor."
  (interactive)
  (save-excursion
    (let ((pt (point)))
      (beginning-of-line)
      (when (re-search-forward longman-org-link-regexp (line-end-position) t)
        (let ((link (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
          (when (<= (match-beginning 0) pt (1- (match-end 0)))
            (url-retrieve link #'longman-lookup--parse-display-cb)))))))

(defun longman-lookup-go-to-link-mouse (pos)
  "Open link at mouse position POS."
  (interactive "e")
  (mouse-set-point pos)
  (longman-lookup-go-to-link))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ro\\.org\\'" . read-only-org-mode))

(provide 'longman-lookup)
;;; longman-lookup.el ends here
