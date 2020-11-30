;;; longman-lookup.el --- Lookup a word in Longman Dictionary  -*- lexical-binding: t -*-

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Keywords: tools, extensions
;; URL: "https://github.com/hikmet517/longman-lookup.el"
;; Keywords: convenience

;;; Commentary:
;; Lookup a word in Longman English Dictionary and create an org mode buffer
;; containing definitions.

;;; TODO:
;; * some entries have only references
;;   these are ignored now, a better way?
;;   - https://www.ldoceonline.com/dictionary/beyond
;; * write tests (ERT), find examples
;;   - a word with no entry but ref
;;   - a word with no sense etc.
;;   - good example: 'mind'
;;   - phrasal verbs: 'look up'
;; * write dictionary
;;   - 'From Longman Dictionary of Contemporary English'
;;   - 'From Longman Business Dictionary'
;; * indentation in 'meddle'


;;; Code:

;;;; Libraries
(require 'url)
(require 'dom)
(require 'org)
(require 'thingatpt)
(require 'subr-x)
(require 'simple)


;;;; Variables
(defvar read-only-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map " " 'scroll-up-command)
    (define-key map [?\S-\ ] 'scroll-down-command)
    (define-key map "\C-?" 'scroll-down-command)
    (define-key map "?" 'describe-mode)
    (define-key map "h" 'describe-mode)
    (define-key map ">" 'end-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map "g" 'revert-buffer)
    map))

(define-derived-mode read-only-org-mode org-mode "Read-Only Org"
  "Major mode used in longman-lookup."
  (org-show-all)
  (goto-char (point-min))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))


;;;; Functions
(defun longman-lookup--get-node-text (n)
  "Get text inside node N (escaping &nbsp and multiple spaces)."
  (string-trim (replace-regexp-in-string
                " +"
                " "
                (replace-regexp-in-string " " " " (dom-texts n "")))))

(defun longman-lookup--parse-sense (sense)
  "Parse SENSE nodes."
  (let ((text ""))
    (dolist (node (dom-non-text-children sense))
      (cond
       ((string= (dom-attr node 'class) "DEF")
        (setq text (concat text "  * " (longman-lookup--get-node-text node) "\n")))
       ((string= (dom-attr node 'class) "EXAMPLE")
        (setq text (concat text "    - " (longman-lookup--get-node-text node) "\n")))
       ((string= (dom-attr node 'class) "GramExa")
        (setq text (concat text (format "    - *%s*\n"
                                        (longman-lookup--get-node-text
                                         (dom-by-class node "PROPFORM")))))
        (dolist (ex (dom-by-class node "^EXAMPLE$"))
          (setq text (concat text "      - " (longman-lookup--get-node-text ex) "\n"))))
       ((string= (dom-attr node 'class) "Subsense")
        (setq text (concat text (longman-lookup--parse-sense node))))
       ((string= (dom-attr node 'class) "Crossref")
        (setq text (concat text "  * " (longman-lookup--get-node-text node) "\n")))))
    text))

(defun longman-lookup--parse-entry (entry)
  "Parse the entry ENTRY."
  (let* ((pos (string-trim (dom-text (car (dom-by-class entry "^POS$")))))
         (senses (dom-by-class entry "^Sense$"))
         (word (dom-text (car (dom-by-class entry "HWD$"))))
         (entry-header (concat "* "
                               word
                               (unless (string-empty-p pos) (format " (%s)" pos))
                               ":\n"))
         (entry-text ""))
    (dolist (sense senses)
      (let ((sense-text (longman-lookup--parse-sense sense)))
        (unless (string-empty-p sense-text)
          (setq entry-text (concat entry-text sense-text)))))
    (if (string-empty-p entry-text)
        nil
      (concat entry-header entry-text))))

;;;###autoload
(defun longman-lookup (word)
  "Fetch the definition of WORD from 'ldoceonline.com' and display it in an `org-mode' buffer."
  (interactive (list
                (let ((w (if (region-active-p)
                             (buffer-substring-no-properties (mark) (point))
                           (thing-at-point 'word))))
                  (if w (read-string (format "Enter word (%s): " w) nil nil w)
                    (read-string "Enter word: ")))))
  (let* ((entries-text nil)
         (header nil)
         (u (concat "https://www.ldoceonline.com/search/english/direct/?q="
                    (replace-regexp-in-string " " "+" word)))
         (temp-buf (url-retrieve-synchronously u)))
    (unless temp-buf
      (error "Fetch failed: %s" u))
    (with-current-buffer temp-buf
      (let* ((tree (libxml-parse-html-region (point-min) (point-max)))
             (entries (dom-by-class tree "^dictentry$")))
        (when (null tree)
          (error "Parse failed"))
        (setq header (longman-lookup--get-node-text (car (dom-by-tag tree 'h1))))
        (when (or (null header)
                  (null entries)
                  (string-prefix-p "Sorry, there are no results for" header)
                  (string-prefix-p "Did you mean" header))
          (error "Word not found: %s" word))
        (setq entries-text (mapconcat #'longman-lookup--parse-entry entries "")))
      (kill-buffer))
    ;; result buffer
    (let ((buf (get-buffer-create (format "*ldoce <%s>*" header))))
      (with-current-buffer buf
        (when buffer-read-only
          (setq buffer-read-only nil)
          (erase-buffer))
        (insert entries-text)
        (read-only-org-mode))
      (display-buffer buf))))

(provide 'longman-lookup)
;;; longman-lookup.el ends here
