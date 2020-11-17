;;; longman-lookup.el --- Lookup a word in Longman Dictionary  -*- lexical-binding: t -*-

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Keywords: tools, extensions
;; URL: "https://github.com/hikmet517/longman-lookup.el"
;; Keywords: convenience

;;; Commentary:
;; Lookup a word in Longman English Dictionary and create an org mode buffer
;; containing definitions.

;;; TODO:
;; some entries have only references (REFHWD), title is not needed for them
;; - https://www.ldoceonline.com/dictionary/beyond

;;; Code:
(require 'url)
(require 'dom)
(require 'org)
(require 'thingatpt)


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
        (setq text (concat text (longman-lookup--parse-sense node))))))
    text))


(defun longman-lookup--parse-entry (entry name)
  "Parse ENTRY with the name NAME."
  (let* ((pos (string-trim (dom-text (car (dom-by-class entry "^POS$")))))
         (senses (dom-by-class entry "^Sense$"))
         (text (format "* %s (%s):\n" name pos)))
    (dolist (sense senses)
      (setq text (concat text (longman-lookup--parse-sense sense))))
    text))


;;;###autoload
(defun longman-lookup (word)
  "Fetch the definition of WORD from 'ldoceonline.com' and create an org mode buffer to display it."
  (interactive (list
                (let ((w (thing-at-point 'word)))
                  (if w (read-string (format "Enter word (%s): " w)
                                     nil nil w)
                    (read-string "Enter word: ")))))
  (let* ((entries-text nil)
         (header nil)
         (u (concat "https://www.ldoceonline.com/search/english/direct/?q="
                    (replace-regexp-in-string " " "+" word)))
         (temp-buf (url-retrieve-synchronously u)))
    (unless temp-buf
      (error "Fetch failed: %s\n" u))
    (with-current-buffer temp-buf
      (let* ((b (point-min))
             (e (point-max))
             (tree (libxml-parse-html-region b e))
             (entries (dom-by-class tree "^dictentry$")))
        (setq header (longman-lookup--get-node-text (car (dom-by-tag tree 'h1))))
        (setq entries-text (mapconcat
                            (lambda (e) (longman-lookup--parse-entry e header))
                            entries
                            "")))
      (kill-buffer))
    (when (or (null tree)
              (null header)
              (null entries))
      (error "Parse failed or necessary information cannot be found."))
    (when (or (string-prefix-p "Sorry, there are no results for" header)
              (string-prefix-p "Did you mean" header))
      (error "Word not found: %s\n" word))
    (let ((buf (get-buffer-create (format "ldoce <%s>" header))))
      (with-current-buffer buf
        (org-mode)
        (insert entries-text))
      (display-buffer buf))))


(provide 'longman-lookup)
;;; longman-lookup.el ends here
