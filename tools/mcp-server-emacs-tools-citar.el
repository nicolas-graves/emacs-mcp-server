;;; mcp-server-emacs-tools-citar.el --- Citar bibliography lookup MCP tool -*- lexical-binding: t; -*-

;;; Copyright © 2026 Nicolas Graves <ngraves@ngraves.fr>

;;; Commentary:

;; MCP tools for interacting with the citar bibliography manager.
;; Provides three operations:
;;   - citar-get-entry: look up a full entry by citation key.
;;   - citar-search: fuzzy-search entries and return the top match's citekey.
;; Requires `citar' to be installed and configured in Emacs.

;;; Code:

(require 'mcp-server-tools)

(defun mcp-server-emacs-tools--citar-get-entry-handler (args)
  "Handle citar-get-entry invocation with ARGS.
ARGS should contain a `key' field with the citation key to look up."
  (require 'citar)
  (let ((key (alist-get 'key args)))
    (unless key
      (error "Missing required parameter: key"))
    (let* ((entries (citar-get-entries))
           (entry (gethash key entries)))
      (if entry
          (let ((parts (list (format "=key= %s" key))))
            (dolist (cell entry)
              (unless (string-empty-p (cdr cell))
                (push (format "%s: %s" (car cell) (cdr cell)) parts)))
            (string-join (nreverse parts) "\n"))
        (format "No entry found for key: %s" key)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "citar-get-entry"
  :title "Citar: Get Bibliography Entry"
  :description "Look up a bibliographic entry by its citation key. Returns all fields (author, title, year, journal, etc.) for the given key from the configured bibliography sources."
  :input-schema '((type . "object")
                  (properties . ((key . ((type . "string")
                                         (description . "The citation key to look up (e.g. \"doe2024\").")))))
                  (required . ["key"]))
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false))
  :function #'mcp-server-emacs-tools--citar-get-entry-handler))

(defun mcp-server-emacs-tools--citar-search-handler (args)
  "Handle citar-search invocation with ARGS.
ARGS should contain a `query' field (e.g. \"Saussay 2022\").
Returns the citekey of the top match (all query words must appear in candidate)."
  (require 'citar)
  (let* ((query (alist-get 'query args))
         (candidates (or (citar--format-candidates)
                         (error "No bibliography set")))
         (words (split-string (downcase query)))
         (match (catch 'found
                  (maphash (lambda (candidate key)
                             (when (cl-every (lambda (word)
                                              (string-match-p (regexp-quote word)
                                                              (downcase candidate)))
                                            words)
                               (throw 'found key)))
                           candidates)
                  nil)))
    (or match "No match found")))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "citar-search"
  :title "Citar: Search Bibliography"
  :description "Fuzzy-search bibliography entries by author, year, title, or key. Returns the citekey of the top match. Prefer this over citar-list-entries when looking for a specific reference."
  :input-schema '((type . "object")
                  (properties . ((query . ((type . "string")
                                           (description . "Search string, such as \"energy policy 2023\".")))))
                  (required . ["query"]))
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false))
  :function #'mcp-server-emacs-tools--citar-search-handler))

(provide 'mcp-server-emacs-tools-citar)

;;; mcp-server-emacs-tools-citar.el ends here
