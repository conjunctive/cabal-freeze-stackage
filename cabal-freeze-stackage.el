;;; cabal-freeze-stackage.el --- generate a Cabal freeze file from Stackage -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Conjunctive

;; Author: Conjunctive <conjunctive@protonmail.com>
;; Keywords: haskell cabal stackage
;; Version: 0.0.2
;; URL: https://github.com/conjunctive/cabal-freeze-stackage
;; Package-Requires: ((emacs "27"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; (cabal-freeze-file-for-system-ghc-in-project)
;; (cabal-freeze-file-for-system-ghc "~/haskell-project" 5)
;; (cabal-freeze-file-for-ghc "8.0.2" "~/haskell-project" 30)
;; (cabal-freeze-file-for-ghc "8.6.3" "~/haskell-project" 20 t)
;; (cabal-freeze-file-for-resolver "lts-16.22" "~/haskell-project")

;;; Code:

(require 'cl-macs)
(require 'dom)
(require 'simple)
(require 'subr-x)
(require 'url)

(defun fetch-system-ghc-version ()
  "Retrieve the currently installed GHC version number."
  (let ((ghc-version-str (shell-command-to-string "ghc --version")))
    (save-excursion
      (save-match-data
        (with-temp-buffer
          (insert ghc-version-str)
          (goto-char (point-min))
          (re-search-forward "[0-9].[0-9].[0-9]" nil t)
          (match-string 0))))))

(defun fetch-stackage-lts-resolver-url (ghc-version &optional max-page-lookup use-nightly)
  "Find the URL for the most recent LTS Stackage resolver that is compatible
with the provided GHC-VERSION.  Optionally provide a MAX-PAGE-LOOKUP to halt
the search after a certain amount of requests.  USE-NIGHTLY will fetch
a compatible nightly Stackage resolver instead of an LTS resolver."
  (cl-labels
      ((f (page)
          (cl-loop with html = (if-let ((snapshots (url-retrieve-synchronously
                                                    (concat "https://www.stackage.org/snapshots?page="
                                                            (number-to-string page)))))
                                   (prog1 (with-current-buffer snapshots
                                            (libxml-parse-html-region (point-min) (point-max)))
                                     (kill-buffer snapshots))
                                 (error "Unable to retrieve snapshots"))
                   with acc = nil
                   with resolver = nil
                   for ul in (dom-by-class html "snapshots")
                   when (eq 'ul (dom-tag ul))
                   do (cl-loop for li in (dom-children ul)
                               when (and (listp li) (eq 'li (dom-tag li)))
                               do (when-let ((strong (dom-child-by-tag li 'strong)))
                                    (when-let ((a (dom-child-by-tag strong 'a)))
                                      (let ((desc (dom-text a)))
                                        (when (string-match ghc-version desc)
                                          (when (and (not resolver)
                                                     (string-prefix-p (if use-nightly
                                                                          "Stackage Nightly"
                                                                          "LTS")
                                                                      desc))
                                            (setq resolver (dom-attr a 'href))))))))
                   finally return (if (and (not resolver)
                                           (< page (or max-page-lookup 8)))
                                      (f (+ 1 page))
                                      resolver))))
    (f 1)))

(defun fetch-cabal-freeze-file (output-directory stackage-resolver-url)
  "Retrieve the Cabal freeze file with the STACKAGE-RESOLVER-URL.
The resulting file will be saved to the provided OUTPUT-DIRECTORY."
  (if-let ((freeze-file (url-retrieve-synchronously (concat stackage-resolver-url "/cabal.config"))))
      (prog1 (with-current-buffer freeze-file
               (delete-region (point-min) url-http-end-of-headers)
               (setq buffer-file-name (concat output-directory "/cabal.project.freeze"))
               (save-buffer))
        (kill-buffer freeze-file))
    (error "Unable to retrieve freeze file")))

(defun cabal-freeze-file-for-resolver (resolver output-directory)
  "Retrieve a Cabal freeze file for the specified Stackage snapshot.
Selection is based on the provided RESOLVER (eg. \"lts-16.22\" or \"nightly-2021-02-23\").
The resulting file will be saved to the provided OUTPUT-DIRECTORY."
  (let ((out-dir (directory-file-name output-directory)))
    (if (file-exists-p out-dir)
        (let ((resolver-url (concat "https://www.stackage.org/" resolver)))
          (fetch-cabal-freeze-file output-directory resolver-url))
        (error "Specified output directory does not exist"))))

(defun cabal-freeze-file-for-ghc (ghc-version output-directory &optional max-page-lookup use-nightly)
  "Retrieve a Cabal freeze file for the latest compatible LTS Stackage snapshot.
Selection is based on the provided GHC-VERSION number (eg. \"8.8.4\").
The resulting file will be saved to the provided OUTPUT-DIRECTORY.
Optionally provide a MAX-PAGE-LOOKUP to avoid making too many requests.
USE-NIGHTLY will use a nightly Stackage resolver instead of an LTS resolver."
  (let ((out-dir (directory-file-name output-directory)))
    (if (file-exists-p out-dir)
        (if-let ((resolver-url (fetch-stackage-lts-resolver-url ghc-version max-page-lookup use-nightly)))
            (fetch-cabal-freeze-file output-directory resolver-url)
          (error "Unable to retrieve Stackage resolver URL"))
        (error "Specified output directory does not exist"))))

(defun cabal-freeze-file-for-system-ghc (output-directory &optional max-page-lookup use-nightly)
  "Retrieve a Cabal freeze file for the latest compatible LTS Stackage snapshot.
Selection is based on the version of GHC currently installed on the system.
The resulting file will be saved to the provided OUTPUT-DIRECTORY.
Optionally provide a MAX-PAGE-LOOKUP to avoid making too many requests.
USE-NIGHTLY will use a nightly Stackage resolver instead of an LTS resolver."
  (if-let ((ghc-version (fetch-system-ghc-version)))
      (cabal-freeze-file-for-ghc ghc-version output-directory max-page-lookup use-nightly)
    (error "Unable to retrieve GHC version")))

;;;###autoload

(defun cabal-freeze-file-for-system-ghc-in-project (&optional max-page-lookup use-nightly)
  "Retrieve a Cabal freeze file for the latest compatible LTS Stackage snapshot.
Selection is based on the version of GHC currently installed on the system.
The resulting file will be saved to the root of the current Projectile project.
Optionally provide a MAX-PAGE-LOOKUP to avoid making too many requests.
USE-NIGHTLY will use a nightly Stackage resolver instead of an LTS resolver."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (cabal-freeze-file-for-system-ghc project-root max-page-lookup use-nightly)
    (error "Unable to locate root directory of project")))

(provide 'cabal-freeze-stackage)

;;; cabal-freeze-stackage.el ends here
