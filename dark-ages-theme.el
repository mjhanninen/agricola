;;; agricola-theme.el -- Agricola color theme

;;; Copyright © Matti Hänninen 2017-19

;;; Author: Matti Hänninen <matti@mjhanninen.com>
;;; Version: 0.1.0
;;; Package-Requires: ((emacs "24"))
;;; URL: https://github.com/mjhanninen/agricola
;;; Keywords: colortheme

;;; Commentary:

;;; Agricola

;;; Code:

(require 'agricola)

(deftheme dark-ages "A dark color theme.")

(agricola/defcolor
  dark-ages/black 0 0 0
  dark-ages/white 255 255 255
  dark-ages/light-gray 200 200 200
  dark-ages/light-red 255 127 131

  ;; Main content colors
  dark-ages/light-blue 195 206 223
  dark-ages/mid-blue 121 137 169
  dark-ages/dark-blue 19 29 49

  dark-ages/red 223 32 61
  dark-ages/green 96 142 11
  dark-ages/blue 24 124 167
  dark-ages/background-red 56 8 16
  dark-ages/gray 127 127 127
  dark-ages/background-gray 32 32 32)

(apply #'custom-theme-set-faces
       'dark-ages
 (agricola/with-abbreviated-prefix ((da dark-ages))
   (thread-first (agricola/new-spec)

     (agricola/set-properties (:foreground :background)
       'default da/light-blue da/black
       'linum da/mid-blue da/black
       'linum-relative-current da/gray da/black)

     ;; Linum needs to be derived from a face that sets everything. Otherwise
     ;; the text next to the linum column tends to bleed in.
     (agricola/inherit
       ('default 'linum 'linum-relative-current))

     (agricola/set-property
       'cursor :background da/red
       'minibuffer-prompt :foreground da/red
       'header-line :foreground da/red
       'hl-line :background da/background-red
       'reqion :background da/red
       'warning :foreground da/red)

     ;; Buttons and links
     (agricola/set-property
       'link :foreground da/white :underline nil)
     (agricola/inherit
       ('link 'button)
       ('link 'link-visited))

     ;; Whitespace annotation
     (agricola/set-properties (:background)
       'trailing-whitespace da/red
       'whitespace-trailing da/red)
     (agricola/set-properties (:foreground)
       'whitespace-space da/red
       'whitespace-newline da/red)

     ;; Parenthesis
     (agricola/set-property
       'show-paren-match :bold t
       'show-paren-match-expression :bold t
       'show-paren-mismatch :background da/red)

     ;; Mode-line
     (agricola/set-property
       'mode-line :background da/mid-blue :foreground da/black
       'mode-line-inactive :background da/dark-blue :foreground da/light-blue
       'mode-line-buffer-id :bold t
       'mode-line-emphasis :bold t
       'vertical-border :foreground da/mid-blue)

     ;; XXX(soija) Odd faces that I don't recognize
     (agricola/set-property
       'default-italic :italic t)

     ;; Info mode
     (agricola/set-property
       'info-quoted-name :foreground da/red)
     (agricola/inherit
       ('font-lock-string 'info-string))

     ;; Font-lock mode
     (agricola/set-property
       'font-lock-warning-face :foreground da/white :background da/red
       'font-lock-comment-face :foreground da/mid-blue
       'font-lock-doc-face :foreground da/mid-blue
       'font-lock-function-name-face :foreground da/white :bold t
       'font-lock-string-face :foreground da/white
       'font-lock-constant-face :foreground da/white)

     ;; Touch these so that the previous properties become cleansed
     (agricola/touch
       'font-lock-builtin-face
       'font-lock-keyword-face
       'font-lock-negation-char-face
       'font-lock-reference-face
       'font-lock-type-face
       'font-lock-variable-name-face)

     ;; Org mode
     (agricola/set-property
       'org-level-1 :foreground da/white :bold t
       )
     (agricola/inherit
       ('org-level-1 'org-level-2
                     'org-level-3
                     'org-level-4
                     'org-level-5
                     'org-level-6
                     'org-level-7
                     'org-level-8))
     (agricola/touch
       'org-date)

     ;; Outline mode
     (agricola/inherit
       ('org-level-1 'outline-1)
       ('org-level-2 'outline-2)
       ('org-level-3 'outline-3)
       ('org-level-4 'outline-4)
       ('org-level-5 'outline-5)
       ('org-level-6 'outline-6))

     ;; Markdown
     (agricola/inherit
       ('org-level-1 'markdown-header-face-1)
       ('org-level-2 'markdown-header-face-2)
       ('org-level-3 'markdown-header-face-3)
       ('org-level-4 'markdown-header-face-4)
       ('org-level-5 'markdown-header-face-5)
       ('org-level-6 'markdown-header-face-6)
       ('org-code 'markdown-code-face)
       ('org-block 'markdown-pre-face))

     ;; Info mode
     (agricola/set-property
       'info-title-1 :foreground da/white :bold t
       'info-menu-header :bold t
       'info-node :bold t
       ;; XXX(soija) this could be brighter
       'info-xref :foreground da/blue)
     (agricola/inherit
       ('info-title-1 'info-title-2 'info-title-3 'info-title-4))
     (agricola/inherit
       ('info-xref 'info-xref-visited 'info-header-xref)
       ('info-node 'info-header-node))
     (agricola/touch
       ;; This is totally usesless face
       'info-menu-star)

     ;; Dired
     (agricola/set-property
       'dired-flagged :foreground da/red
       'dired-header :bold t
       'dired-marked :foreground da/blue
       'dired-warning :background da/red)
     (agricola/inherit
       ('dired-marked 'dired-mark))
     (agricola/touch
       'dired-directory
       'dired-ignored
       'dired-perm-write
       'dired-symlink)

     ;; Calendar
     (agricola/set-property
       'calendar-month-header :bold t
       'calendar-today :foreground da/red
       'calendar-weekday-header :bold t
       'calendar-weekend-header :inherit 'calendar-weekday-header :foreground da/red)

     ;; Helm
     (agricola/set-property
       'helm-source-header :bold t :foreground da/red
       'helm-selection :bold t :background da/background-red
       'helm-match :foreground da/red
       'helm-M-x-key :foreground da/red)
     (agricola/touch
       'helm-selection-line
       ;; Git ls
       'helm-ls-git-added-copied-face
       'helm-ls-git-added-modified-face
       'helm-ls-git-conflict-face
       'helm-ls-git-deleted-and-staged-face
       'helm-ls-git-deleted-not-staged-face
       'helm-ls-git-modified-and-staged-face
       'helm-ls-git-modified-not-staged-face
       'helm-ls-git-renamed-modified-face
       'helm-ls-git-untracked-face
       ;; Buffers
       'helm-buffer-archive
       'helm-buffer-directory
       'helm-buffer-file
       'helm-buffer-modified
       'helm-buffer-not-saved
       'helm-buffer-process
       'helm-buffer-saved-out
       'helm-buffer-size
       'helm-non-file-buffer
       ;; Ff
       'helm-ff-denied
       'helm-ff-directory
       'helm-ff-dirs
       'helm-ff-dotted-directory
       'helm-ff-dotted-symlink-directory
       'helm-ff-executable
       'helm-ff-file
       'helm-ff-invalid-symlink
       'helm-ff-pipe
       'helm-ff- ;; omtted -> prefix
       'helm-ff-socket
       'helm-ff-suid
       'helm-ff-symlink
       'helm-ff-truename
       ;; Candidates
       'helm-candidate-number
       'helm-candidate-number-suspended)

     ;; Incremental search and highlighting
     (agricola/set-property
       'isearch :background da/white :foreground da/black
       ;; Failing postfix shown in ibuffer
       'isearch-fail :background da/red
       ;; Mouse-overs etc.
       'highlight :inherit 'unspecified :background da/white :foreground da/black
       ;; Delayed highlighting in incremental searching
       ;; XXX(soija) I'm not happy with having to force the black background.
       ;; Think about this.
       'lazy-highlight :bold t :foreground da/red :background da/black :inherit 'unspecified)

     ;; Clojure
     (agricola/touch
       'clojure-character-face
       'clojure-keyword-face
       ;; Stack traces
       'cider-stacktrace-face
       'cider-stacktrace-ns-face
       'cider-reader-conditional-face
       'cider-stacktrace-suppressed-button-face
       'cider-stacktrace-promoted-button-face
       ;; REPL
       'cider-repl-input-face)
     (agricola/set-property
       'cider-stacktrace-error-class-face :foreground da/red
       'cider-stacktrace-error-message-face :foreground da/red
       'cider-stacktrace-filter-active-face :foreground da/red
       'cider-stacktrace-filter-inactive-face :foreground da/white
       'cider-repl-prompt-face :foreground da/gray
       'cider-repl-stderr-face :foreground da/light-red
       'cider-repl-stdout-face :foreground da/green)

     ;; Magit
     (agricola/set-property
       'magit-section-heading :foreground da/white :bold t
       'magit-diff-file-heading :foreground da/white
       'magit-diff-hunk-region :foreground da/red
       'magit-diff-context :foreground da/mid-blue
       'magit-diff-context-highlight :foreground da/light-blue
       ;; +,-
       'magit-diff-added :background da/green :foreground da/white
       'magit-diff-added-highlight :background da/green :foreground da/white
       'magit-diff-removed :background da/background-red :foreground da/white
       'magit-diff-removed-highlight :background da/red :foreground da/white
       'magit-popup-heading :foreground da/white
       'magit-popup-argument :foreground da/red
       ;; Branching
       'magit-branch-current :foreground da/white

       )
     (agricola/inherit
       ('magit-diff-file-heading 'magit-diff-file-heading-highlight)
       ('magit-diff-file-heading 'magit-diff-hunk-heading)
       ('magit-diff-hunk-heading 'magit-diff-hunk-heading-highlight)
       ('magit-popup-argument 'magit-popup-option-value)
       ('magit-branch-current 'magit-branch-local)
       ('magit-branch-current 'magit-branch-remote)
       ('magit-branch-current 'magit-branch-remote-head)
       ('magit-branch-current 'magit-branch-upstream)

       )

     (agricola/touch
       'magit-section-secondary-heading
       'magit-section-heading-selection
       'magit-section-highlight
       'magit-diff-context-highlight
       'magit-diff-file-heading
       'magit-popup-key
       'magit-popup-disabled-argument
       'magit-log-author
       'magit-log-date
       'magit-log-graph
       'magit-hash
       'magit-diffstat-added
       'magit-diffstat-removed
       )

     ;; Done
     (agricola/compile-spec))))

(provide-theme 'agricola)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dark-ages-theme.el ends here
