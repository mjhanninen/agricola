;;; agricola-theme.el -- Agricola color theme

;;; Copyright © Matti Hänninen 2017

;;; Author: Matti Hänninen <matti@mjhanninen.com>
;;; Version: 0.1.0
;;; Package-Requires: ((emacs "24"))
;;; URL: https://github.com/mjhanninen/agricola
;;; Keywords: colortheme

;;; Commentary:

;;; Agricola, the purple turtle.

;;; Code:

(require 'agricola)

(deftheme agricola "A light color.")

;;;
;;; Utilities
;;;


;;; Color definitions
;;;

;;; ## Emacs colorspace
;;;
;;; Emacs uses sRGB colorspace.  All the values below are sRGB coordinates.
;;;
;;; ## Sources
;;;
;;; | agricola-1982 | Agricola 1982 stamp on Wikipedia |
;;;
;;; ## Ink
;;;
;;; The default foreground color, or *the ink*, is black.
;;;
;;; | Source        |  R |  G |  B |
;;; |:--------------|---:|---:|---:|
;;; | agricola-1982 | 53 | 40 | 47 |
;;;
;;; ## Light background
;;;
;;; | agricola-1982 | 241 | 242 | 236 |
;;;
;;; ## Background
;;;
;;; | Source                       | R   | G   | B   |    H |    S |    L |
;;; |------------------------------|-----|-----|-----|-----:|-----:|-----:|
;;; | Pragmata Pro Fraktur example | 232 | 226 | 214 | 1111 | 2812 | 8745 |
;;; | Pragmata Pro Haskell example | 236 | 233 | 224 | 1250 | 2400 | 9019 |
;;; | Input Mono example           | 215 | 216 | 191 | 1733 | 2327 | 7980 |
;;; | agricola-1982 (bright)       | 241 | 242 | 236 | 1945 | 1875 | 9373 |
;;; | agricola-1982                | 210 | 202 | 175 | 1286 | 2800 | 7549 |
;;;
;;; It can be seen that the saturation varies
;;; Hue between .12 (dark) and .30 (bright)
;;;
;;; |    L |    S |
;;; | 9400 | 1800 |
;;; | 9000 | 2400 |
;;; | 7600 | 2800 |
;;;
;;; ## The Red
;;;
;;; | Pragmata Pro Fraktur example | 204 |  8 | 30 |
;;; | Pragmata Pro Haskell example | 234 | 68 | 79 |
;;; | agricola-1982 | 173 | 82 | 60
;;;
;;; ## The Green
;;;
;;; | Pragmata Pro Haskell example | 52 | 144 | 54 |
;;;
;;; ## Colors
;;;
;;; | Red   | 0.975 | 0.750 | 0.500 |
;;; | Green | 0.225 | 0.850 | 0.300 |
;;; | Blue  | 0.550 | 0.750 | 0.375 |
;;; | Ind

(agricola/defcolor
  agricola/black 0 0 0
  agricola/white 255 255 255
  ;; Background
  agricola/background 236 233 224
  ;; Base colors
  agricola/red 223 32 61
  agricola/green 96 142 11
  agricola/blue 24 124 167
  ;; Ink
  agricola/ink 118 103 126
  )

(comment
  (agricola/with-abbreviated-prefix (a agricola)

    (thread-first (agricola/new-spec)

      (agricola/set-properties (:foreground :background)
        'default a/black a/white
        'linum a/ink a/background
        'linum-relative-current a/ink a/background)

      ;; Linum needs to be derived from a face that sets everything. Otherwise
      ;; the text next to the linum column tends to bleed in.
      (agricola/inherit
        ('default 'linum 'linum-relative-current))

      (agricola/set-property
        'cursor :background a/red
        'highlight :foreground a/red :background a/white
        'lazy-highlight :inherit 'highlight :background a/background-light
        'minibuffer-prompt :foreground a/red
        'header-line :foreground a/red
        'link :foreground a/green :underline t
        'hl-line :background a/white
        'reqion :background a/background-light
        'show-parent-match-face :background a/blue
        'warning :foreground a/red)

      ;; Whitespace annotation
      (agricola/set-properties (:background)
        'trailing-whitespace a/red
        'whitespace-trailing a/red)
      (agricola/set-properties (:foreground)
        'whitespace-space a/red
        'whitespace-newline a/red)

      ;; XXX(soija) Odd faces that I don't recognize
      (agricola/set-property
        'default-italic :italic t)

      ;; Info mode
      (agricola/set-property
        'info-quoted-name :foreground a/red)
      (agricola/inherit
        ('font-lock-string 'info-string))

      ;; Font-lock mode
      (agricola/set-property
        font-lock-warning-face :foreground a/white :background a/red
        font-lock-comment-face :foreground a/ink
        font-lock-doc-face :foreground a/ink
        font-lock-function-name-face :bold t
        font-lock-string-face :foreground a/green
        font-lock-constant-face :foreground a/green)
      ;; Touch these so that the previous properties become cleansed
      (agricola/touch
       font-lock-builtin-face
       font-lock-keyword-face
       font-lock-negation-char-face
       font-lock-reference-face
       font-lock-type-face
       font-lock-variable-name-face)

      ;; Org mode
      (agricola/set-property
        'org-level-1 :foreground red
        'org-level-1 :bold t)
      (agricola/inherit
        ('org-level-1 'org-level-2
                      'org-level-3
                      'org-level-4
                      'org-level-5
                      'org-level-6
                      'org-level-7
                      'org-level-8))

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
        ('org-block 'markdown-pre-face)))))

(comment
  (let* ((class '((class color) (min-colors 89)))
         (agricola "#663399")
         (ninja   "#333353")

         ;; default background
         (base00 (if (display-graphic-p)
                     agricola/background
                   nil))

         ;; lighter background (status bar)
         (base01 agricola/ink)

         (base02 agricola/ink) ;; selection background

         (base03 agricola/ink) ;; comments, invisibles

         (base04 agricola/black) ;; dark foreground (status bar)

         (base05 agricola/black) ;; default foreground

         (base06 (if (display-graphic-p) "#ccccff" "#afafff")) ;; light foreground

         (base07 (if (display-graphic-p) "#53495d" "#4e4e4e")) ;; light background

         (base08 agricola/black) ;; variables

         (base09 agricola/black) ;; constants

         (base0A agricola/ink) ;; search text background

         (base0B agricola/green) ;; strings

         (base0C agricola/green) ;; regex, escaped chars

         (base0D agricola/black) ;; functions

         (base0E agricola/red) ;; keywords

         (base0F agricola/blue) ;; deprecations

         (fg1  base05)
         (fg2  base06)
         (fg3  base0C)
         (fg4  base04)
         (bg0  base02)
         (bg1  base00)
         (bg2  base02)
         (bg3  base02)
         (bg4  base07)
         (bg5  base02)
         (key2 base0E)
         (key3 base0F)

         (builtin base0C)
         (keyword base0E)
         (const   base09)
         (comment base03)
         (func    base0D)
         (str     base0B)
         (type    base0A)
         (var     base08)
         (warning base0F)

         (rainbow-1 base03)
         (rainbow-2 base08)
         (rainbow-3 base06)
         (rainbow-4 base0A)
         (rainbow-5 base0B)
         (rainbow-6 base0C)
         (rainbow-7 base0D)
         (rainbow-8 base0E)
         (rainbow-9 base0F)

         (eph-verbatim base0D)
         (eph-code     base0F)

         (ex-attr base0F)
         (ex-atom base06)
         )

    (custom-theme-set-faces
     'agricola
     ;; Default

   ;;;;;;;
     ;; default
     `(ffap ((,class (:foreground ,fg4))))
     `(fringe ((,class (:background ,bg1 :foreground ,fg4))))
     `(tooltip ((,class (:foreground ,fg2 :background ,bg0))))
     `(vertical-border ((,class (:foreground ,bg2))))

     ;; auto-complete
     `(ac-completion-face ((,class (:underline t :foreground ,keyword))))

     ;; company
     `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
     `(company-preview ((,class (:background ,bg1 :foreground ,key2))))
     `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
     `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
     `(company-scrollbar-bg ((,class (:background ,bg3))))
     `(company-scrollbar-fg ((,class (:foreground ,keyword))))
     `(company-template-field ((,class (:inherit region))))
     `(company-tooltip ((,class (:inherit tooltip :bold t))))
     `(company-tooltip-annotation ((,class (:foreground ,const))))
     `(company-tooltip-common ((,class ( :foreground ,fg3))))
     `(company-tooltip-common-selection ((,class (:foreground ,str))))
     `(company-tooltip-mouse ((,class (:inherit highlight))))
     `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))

     ;; diff-hl
     `(diff-hl-change ((,class (:foreground ,rainbow-5 :background ,rainbow-5))))
     `(diff-hl-delete ((,class (:foreground ,rainbow-9 :background ,rainbow-9))))
     `(diff-hl-insert ((,class (:foreground ,rainbow-6 :background ,rainbow-6))))

     ;; enh-ruby
     `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,str))))
     `(enh-ruby-op-face ((,class (:foreground ,keyword))))
     `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,str))))
     `(enh-ruby-string-delimiter-face ((,class (:foreground ,str))))

     ;; elixir
     `(elixir-attribute-face ((,class (:foreground ,ex-attr))))
     `(elixir-atom-face ((,class (:foreground ,ex-atom))))

     ;; evil
     `(evil-ex-lazy-highlight ((,class (:inherit lazy-highlight))))
     `(evil-search-highlight-persist-highlight-face ((,class (:background ,type :foreground ,bg5))))

     ;; font-latex
     `(font-latex-bold-face ((,class (:foreground ,type))))
     `(font-latex-italic-face ((,class (:foreground ,key3 :italic t))))
     `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
     `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
     `(font-latex-string-face ((,class (:foreground ,str))))

     ;; gnus-group
     `(gnus-group-mail-1 ((,class (:foreground ,keyword :bold t))))
     `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :bold nil))))
     `(gnus-group-mail-2 ((,class (:foreground ,const :bold t))))
     `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :bold nil))))
     `(gnus-group-mail-3 ((,class (:foreground ,comment :bold t))))
     `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :bold nil))))
     `(gnus-group-mail-low ((,class (:foreground ,bg5 :bold t))))
     `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-mail-low :bold nil))))
     `(gnus-group-news-1 ((,class (:foreground ,keyword :bold t))))
     `(gnus-group-news-1-empty ((,class (:inherit gnus-group-news-1 :bold nil))))
     `(gnus-group-news-2 ((,class (:foreground ,const :bold t))))
     `(gnus-group-news-2-empty ((,class (:inherit gnus-group-news-2 :bold nil))))
     `(gnus-group-news-3 ((,class (:foreground ,comment :bold t))))
     `(gnus-group-news-3-empty ((,class (:inherit gnus-group-news-3 :bold nil))))
     `(gnus-group-news-4 ((,class (:inherit gnus-group-news-low))))
     `(gnus-group-news-4-empty ((,class (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-5 ((,class (:inherit gnus-group-news-low))))
     `(gnus-group-news-5-empty ((,class (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-6 ((,class (:inherit gnus-group-news-low))))
     `(gnus-group-news-6-empty ((,class (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-low ((,class (:foreground ,bg5 :bold t))))
     `(gnus-group-news-low-empty ((,class (:inherit gnus-group-news-low :bold nil))))
     `(gnus-header-content ((,class (:foreground ,keyword))))
     `(gnus-header-from ((,class (:foreground ,var))))
     `(gnus-header-name ((,class (:foreground ,type))))
     `(gnus-header-subject ((,class (:foreground ,func :bold t))))
     `(gnus-summary-markup-face ((,class (:foreground ,const))))
     `(gnus-summary-normal-ancient ((,class (:inherit gnus-summary-normal-read))))
     `(gnus-summary-normal-read ((,class (:foreground ,bg5 :weight normal))))
     `(gnus-summary-normal-ticked ((,class (:foreground ,keyword :weight light))))
     `(gnus-summary-normal-unread ((,class (:foreground ,comment :weight normal))))
     `(gnus-summary-selected ((,class (:inverse-video t))))

     ;; helm
     `(helm-match ((,class (:inherit highlight))))
     `(helm-bookmark-w3m ((,class (:foreground ,type))))
     `(helm-buffer-directory ((,class (:foreground ,type :background ,bg1))))
     `(helm-buffer-not-saved ((,class (:foreground ,str :background ,bg1))))

     `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
     `(helm-buffer-saved-out ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-buffer-size ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-candidate-number ((,class (:foreground ,bg1 :background ,fg1))))
     `(helm-ff-directory ((,class (:foreground ,func :background ,bg1 :weight bold))))
     `(helm-ff-executable ((,class (:foreground ,key2 :background ,bg1 :weight normal))))
     `(helm-ff-file ((,class (:foreground ,fg1 :background ,bg1 :weight normal))))
     `(helm-ff-invalid-symlink ((,class (:foreground ,key3 :background ,bg1 :weight bold))))
     `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
     `(helm-ff-symlink ((,class (:foreground ,keyword :background ,bg1 :weight bold))))
     `(helm-grep-cmd-line ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-grep-file ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-grep-finish ((,class (:foreground ,fg2 :background ,bg1))))
     `(helm-grep-lineno ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
     `(helm-grep-running ((,class (:foreground ,func :background ,bg1))))
     `(helm-header ((,class (:foreground ,fg2 :background ,bg1 :underline nil :box nil))))
     `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg1))))
     `(helm-selection ((,class (:background ,bg2 :underline nil))))
     `(helm-selection-line ((,class (:background ,bg2))))
     `(helm-separator ((,class (:foreground ,type :background ,bg1))))
     `(helm-source-go-package-godoc-description ((,class (:foreground ,str))))
     `(helm-source-header ((,class (:foreground ,type :background ,bg1 :underline nil :weight bold))))
     `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
     `(helm-time-zone-home ((,class (:foreground ,type :background ,bg1))))
     `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))
     ;; icomplete
     `(icompletep-determined ((,class :foreground ,builtin)))
     ;; ido
     `(ido-first-match ((,class (:foreground ,keyword :bold t))))
     `(ido-only-match ((,class (:foreground ,warning))))
     `(ido-subdir ((,class (:foreground ,builtin))))
     ;; isearch
     `(isearch ((,class (:bold t :foreground ,key3 :background ,bg0))))
     `(isearch-fail ((,class (:foreground ,bg0 :background ,key3))))
     ;; jde-java
     `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
     `(jde-java-font-lock-modifier-face ((t (:foreground ,key3))))
     `(jde-java-font-lock-number-face ((t (:foreground ,var))))
     `(jde-java-font-lock-package-face ((t (:foreground ,var))))
     `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
     `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
     ;; js2-mode
     `(js2-external-variable ((,class (:foreground ,type  ))))
     `(js2-function-param ((,class (:foreground ,var))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,key2))))
     `(js2-jsdoc-value ((,class (:foreground ,str))))
     `(js2-private-function-call ((,class (:foreground ,const))))
     `(js2-private-member ((,class (:foreground ,fg3))))
     ;; js3-mode
     `(js3-error-face ((,class (:underline ,warning))))
     `(js3-external-variable-face ((,class (:foreground ,var))))
     `(js3-function-param-face ((,class (:foreground ,key3))))
     `(js3-instance-member-face ((,class (:foreground ,const))))
     `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
     `(js3-warning-face ((,class (:underline ,keyword))))
     ;; magit
     `(magit-branch ((,class (:foreground ,const :weight bold))))
     `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
     `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
     `(magit-diffstat-added   ((,class (:foreground ,type))))
     `(magit-diffstat-removed ((,class (:foreground ,var))))
     `(magit-hash ((,class (:foreground ,fg2))))
     `(magit-hunk-heading           ((,class (:background ,bg3))))
     `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
     `(magit-item-highlight ((,class :background ,bg3)))
     `(magit-log-author ((,class (:foreground ,fg3))))
     `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
     `(magit-process-ok ((,class (:foreground ,func :weight bold))))
     `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
     `(magit-section-highlight      ((,class (:background ,bg2))))
     ;; mode-line
     `(mode-line ((,class (:foreground ,type :background ,agricola))))
     `(mode-line-inactive ((,class (:inherit mode-line :background ,bg1 :foreground ,comment :box ,comment))))
     `(mode-line-buffer-id ((,class (:foreground ,fg1))))
     ;; mu4e
     `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
     `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
     `(mu4e-header-marks-face ((,class (:foreground ,type))))
     `(mu4e-view-url-number-face ((,class (:foreground ,type))))
     ;; neotree
     `(neo-dir-link-face ((,class (:foreground ,rainbow-4))))
     `(neo-root-dir-face ((,class (:foreground ,rainbow-9))))
     ;; org
     `(org-agenda-date ((,class (:foreground ,rainbow-2 :underline nil))))
     `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
     `(org-agenda-done ((,class (:foreground ,rainbow-6))))
     `(org-agenda-structure ((,class (:foreground ,rainbow-3))))
     `(org-block ((,class (:foreground ,rainbow-5))))
     `(org-code ((,class (:foreground ,rainbow-7))))
     `(org-column ((,class (:background ,bg4))))
     `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
     `(org-date ((,class (:foreground ,rainbow-2 :underline t))))
     `(org-document-info ((,class (:foreground ,rainbow-8))))
     `(org-document-info-keyword ((,class (:foreground ,comment))))
     `(org-document-title ((,class (:weight bold :foreground ,rainbow-5 :height 1.44))))
     `(org-done ((,class (:foreground ,rainbow-6))))
     `(org-ellipsis ((,class (:foreground ,comment))))
     `(org-footnote ((,class (:foreground ,rainbow-8))))
     `(org-formula ((,class (:foreground ,rainbow-4))))
     `(org-headline-done ((,class (:foreground ,comment :bold nil :strike-through t))))
     `(org-hide ((,class (:foreground ,bg1 :background ,bg1))))
     `(org-link ((,class (:foreground ,rainbow-2 :underline t))))
     `(org-priority ((,class (:foreground ,rainbow-2))))
     `(org-scheduled ((,class (:foreground ,rainbow-6))))
     `(org-scheduled-previously ((,class (:foreground ,rainbow-7))))
     `(org-scheduled-today ((,class (:foreground ,rainbow-6))))
     `(org-sexp-date ((,class (:foreground ,fg4))))
     `(org-special-keyword ((,class (:foreground ,rainbow-7))))
     `(org-table ((,class (:foreground ,rainbow-3))))
     `(org-tag ((,class (:foreground ,rainbow-4 :bold t :background ,bg2))))
     `(org-todo ((,class (:foreground ,rainbow-5 :bold t :background ,bg2))))
     `(org-upcoming-deadline ((,class (:foreground ,rainbow-7))))
     `(org-warning ((,class (:weight bold :foreground ,rainbow-4))))
     ;; powerline
     `(powerline-active1 ((,class (:inherit mode-line))))
     `(powerline-active2 ((,class (:inherit powerline-active :background ,bg2))))
     `(powerline-inactive1 ((,class (:inherit mode-line-inactive))))
     `(powerline-inactive2 ((,class (:inherit powerline-inactive))))
     `(powerline-evil-base-face ((t (:foreground ,bg2))))
     `(powerline-evil-emacs-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-7))))
     `(powerline-evil-insert-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-2))))
     `(powerline-evil-motion-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-3))))
     `(powerline-evil-normal-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-6))))
     `(powerline-evil-operator-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-4))))
     `(powerline-evil-replace-face ((,class (:inherit powerline-evil-base-face :background "#ff5555"))))
     `(powerline-evil-visual-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-5))))
     ;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,class :foreground ,rainbow-1)))
     `(rainbow-delimiters-depth-2-face ((,class :foreground ,rainbow-2)))
     `(rainbow-delimiters-depth-3-face ((,class :foreground ,rainbow-3)))
     `(rainbow-delimiters-depth-4-face ((,class :foreground ,rainbow-4)))
     `(rainbow-delimiters-depth-5-face ((,class :foreground ,rainbow-5)))
     `(rainbow-delimiters-depth-6-face ((,class :foreground ,rainbow-6)))
     `(rainbow-delimiters-depth-7-face ((,class :foreground ,rainbow-7)))
     `(rainbow-delimiters-depth-8-face ((,class :foreground ,rainbow-8)))
     `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
     ;; rpm-spec
     `(rpm-spec-dir-face ((,class (:foreground ,rainbow-6))))
     `(rpm-spec-doc-face ((,class (:foreground ,rainbow-4))))
     `(rpm-spec-ghost-face ((,class (:foreground ,rainbow-3))))
     `(rpm-spec-macro-face ((,class (:foreground ,rainbow-7))))
     `(rpm-spec-obsolete-tag-face ((,class (:inherit font-lock-warning-face))))
     `(rpm-spec-package-face ((,class (:foreground ,rainbow-3))))
     `(rpm-spec-section-face ((,class (:foreground ,rainbow-7))))
     `(rpm-spec-tag-face ((,class (:foreground ,rainbow-2))))
     `(rpm-spec-var-face ((,class (:foreground "#a0522d"))))
     ;; slime
     `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
     ;; spam
     `(spam ((,class (:inherit gnus-summary-normal-read :foreground ,warning :strike-through t :slant oblique))))
     ;; spacemacs
     `(spacemacs-normal ((,class (:background ,agricola))))
     `(spacemacs-replace ((,class (:background ,str))))
     ;; term
     `(term ((,class (:foreground ,fg1 :background ,bg1))))
     `(term-color-black ((,class (:foreground ,bg3 :background ,bg3))))
     `(term-color-blue ((,class (:foreground ,func :background ,func))))
     `(term-color-cyan ((,class (:foreground ,str :background ,str))))
     `(term-color-green ((,class (:foreground ,type :background ,bg3))))
     `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
     `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
     `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))
     `(term-color-yellow ((,class (:foreground ,var :background ,var))))
     ;; undo-tree
     `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
     `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
     `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
     `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
     ;; web-mode
     `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
     `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
     `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,type))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,func))))
     `(web-mode-html-tag-face ((,class (:foreground ,keyword :bold t))))
     `(web-mode-keyword-face ((,class (:foreground ,keyword))))
     `(web-mode-string-face ((,class (:foreground ,str))))
     `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
     `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
     ;; which-func
     `(which-func ((,class (:inherit ,font-lock-function-name-face))))
     `(dired-directory ((,class (:foreground ,func :weight normal))))
     `(dired-flagged ((,class (:foreground ,keyword))))
     `(dired-header ((,class (:foreground ,type))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,var :weight bold))))
     `(dired-marked ((,class (:foreground ,builtin :weight bold))))
     `(dired-perm-write ((,class (:foreground ,fg3 :underline t))))
     `(dired-symlink ((,class (:foreground ,str :weight normal :slant italic))))
     `(dired-warning ((,class (:foreground ,warning :underline t))))
     `(diredp-compressed-file-name ((,class (:foreground ,fg3))))
     `(diredp-compressed-file-suffix ((,class (:foreground ,fg4))))
     `(diredp-date-time ((,class (:foreground ,var))))
     `(diredp-deletion-file-name ((,class (:foreground ,keyword :background ,bg5))))
     `(diredp-deletion ((,class (:foreground ,keyword :weight bold))))
     `(diredp-dir-heading ((,class (:foreground ,fg2 :background ,bg4))))
     `(diredp-dir-name ((,class (:inherit dired-directory))))
     `(diredp-dir-priv ((,class (:inherit dired-directory))))
     `(diredp-executable-tag ((,class (:foreground ,builtin))))
     `(diredp-file-name ((,class (:foreground ,fg1))))
     `(diredp-file-suffix ((,class (:foreground ,fg4))))
     `(diredp-flag-mark-line ((,class (:foreground ,fg2 :slant italic :background ,bg5))))
     `(diredp-flag-mark ((,class (:foreground ,fg2 :weight bold :background ,bg5))))
     `(diredp-ignored-file-name ((,class (:foreground ,fg1))))
     `(diredp-mode-line-flagged ((,class (:foreground ,warning))))
     `(diredp-mode-line-marked ((,class (:foreground ,warning))))
     `(diredp-no-priv ((,class (:foreground ,fg1))))
     `(diredp-number ((,class (:foreground ,const))))
     `(diredp-other-priv ((,class (:foreground ,builtin))))
     `(diredp-rare-priv ((,class (:foreground ,builtin))))
     `(diredp-read-priv ((,class (:foreground ,type))))
     `(diredp-write-priv ((,class (:foreground ,keyword))))
     `(diredp-exec-priv ((,class (:foreground ,str))))
     `(diredp-symlink ((,class (:foreground ,warning))))
     `(diredp-link-priv ((,class (:foreground ,warning))))
     `(diredp-autofile-name ((,class (:foreground ,str))))
     `(diredp-tagged-autofile-name ((,class (:foreground ,str))))
     `(icicle-whitespace-highlight               ((,class (:background ,var))))
     `(icicle-special-candidate                  ((,class (:foreground ,fg2))))
     `(icicle-extra-candidate                    ((,class (:foreground ,fg2))))
     `(icicle-search-main-regexp-others          ((,class (:foreground ,var))))
     `(icicle-search-current-input               ((,class (:foreground ,keyword))))
     `(icicle-search-context-level-8             ((,class (:foreground ,warning))))
     `(icicle-search-context-level-7             ((,class (:foreground ,warning))))
     `(icicle-search-context-level-6             ((,class (:foreground ,warning))))
     `(icicle-search-context-level-5             ((,class (:foreground ,warning))))
     `(icicle-search-context-level-4             ((,class (:foreground ,warning))))
     `(icicle-search-context-level-3             ((,class (:foreground ,warning))))
     `(icicle-search-context-level-2             ((,class (:foreground ,warning))))
     `(icicle-search-context-level-1             ((,class (:foreground ,warning))))
     `(icicle-search-main-regexp-current         ((,class (:foreground ,fg1))))
     `(icicle-saved-candidate                    ((,class (:foreground ,fg1))))
     `(icicle-proxy-candidate                    ((,class (:foreground ,fg1))))
     `(icicle-mustmatch-completion               ((,class (:foreground ,type))))
     `(icicle-multi-command-completion           ((,class (:foreground ,fg2 :background ,bg2))))
     `(icicle-msg-emphasis                       ((,class (:foreground ,func))))
     `(icicle-mode-line-help                     ((,class (:foreground ,fg4))))
     `(icicle-match-highlight-minibuffer         ((,class (:foreground ,builtin))))
     `(icicle-match-highlight-Completions        ((,class (:foreground ,func))))
     `(icicle-key-complete-menu-local            ((,class (:foreground ,fg1))))
     `(icicle-key-complete-menu                  ((,class (:foreground ,fg1))))
     `(icicle-input-completion-fail-lax          ((,class (:foreground ,keyword))))
     `(icicle-input-completion-fail              ((,class (:foreground ,keyword))))
     `(icicle-historical-candidate-other         ((,class (:foreground ,fg1))))
     `(icicle-historical-candidate               ((,class (:foreground ,fg1))))
     `(icicle-current-candidate-highlight        ((,class (:foreground ,warning :background ,bg3))))
     `(icicle-Completions-instruction-2          ((,class (:foreground ,fg4))))
     `(icicle-Completions-instruction-1          ((,class (:foreground ,fg4))))
     `(icicle-completion                         ((,class (:foreground ,var))))
     `(icicle-complete-input                     ((,class (:foreground ,builtin))))
     `(icicle-common-match-highlight-Completions ((,class (:foreground ,type))))
     `(icicle-candidate-part                     ((,class (:foreground ,var))))
     `(icicle-annotation                         ((,class (:foreground ,fg4))))
     ;; Cider mode
     ;; `(cider-debug-code-overlay-face ((,class (:inherit default))))
     ;; `(cider-debug-prompt-face ((,class (:inherit default))))
     ;; `(cider-deprecated-face ((,class (:inherit default))))
     ;; `(cider-docview-emphasis-face ((,class (:inherit default))))
     ;; `(cider-docview-literal-face ((,class (:inherit default))))
     ;; `(cider-docview-strong-face ((,class (:inherit default))))
     ;; `(cider-docview-table-border-face ((,class (:inherit default))))
     ;; `(cider-enlightened-face ((,class (:inherit default))))
     ;; `(cider-enlightened-local-face ((,class (:inherit default))))
     ;; `(cider-error-highlight-face ((,class (:inherit default))))
     ;; `(cider-fragile-button-face ((,class (:inherit default))))
     ;; `(cider-fringe-good-face ((,class (:inherit default))))
     ;; `(cider-instrumented-face ((,class (:inherit default))))
     ;; `(cider-reader-conditional-face ((,class (:inherit default))))
     ;; `(cider-repl-input-face ((,class (:inherit default))))
     ;; `(cider-repl-prompt-face ((,class (:inherit default))))
     ;; `(cider-repl-result-face ((,class (:inherit default))))
     ;; `(cider-repl-stderr-face ((,class (:inherit default))))
     ;; `(cider-repl-stdout-face ((,class (:inherit default))))
     ;; `(cider-result-overlay-face ((,class (:inherit default))))
     ;; `(cider-stacktrace-error-class-face ((,class (:inherit default))))
     ;; `(cider-stacktrace-error-message-face ((,class (:inherit default))))
     `(cider-stacktrace-face ((,class (:background ,bg2 :foreground ,fg1 :inherit default))))
     ;; `(cider-stacktrace-filter-active-face ((,class (:inherit default))))
     ;; `(cider-stacktrace-filter-inactive-face ((,class (:inherit default))))
     ;; `(cider-stacktrace-fn-face ((,class (:inherit default))))
     ;; `(cider-stacktrace-ns-face ((,class (:inherit default))))
     ;; `(cider-stacktrace-promoted-button-face ((,class (:inherit default))))
     ;; `(cider-stacktrace-suppressed-button-face ((,class (:inherit default))))
     ;; `(cider-test-error-face ((,class (:inherit default))))
     ;; `(cider-test-failure-face ((,class (:inherit default))))
     ;; `(cider-test-success-face ((,class (:inherit default))))
     ;; `(cider-traced-face ((,class (:inherit default))))
     ;; `(cider-warning-highlight-face ((,class (:inherit default))))
     )

    (custom-theme-set-variables
     'agricola
     `(ansi-color-names-vector [(,bg3 ,bg3) (,keyword ,bg3) (,type ,bg3)
                                (,var ,var) (,func ,func) (,builtin ,builtin)
                                (,str ,str) (,fg2 ,fg2)]))))
(comment
  ;;;###autoload
  (when load-file-name
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory (file-name-directory load-file-name)))))

(provide-theme 'agricola)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; agricola-theme.el ends here