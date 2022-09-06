;;; hannover-night-theme.el --- A dark theme with moderate contrast
;; Copyright (C) 2021, Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; Url: https://github.com/florommel/hannover-night-theme
;; Created: 2021-11-28
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

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

;; A dark theme with moderate contrast.
;; Provides carefully crafted faces for lots of packages.
;;
;; Colors and some other aspects of the theme can be configured
;; (See the hannover-night-theme group).
;;
;; The main colors are exported as global variables, so they can be used
;; for customizations.

;;; Code:

(deftheme hannover-night
  "A dark theme with moderate contrast.")

(defgroup hannover-night-theme nil
  "Hannover night theme customizations"
  :group 'faces)

(defcustom hannover-night-mode-line-box-width 8
  "Determines the height of the mode-line.
Set it to 0 or 1 if you use powerline or similar packages."
  :type 'natnum
  :group 'hannover-night-theme)

(defcustom hannover-night-header-line-box-width 8
  "Determines the height of the header-line"
  :type 'natnum
  :group 'hannover-night-theme)

(defcustom hannover-night-tab-line-box-width 8
  "Determines the height of the tab-line"
  :type 'natnum
  :group 'hannover-night-theme)

(defcustom hannover-night-tab-bar-box-width 8
  "Determines the height of the tab-bar"
  :type 'natnum
  :group 'hannover-night-theme)

(defcustom hannover-night-org-block-begin-end-height 0.8
  "Determines the text height of org-mode block begin and end lines"
  :type 'number
  :group 'hannover-night-theme)

(defcustom hannover-night-outline-height 1.0
  "Determines the text height of outlines (including org-mode headers)"
  :type 'number
  :group 'hannover-night-theme)

(defun hannover-night-mix (color1 color2 ratio)
  "Mix COLOR1 and COLOR2 depending on RATIO."
  (let ((color1 (color-name-to-rgb color1))
        (color2 (color-name-to-rgb color2)))
    (apply #'color-rgb-to-hex
           (list (+ (* (- 1 ratio) (nth 0 color1)) (* ratio (nth 0 color2)))
                 (+ (* (- 1 ratio) (nth 1 color1)) (* ratio (nth 1 color2)))
                 (+ (* (- 1 ratio) (nth 2 color1)) (* ratio (nth 2 color2)))))))

(defun hannover-night-shade (color ratio)
  "Shade COLOR to make it more dim depending on RATIO"
  (hannover-night-mix hannover-night-color-bg color ratio))

(cl-macrolet ((color (name) (intern (concat "hannover-night-color-" (symbol-name name))))
              (shade (color ratio) (list 'hannover-night-shade color ratio))
              (mix (color1 color2 ratio) (list 'hannover-night-mix color1 color2 ratio)))

  (cl-flet ((color-def (name value doc)
                       (custom-declare-variable
                        (intern (concat "hannover-night-color-" (symbol-name name)))
                        value
                        doc
                        :type 'color
                        :group 'hannover-night-theme)))
    (color-def 'fg                     "#d8d8d8"                                           "Main foreground color")
    (color-def 'bg                     "#292929"                                           "Main background color")
    (color-def 'blue                   "#72ace5"                                           "Blue foreground color")
    (color-def 'red                    "#eb6060"                                           "Red foreground color")
    (color-def 'orange                 "#e59e8d"                                           "Orange foreground color")
    (color-def 'green                  "#9eca72"                                           "Green foreground color")
    (color-def 'yellow                 "#e7cc99"                                           "Yellow foreground color")
    (color-def 'purple                 "#b49ce2"                                           "Purple foreground color")
    (color-def 'cyan                   "#8ff0f0"                                           "Cyan foreground color")

    (color-def '1-blue                 (shade (color blue)   0.6)                          "Dimmed blue color")
    (color-def '1-red                  (shade (color red)    0.6)                          "Dimmed red color")
    (color-def '1-orange               (shade (color orange) 0.6)                          "Dimmed orange color")
    (color-def '1-green                (shade (color green)  0.6)                          "Dimmed green color")
    (color-def '1-yellow               (shade (color yellow) 0.6)                          "Dimmed yellow color")
    (color-def '1-purple               (shade (color purple) 0.6)                          "Dimmed purple color")
    (color-def '1-cyan                 (shade (color cyan)   0.6)                          "Dimmed cyan color")

    (color-def '2-blue                 (shade (color blue)   0.1)                          "Dark blue color")
    (color-def '2-red                  (shade (color red)    0.1)                          "Dark red color")
    (color-def '2-orange               (shade (color orange) 0.1)                          "Dark orange color")
    (color-def '2-green                (shade (color green)  0.1)                          "Dark green color")
    (color-def '2-yellow               (shade (color yellow) 0.1)                          "Dark yellow color")
    (color-def '2-purple               (shade (color purple) 0.1)                          "Dark purple color")
    (color-def '2-cyan                 (shade (color cyan)   0.1)                          "Dark cyan color")

    (color-def 'grey                   (shade "#808080" 0.8)                               "Grey foreground color")
    (color-def 'dim                    (shade "#808080" 0.4)                               "Dim foreground color")
    (color-def 'fg-distant             (mix (color grey) (color fg) 0.5)                   "Distant foreground color")
    (color-def 'pale-blue              (mix (color blue) (color fg) 0.55)                  "Pale blue foreground color")

    (color-def 'fg-tooltip             "black"                                             "Tooltip foreground color")
    (color-def 'bg-tooltip             (color yellow)                                      "Tooltip background color")

    (color-def 'cursor                 (color blue)                                        "Cursor color")
    (color-def 'bg-highlight           (shade (color fg) 0.2)                              "Highlight background color")
    (color-def 'bg-mode-line           "#3d4a74"                                           "Modeline background color")
    (color-def 'bg-mode-line-inactive  (color bg-highlight)                                "Modeline inactive background color")
    (color-def 'bg-header-line         (mix (color bg-mode-line-inactive) (color bg) 0.4)  "Header line background color")
    (color-def 'fg-mode-line           (mix (color bg-mode-line) (color fg) 0.6)           "Modeline foreground color")
    (color-def 'fg-mode-line-inactive  (mix (color bg-mode-line-inactive) (color fg) 0.55) "Modeline inactive foreground color")

    (color-def 'bg-region              "#4c64ad"                                           "Region background color")
    (color-def 'fg-escape              (color yellow)                                      "Escape foreground color")
    (color-def 'fg-shadow              (color grey)                                        "Shadow color")
    (color-def 'bg-secondary-selection (mix (color 2-orange) (color 1-orange) 0.3)         "Secondary selection background color")
    (color-def 'fl-comment             (color grey)                                        "Font-lock comment color")
    (color-def 'fl-doc                 (mix (color grey) (color 1-green) 0.7)              "Font-lock documentation color")
    (color-def 'fl-keyword             (color blue)                                        "Font-lock keyword color")
    (color-def 'fl-preprocessor        (color blue)                                        "Font-lock preprocessor color")
    (color-def 'fl-string              (color green)                                       "Font-lock string color")
    (color-def 'fl-type                (color purple)                                      "Font-lock type color")
    (color-def 'fl-function            (color orange)                                      "Font-lock variable color")
    (color-def 'fl-variable            (color yellow)                                      "Font-lock function color")
    (color-def 'fl-const               (color pale-blue)                                   "Font-lock constant color")
    (color-def 'fl-neg                 (color orange)                                      "Font-lock negation color")
    (color-def 'fl-warning             (color red)                                         "Font-lock warning color")
    (color-def 'bg-warning             (color yellow)                                      "Font-lock warning color"))

  (cl-flet ((box (height color)
                 (if (and height (> height 0))
                     `(:line-width ,height :color ,color :style nil)
                   'nil)))
      (custom-theme-set-faces
       'hannover-night
       `(default ((t :foreground ,(color fg) :background ,(color bg))))
       `(cursor ((t (:background ,(color cursor)))))
       `(escape-glyph ((t (:foreground ,(color fg-escape)))))
       `(homoglyph ((t (:foreground ,(color fg-escape)))))
       `(minibuffer-prompt ((t :foreground ,(color orange) :slant italic)))
       `(highlight ((t (:underline (:color ,(color yellow) :style line) :foreground ,(color yellow)))))
       `(region ((t :extend t :background ,(color bg-region) :distant-foreground ,(color fg-distant))))
       `(shadow ((t (:foreground ,(color fg-shadow)))))
       `(secondary-selection ((t (:extend t :background ,(color bg-secondary-selection) :distant-foreground ,(color fg-distant)))))
       `(trailing-whitespace ((t (:background ,(color bg-warning)))))
       `(scroll-bar ((t :foreground ,(color 1-blue) :background ,(color bg))))
       `(pulse-highlight-start-face ((t :background ,(color 1-yellow))))
       `(pulse-highlight-face ((t :background ,(color 1-yellow))))

       `(italic ((t (:slant italic))))
       `(bold ((t (:weight bold))))
       `(bold-italic ((t (:weight bold :slant italic))))
       `(underline ((t (:underline (:color foreground-color :style line)))))

       `(font-lock-builtin-face ((t (:foreground ,(color fl-type)))))
       `(font-lock-comment-delimiter-face ((t (:foreground ,(color fl-comment)))))
       `(font-lock-comment-face ((t (:foreground ,(color fl-comment) :slant italic))))
       `(font-lock-constant-face ((t (:foreground ,(color fl-const)))))
       `(font-lock-doc-face ((t (:foreground ,(color fl-doc)))))
       `(font-lock-function-name-face ((t (:foreground ,(color fl-function)))))
       `(font-lock-keyword-face ((t (:foreground ,(color fl-keyword)))))
       `(font-lock-negation-char-face ((t (:foreground ,(color fl-neg)))))
       `(font-lock-preprocessor-face ((t (:foreground ,(color fl-preprocessor)))))
       `(font-lock-regexp-grouping-backslash ((t (:foreground ,(color fl-neg)))))
       `(font-lock-regexp-grouping-construct ((t (:foreground ,(color fl-keyword)))))
       `(font-lock-string-face ((t (:foreground ,(color fl-string)))))
       `(font-lock-type-face ((t (:foreground ,(color fl-type)))))
       `(font-lock-variable-name-face ((t (:foreground ,(color fl-variable)))))
       `(font-lock-warning-face ((t (:foreground ,(color fl-warning)))))

       `(fringe ((t (:background ,(color bg) :foreground ,(color grey)))))
       `(button ((t (:inherit (link)))))
       `(link ((t (:underline (:color foreground-color :style line) :foreground ,(color blue)))))
       `(link-visited ((t (:underline (:color foreground-color :style line) :foreground ,(color purple)))))
       `(tooltip ((t (:inherit (default) :background ,(color bg-tooltip) :foreground ,(color fg-tooltip)))))
       `(isearch ((t (:background ,(color fg) :foreground ,(color bg) :distant-foreground ,(color bg)))))
       `(isearch-fail ((t (:inverse-video t :inherit (error)))))
       `(lazy-highlight ((t (:inherit (isearch) :distant-foreground ,(color bg)))))
       `(match ((((type nil)) (:underline t :foreground ,(color yellow) :background ,(color 2-yellow))) (t (:foreground ,(color yellow) :background ,(color 2-yellow) :box (:line-width (-1 . -1) :color ,(color 1-yellow))))))
       `(next-error ((t (:inherit (region)))))
       `(query-replace ((t (:inverse-video t :background ,(color bg) :foreground ,(color red)))))
       `(hl-line ((t (:background ,(shade (color bg-highlight) 0.35)))))
       `(fill-column-indicator ((t (:foreground ,(shade (color fg) 0.1)))))
       `(nobreak-space ((t (:foreground ,(color yellow)))))
       `(error ((t (:foreground ,(color red)))))
       `(warning ((t (:foreground ,(color yellow)))))
       `(success ((t (:foreground ,(color green)))))

       `(header-line ((t (:box ,(box hannover-night-header-line-box-width (color bg-header-line)) :foreground ,(color fg) :background ,(color bg-header-line)))))
       `(mode-line ((t (:box ,(box hannover-night-mode-line-box-width (color bg-mode-line)) :foreground ,(color fg-mode-line) :background ,(color bg-mode-line)))))
       `(mode-line-inactive ((t (:box ,(box hannover-night-mode-line-box-width (color bg-mode-line-inactive)) :foreground ,(color fg-mode-line-inactive) :background ,(color bg-mode-line-inactive)))))
       `(mode-line-buffer-id ((t (:foreground ,(color fg)))))
       `(mode-line-emphasis ((t (:foreground ,(color yellow) :weight bold))))
       `(mode-line-highlight ((t (:foreground ,(color yellow)))))
       `(mode-line-special ((t (:background ,(color yellow) :foreground ,(color bg) :box ,(box hannover-night-mode-line-box-width (color yellow))))))

       ;; tab-line
       `(tab-line ((t (:inherit (mode-line-inactive) :box nil :foreground ,(color fg) :overline ,(color grey)))))
       `(tab-line-close-highlight ((t (:foreground ,(color red)))))
       `(tab-line-highlight ((t (:foreground ,(color pale-blue)))))
       `(tab-line-tab ((t (:inherit (tab-line) :box (:line-width (,(round (* 1.5 hannover-night-tab-line-box-width)) . ,hannover-night-tab-line-box-width) :color ,(color bg)) :background ,(color bg) :overline ,(color grey)))))
       `(tab-line-tab-current ((t (:inherit (tab-line-tab) :box (:line-width (,(round (* 1.5 hannover-night-tab-line-box-width)) . ,hannover-night-tab-line-box-width) :color ,(color bg)) :background ,(color bg) :overline ,(color 1-blue)))))
       `(tab-line-tab-group ((t (:inherit (tab-line-tab) :box (:line-width (,(round (* 1.5 hannover-night-tab-line-box-width)) . ,hannover-night-tab-line-box-width) :color ,(color 2-purple)) :background ,(color 2-purple) :overline ,(color 1-purple)))))
       `(tab-line-tab-inactive ((t (:inherit (tab-line-tab) :box (:line-width (,(round (* 1.5 hannover-night-tab-line-box-width)) . ,hannover-night-tab-line-box-width) :color ,(color bg-mode-line-inactive)) :background ,(color bg-mode-line-inactive)  :overline ,(color grey)))))
       `(tab-line-tab-inactive-alternate ((t (:inherit (tab-line-tab-inactive)))))
       `(tab-line-tab-modified ((t (:foreground ,(color orange)))))
       `(tab-line-tab-special ((t (:slant italic))))

       ;; tab-bar
       `(tab-bar ((t (:inherit (mode-line-inactive) :box nil :foreground ,(color fg)))))
       `(tab-bar-tab ((t (:inherit (tab-bar) :box (:line-width (,(round (* 1.5 hannover-night-tab-bar-box-width)) . ,hannover-night-tab-bar-box-width) :color ,(color bg)) :background ,(color bg) :overline ,(color 1-blue)))))
       `(tab-bar-tab-group-current ((t (:inherit (tab-bar-tab) :weight bold))))
       `(tab-bar-tab-inactive ((t (:inherit (tab-bar) :box (:line-width (,(round (* 1.5 hannover-night-tab-bar-box-width)) . ,hannover-night-tab-bar-box-width) :color ,(color bg-mode-line-inactive)) :background ,(color bg-mode-line-inactive)))))
       `(tab-bar-tab-group-inactive ((t (:inherit (tab-bar-tab-inactive) :foreground ,(color grey)))))
       `(tab-bar-tab-ungrouped ((t (:inherit (tab-bar-tab-inactive) :foreground ,(color grey)))))

       ;; powerline
       `(powerline-active0 ((t (:inherit mode-line :box nil))))
       `(powerline-active1 ((t (:inherit powerline-active0 :background ,(shade (color bg-mode-line-inactive) 0.6)))))
       `(powerline-active2 ((t (:inherit powerline-active0))))
       `(powerline-inactive0 ((t (:inherit mode-line-inactive :box nil))))
       `(powerline-inactive1 ((t (:inherit powerline-active1))))
       `(powerline-inactive2 ((t (:inherit powerline-inactive0))))

       ;; window-divider
       `(window-divider ((t (:foreground ,(color bg-mode-line-inactive)))))
       `(window-divider-first-pixel ((t (:foreground ,(color bg)))))
       `(window-divider-last-pixel ((t (:foreground ,(color bg)))))

       ;; term
       `(term-color-black ((t (:background ,(color dim) :foreground ,(color dim)))))
       `(term-color-white ((t (:background "white" :foreground "white"))))
       `(term-color-blue ((t (:background ,(color blue) :foreground ,(color blue)))))
       `(term-color-cyan ((t (:background ,(color cyan) :foreground ,(color cyan)))))
       `(term-color-green ((t (:background ,(color green) :foreground ,(color green)))))
       `(term-color-magenta ((t (:background ,(color purple) :foreground ,(color purple)))))
       `(term-color-red ((t (:background ,(color red) :foreground ,(color red)))))
       `(term-color-yellow ((t (:background ,(color yellow) :foreground ,(color yellow)))))

       ;; multiple cursors
       `(mc/cursor-face ((t (:inverse-video t :background ,(color bg) :foreground "white" :distant-foreground ,(color bg)))))

       ;; dired
       `(dired-directory  ((t (:foreground ,(color blue) :weight normal))))
       `(dired-flagged  ((t (:background ,(color 1-red)))))
       `(dired-header ((t (:foreground ,(color blue) :weight bold))))
       `(dired-ignored ((t (:foreground ,(mix (color grey) (color fg) 0.2)))))
       `(dired-mark  ((t (:background ,(color 1-red)))))
       `(dired-marked  ((t (:background ,(color 1-red)))))
       `(dired-perm-write  ((t (:inherit (dired-ignored)))))
       `(dired-set-id  ((t (:foreground ,(color red)))))
       `(dired-special  ((t (:foreground ,(color orange)))))
       `(dired-symlink  ((t (:foreground ,(color purple)))))
       `(dired-warning  ((t (:foreground ,(color red)))))

       ;; diredfl
       `(diredfl-autofile-name ((t (:foreground ,(color purple)))))
       `(diredfl-compressed-file-name ((t :foreground ,(color yellow))))
       `(diredfl-compressed-extensions ((t (:inherit diredfl-compressed-file-name))))
       `(diredfl-compressed-file-suffix ((t (:inherit diredfl-compressed-file-name))))
       `(diredfl-date-time ((t (:foreground ,(color orange)))))
       `(diredfl-deletion ((t (:inherit (diredfl-flag-mark)))))
       `(diredfl-dir-heading ((t (:inherit dired-header))))
       `(diredfl-dir-name ((t (:inherit dired-directory))))
       `(diredfl-dir-priv ((t (:inherit dired-directory))))
       `(diredfl-exec-priv ((t (:foreground ,(color yellow)))))
       `(diredfl-executable-tag ((t (:inherit dired-flagged))))
       `(diredfl-file-name ((t nil)))
       `(diredfl-file-suffix ((t (:foreground ,(color purple)))))
       `(diredfl-flag-mark ((t (:background ,(color 1-red)))))
       `(diredfl-flag-mark-line ((t (:background ,(mix (color 1-red) (color 2-red) 0.7)))))
       `(diredfl-ignored-file-name ((t (:inherit (dired-ignored)))))
       `(diredfl-link-priv ((t (:inherit diredfl-symlink))))
       `(diredfl-no-priv ((t (:foreground ,(color grey)))))
       `(diredfl-number ((t (:foreground ,(color purple)))))
       `(diredfl-other-priv ((t (:foreground ,(color cyan)))))
       `(diredfl-rare-priv ((t (:foreground ,(color cyan)))))
       `(diredfl-read-priv ((t (:foreground ,(color green)))))
       `(diredfl-symlink ((t (:foreground ,(color cyan)))))
       `(diredfl-tagged-autofile-name ((t nil)))
       `(diredfl-write-priv ((t (:foreground ,(color orange)))))
       `(diredfl-deletion-file-name ((t (:inherit (diredfl-flag-mark-line)))))

       ;; magit
       `(magit-bisect-bad ((t (:foreground ,(color red)))))
       `(magit-bisect-good ((t (:foreground ,(color green)))))
       `(magit-bisect-skip ((t (:foreground ,(color yellow)))))
       `(magit-blame-highlight ((t (:background ,(shade (color grey) 0.5) :foreground ,(color fg)))))
       `(magit-branch-local ((t (:foreground ,(color blue)))))
       `(magit-branch-remote ((t (:foreground ,(color green)))))
       `(magit-cherry-equivalent ((t (:foreground ,(color purple)))))
       `(magit-cherry-unmatched ((t (:foreground ,(color cyan)))))
       `(magit-diff-added ((t (:background ,(mix (color 1-green) (color 2-green) 0.6)))))
       `(magit-diff-added-highlight ((t (:inherit (magit-diff-added)))))
       `(magit-diff-base ((t (:background ,(mix (color 1-yellow) (color 2-yellow) 0.6)))))
       `(magit-diff-base-highlight ((t (:inherit (magit-diff-base)))))
       `(magit-diff-context ((t (:background ,(shade (color grey) 0.1) :foreground ,(shade (color grey) 1.5)))))
       `(magit-diff-context-highlight ((t (:inherit (magit-diff-context) :background ,(shade (color grey) 0.2)))))
       `(magit-diff-file-heading-highlight ((t (:inherit (magit-section-highlight)))))
       `(magit-diff-file-heading-selection ((t (:inherit (magit-diff-file-heading-highlight) :foreground ,(color orange)))))
       `(magit-diff-hunk-heading ((t (:background ,(shade (color grey) 0.5) :overline ,(color bg) :extend t))))
       `(magit-diff-hunk-heading-highlight ((t (:background ,(shade (color 1-blue) 0.7) :extend t))))
       `(magit-diff-hunk-heading-selection ((t (:inherit (magit-diff-hunk-heading-highlight) :foreground ,(color orange) :extend t))))
       `(magit-diff-lines-boundary ((t (:inherit (magit-diff-lines-heading)))))
       `(magit-diff-lines-heading ((t (:inherit (magit-diff-hunk-heading-highlight) :background ,(mix (color 1-orange) (color 2-orange) 0.4) :extend t))))
       `(magit-diff-removed ((t (:background ,(mix (color 1-red) (color 2-red) 0.7)))))
       `(magit-diff-removed-highlight ((t (:inherit (magit-diff-removed)))))
       `(magit-diffstat-added ((t (:foreground ,(color 1-green)))))
       `(magit-diffstat-removed ((t (:foreground ,(color 1-red)))))
       `(magit-dimmed ((t (:foreground ,(color grey)))))
       `(magit-hash ((t (:foreground ,(color grey)))))
       `(magit-log-author ((t (:foreground ,(color orange)))))
       `(magit-process-ng ((t (:inherit (magit-section-heading) :foreground ,(color red)))))
       `(magit-process-ok ((t (:inherit (magit-section-heading) :foreground ,(color green)))))
       `(magit-reflog-amend ((t (:foreground ,(color purple)))))
       `(magit-reflog-checkout ((t (:foreground ,(color blue)))))
       `(magit-reflog-cherry-pick ((t (:foreground ,(color green)))))
       `(magit-reflog-commit ((t (:foreground ,(color green)))))
       `(magit-reflog-merge ((t (:foreground ,(color green)))))
       `(magit-reflog-other ((t (:foreground ,(color cyan)))))
       `(magit-reflog-rebase ((t (:foreground ,(color purple)))))
       `(magit-reflog-remote ((t (:foreground ,(color cyan)))))
       `(magit-reflog-reset ((t (:foreground ,(color red)))))
       `(magit-refname ((t (:foreground ,(shade (color grey) 2)))))
       `(magit-sequence-drop ((t (:foreground ,(color red)))))
       `(magit-sequence-head ((t (:foreground ,(color pale-blue)))))
       `(magit-sequence-part ((t (:foreground ,(color yellow)))))
       `(magit-sequence-stop ((t (:foreground ,(color green)))))
       `(magit-signature-bad ((t (:foreground ,(color red) :weight bold))))
       `(magit-signature-error ((t (:foreground ,(color pale-blue)))))
       `(magit-signature-expired ((t (:foreground ,(color orange)))))
       `(magit-signature-good ((t (:foreground ,(color green)))))
       `(magit-signature-revoked ((t (:foreground ,(color purple)))))
       `(magit-signature-untrusted ((t (:foreground ,(color cyan)))))
       `(magit-tag ((t (:foreground ,(color yellow)))))
       `(magit-section-heading ((t (:foreground ,(color yellow) :weight bold :extend t))))
       `(magit-section-heading-selection ((t (:foreground ,(color orange) :extend t))))
       `(magit-section-highlight ((t (:background ,(shade (color grey) 0.2) :extend t))))

       ;; diff
       `(diff-added ((t (:background ,(color 2-green)))))
       `(diff-removed ((t (:background ,(color 2-red)))))
       `(diff-changed ((t (:background ,(color 2-yellow)))))
       `(diff-refine-added ((t (:background ,(mix (color 2-green) (color 1-green) 0.5) :foreground ,(color fg)))))
       `(diff-refine-removed ((t (:background ,(mix (color 2-red) (color 1-red) 0.5) :foreground ,(color fg)))))
       `(diff-refine-changed ((t (:background ,(mix (color 2-yellow) (color 1-yellow) 0.5) :foreground ,(color fg)))))
       `(diff-indicator-added ((t (:inherit diff-added :foreground ,(color green)))))
       `(diff-indicator-removed ((t (:inherit diff-removed :foreground ,(color orange)))))
       `(diff-indicator-changed ((t (:inherit diff-changed :foreground ,(color yellow)))))
       `(diff-header ((t ())))
       `(diff-file-header ((t (:foreground ,(color fl-string)))))
       `(diff-function ((t (:inherit diff-header :background ,(color 2-blue) :box (:line-width (1 . 1) :color ,(color 1-blue))))))
       `(diff-hunk-header ((t (:inherit diff-function :foreground ,(color blue) :weight bold))))
       `(diff-index ((t (:inherit diff-file-header))))

       ;; diff-hl
       `(diff-hl-change ((t (:foreground ,(mix (color 1-orange) (color 2-yellow) 0.3)))))
       `(diff-hl-delete ((t (:foreground ,(color 1-red)))))
       `(diff-hl-insert ((t (:foreground ,(color 1-green)))))
       `(diff-hl-dired-change ((t (:inherit diff-hl-change :foreground ,(color bg)))))
       `(diff-hl-dired-delete ((t (:inherit diff-hl-delete :foreground ,(color bg)))))
       `(diff-hl-dired-insert ((t (:inherit diff-hl-insert :foreground ,(color bg)))))
       `(diff-hl-dired-unknown ((t (:background ,(color 1-blue) :foreground ,(color bg)))))

       ;; git-gutter
       `(git-gutter:added ((t (:foreground ,(color 1-green)))))
       `(git-gutter:deleted ((t (:foreground ,(color 1-red)))))
       `(git-gutter:modified ((t (:foreground ,(mix (color 1-orange) (color 2-yellow) 0.3)))))
       `(git-gutter:separator ((t (:foreground ,(color grey)))))
       `(git-gutter:unchanged ((t (:foreground ,(color grey)))))

       ;; wgrep
       `(wgrep-delete-face ((t (:background ,(color 1-red)))))
       `(wgrep-done-face ((t (:foreground ,(color blue) :distant-foreground ,(color 2-blue)))))
       `(wgrep-face ((t (:background ,(mix (color 1-yellow) (color 2-yellow) 0.6) :foreground ,(color fg)))))
       `(wgrep-file-face ((t (:inherit (wgrep-face)))))
       `(wgrep-reject-face ((t (:foreground ,(color red) :distant-foreground ,(color 2-red) :weight bold))))

       ;; avy
       `(avy-lead-face ((t (:background ,(mix (color cyan) (color bg) 0.6) :foreground ,(color fg)))))
       `(avy-lead-face-0 ((t (:background ,(mix (color grey) (color bg) 0.5) :foreground ,(color fg)))))
       `(avy-lead-face-1 ((t (:background ,(mix (color blue) (color bg) 0.6) :foreground ,(color fg)))))
       `(avy-lead-face-2 ((t (:background ,(mix (color yellow) (color bg) 0.6) :foreground ,(color fg)))))

       ;; anzu
       `(anzu-mode-line ((t (:foreground ,(color yellow)))))
       `(anzu-mode-line-no-match ((t (:foreground ,(color red)))))
       `(anzu-match-1 ((t (:background ,(color 1-green) :foreground ,(color bg)))))
       `(anzu-match-2 ((t (:background ,(color 1-yellow) :foreground ,(color bg)))))
       `(anzu-match-3 ((t (:background ,(color 1-purple) :foreground ,(color bg)))))
       `(anzu-replace-highlight ((t (:inherit (query-replace) :strike-through t))))
       `(anzu-replace-to ((t (:foreground ,(color cyan) :box (:line-width (-1 . -1) :color ,(color 1-cyan))))))

       ;; ivy/swiper
       `(ivy-action ((t (:foreground ,(color orange)))))
       `(ivy-completions-annotations ((t (:inherit (completions-annotations)))))
       `(ivy-confirm-face ((t (:inherit (minibuffer-prompt) :foreground ,(color green)))))
       `(ivy-current-match ((t (:background ,(color bg-highlight)  :distant-foreground ,(color fg-distant) :extend t))))
       `(ivy-cursor ((t (:background ,(color fg) :foreground ,(color bg)))))
       `(ivy-grep-info ((t (:inherit (compilation-info)))))
       `(ivy-grep-line-number ((t (:inherit (compilation-line-number)))))
       `(ivy-highlight-face ((t (:inherit (highlight)))))
       `(ivy-match-required-face ((t (:inherit (minibuffer-prompt) :foreground ,(color red)))))
       `(ivy-minibuffer-match-face-1 ((t (:underline (:color ,(color grey) :style line)))))
       `(ivy-minibuffer-match-face-2 ((t (:weight bold :foreground ,(color cyan) :underline (:color ,(color cyan) :style line)))))
       `(ivy-minibuffer-match-face-3 ((t (:weight bold :foreground ,(color yellow) :underline (:color ,(color yellow) :style line)))))
       `(ivy-minibuffer-match-face-4 ((t (:weight bold :foreground ,(color orange) :underline (:color ,(color orange) :style line)))))
       `(ivy-minibuffer-match-highlight ((t (:background ,(color bg-highlight) :extend t))))
       `(ivy-modified-buffer ((t (:foreground ,(color orange)))))
       `(ivy-modified-outside-buffer ((t (:foreground ,(color red)))))
       `(ivy-org ((t (:foreground ,(color yellow)))))
       `(ivy-prompt-match ((t (:inherit (ivy-current-match)))))
       `(ivy-remote ((t (:foreground ,(color cyan)))))
       `(ivy-separator ((t (:inherit (font-lock-doc-face)))))
       `(ivy-subdir ((t (:inherit (dired-directory)))))
       `(ivy-virtual ((t (:foreground ,(color purple)))))
       `(ivy-yank-word ((t (:inherit (highlight)))))
       `(ivy-background-match-face-1 ((t (:inherit (swiper-match-face-1)))))
       `(ivy-background-match-face-2 ((t (:inherit (swiper-match-face-2)))))
       `(ivy-background-match-face-3 ((t (:inherit (swiper-match-face-3)))))
       `(ivy-background-match-face-4 ((t (:inherit (swiper-match-face-4)))))
       `(swiper-line-face ((t (:background ,(color bg-highlight) :extend t))))
       `(swiper-match-face-1 ((t (:inherit (ivy-minibuffer-match-face-1) :background ,(color bg-mode-line) :distant-foreground ,(color fg-distant) :box (:color ,(color grey) :line-width (-1 . -1))))))
       `(swiper-match-face-2 ((t (:inherit (ivy-minibuffer-match-face-2) :background ,(color bg-mode-line) :box (:color ,(color grey) :line-width (-1 . -1))))))
       `(swiper-match-face-3 ((t (:inherit (ivy-minibuffer-match-face-3) :background ,(color bg-mode-line) :box (:color ,(color grey) :line-width (-1 . -1))))))
       `(swiper-match-face-4 ((t (:inherit (ivy-minibuffer-match-face-4) :background ,(color bg-mode-line) :box (:color ,(color grey) :line-width (-1 . -1))))))
       `(swiper-background-match-face-1 ((t (:inherit (swiper-match-face-1) :background ,(color bg-highlight)))))
       `(swiper-background-match-face-2 ((t (:inherit (swiper-match-face-2) :background ,(color bg-highlight)))))
       `(swiper-background-match-face-3 ((t (:inherit (swiper-match-face-3) :background ,(color bg-highlight)))))
       `(swiper-background-match-face-4 ((t (:inherit (swiper-match-face-4) :background ,(color bg-highlight)))))

       ;; selectrum
       `(selectrum-current-candidate ((t (:background ,(color bg-highlight) :distant-foreground ,(color fg-distant)))))
       `(selectrum-primary-highlight ((t (:weight bold :foreground ,(color cyan) :underline (:color ,(color cyan) :style line)))))
       `(selectrum-prescient-current-candidate ((t (:background ,(color bg-highlight)))))
       `(selectrum-mouse-highlight ((t (:background ,(color bg-region) :foreground ,(color fg)))))
       `(selectrum-prescient-primary-highlight ((t (:weight bold :foreground ,(color cyan) :underline (:color ,(color cyan) :style line)))))

       ;; vertico
       `(vertico-current ((t (:background ,(color bg-highlight) :distant-foreground ,(color fg-distant)))))

       ;; orderless
       `(orderless-match-face-0 ((t (:weight bold :foreground ,(color cyan) :underline (:color ,(color cyan) :style line)))))
       `(orderless-match-face-1 ((t (:weight bold :foreground ,(color yellow) :underline (:color ,(color yellow) :style line)))))
       `(orderless-match-face-2 ((t (:weight bold :foreground ,(color orange) :underline (:color ,(color orange) :style line)))))
       `(orderless-match-face-3 ((t (:weight bold :foreground ,(color blue) :underline (:color ,(color blue) :style line)))))

       ;; paren showing
       `(show-paren-match ((t (:background ,(color 1-purple) :foreground ,(color fg)))))
       `(show-paren-match-expression ((t (:inherit (show-paren-match)))))
       `(show-paren-mismatch ((t (:background ,(color 1-red) :foreground ,(color fg)))))

       ;; compilation
       `(compilation-column-number ((t (:foreground ,(color purple)))))
       `(compilation-info ((t (:foreground ,(color green)))))
       `(compilation-line-number ((t (:foreground ,(color blue)))))
       `(compilation-mode-line-exit ((t (:foreground ,(color green)))))
       `(compilation-mode-line-fail ((t (:foreground ,(color red)))))
       `(compilation-mode-line-run ((t (:foreground ,(color yellow)))))
       `(compilation-warning ((t (:foreground ,(color yellow)))))
       `(compilation-error ((t (:foreground ,(color red)))))

       ;; display-line-numbers
       `(line-number ((t (:inherit (shadow)))))
       `(line-number-current-line ((t (:inherit (line-number) :foreground ,(color 1-blue)))))
       `(line-number-major-tick ((t (:inherit (line-number) :background ,(color 2-blue) :foreground ,(color yellow)))))
       `(line-number-minor-tick ((t (:inherit (line-number-major-tick) :foreground ,(color orange)))))

       ;; company
       `(company-echo-common ((t (:foreground ,(color orange)))))
       `(company-preview ((t (:background ,(color bg-highlight) :foreground ,(color fg)))))
       `(company-preview-common ((t (:background ,(color bg-highlight) :foreground ,(color blue)))))
       `(company-preview-search ((t (:background ,(color bg-highlight) :foreground ,(color yellow)))))
       `(company-tooltip-scrollbar-track ((t (:background ,(mix (color fg-tooltip) (color bg-tooltip) 0.9)))))
       `(company-tooltip-scrollbar-thumb ((t (:background ,(mix (color fg-tooltip) (color bg-tooltip) 0.6)))))
       `(company-template-field ((t (:background ,(color 1-orange) :foreground ,(color fg)))))
       `(company-tooltip ((t (:background ,(color bg-tooltip) :foreground ,(color fg-tooltip)))))
       `(company-tooltip-annotation ((t (:inherit (company-tooltip) :foreground ,(mix (color fg-tooltip) (color blue) 0.9) :slant italic))))
       `(company-tooltip-annotation-selection ((t (:inherit (company-tooltip-selection company-tooltip-annotation)))))
       `(company-tooltip-common ((t (:inherit (company-tooltip) :foreground ,(mix (color fg-tooltip) (color red) 0.6)))))
       `(company-tooltip-common-selection ((t (:inherit (company-tooltip-selection company-tooltip-common)))))
       `(company-tooltip-mouse ((t (:background ,(mix (color fg-tooltip) (color bg-tooltip) 0.95) :foreground ,(color fg-tooltip)))))
       `(company-tooltip-search ((t (:inherit (company-tooltip) :foreground ,(mix (color fg-tooltip) (color green) 0.6) :underline t))))
       `(company-tooltip-search-selection ((t (:inherit (company-tooltip-selection company-tooltip-search)))))
       `(company-tooltip-selection ((t (:background ,(mix (color fg-tooltip) (color bg-tooltip) 0.85)))))

       ;; company-posframe
       `(company-posframe-active-backend-name ((t (:background ,(color bg-mode-line-inactive) :foreground ,(color yellow)))))
       `(company-posframe-inactive-backend-name ((t (:background ,(color bg-mode-line-inactive) :foreground ,(color fg-mode-line-inactive)))))
       `(company-posframe-metadata ((t (:background ,(color bg-mode-line-inactive) :foreground ,(color fg-mode-line-inactive) :slant italic))))
       `(company-posframe-quickhelp ((t (:background ,(color 2-yellow)))))
       `(company-posframe-quickhelp-header ((t (:background ,(color 2-yellow) :foreground ,(color blue)))))

       ;; popup
       `(popup-tip-face ((t (:inherit company-tooltip :underline nil :weight normal :slant normal :box nil :overline nil :strike-through nil))))

       ;; corfu
       `(corfu-default ((t (:background ,(mix "black" (color 2-blue) 0.6)))))
       `(corfu-border ((t (:background ,(color 1-blue)))))
       `(corfu-annotation ((t (:inherit shadow :slant italic))))
       `(corfu-bar ((t (:background ,(color blue)))))
       `(corfu-current ((t (:background ,(mix (color 2-blue) (color fg) 0.05)))))
       `(corfu-deprecated ((t (:inherit shadow :strike-through t))))
       `(corfu-echo ((t (:inherit shadow))))

       ;; rainbow-delimiters
       `(rainbow-delimiters-base-error-face ((t (:foreground ,(color red)))))
       `(rainbow-delimiters-depth-1-face ((t (:foreground ,(mix (color 1-blue) (color fg) 0.35)))))
       `(rainbow-delimiters-depth-2-face ((t (:foreground ,(mix (color 1-orange) (color fg) 0.35)))))
       `(rainbow-delimiters-depth-3-face ((t (:foreground ,(mix (color 1-green) (color fg) 0.35)))))
       `(rainbow-delimiters-depth-4-face ((t (:foreground ,(mix (color 1-yellow) (color fg) 0.35)))))
       `(rainbow-delimiters-depth-5-face ((t (:foreground ,(mix (color 1-purple) (color fg) 0.35)))))
       `(rainbow-delimiters-depth-6-face ((t (:foreground ,(mix (color 1-cyan) (color fg) 0.35)))))
       `(rainbow-delimiters-depth-7-face ((t (:foreground ,(mix (color 1-blue) (color fg) 0.35)))))
       `(rainbow-delimiters-depth-8-face ((t (:foreground ,(mix (color 1-orange) (color fg) 0.35)))))
       `(rainbow-delimiters-depth-9-face ((t (:foreground ,(mix (color 1-green) (color fg) 0.35)))))

       ;; highlight
       `(hlt-property-highlight ((t (:background ,(mix (color 1-red) (color 2-red) 0.85)))))
       `(hlt-regexp-level-1 ((t (:background ,(color blue) :foreground ,(color bg)))))
       `(hlt-regexp-level-2 ((t (:background ,(color orange) :foreground ,(color bg)))))
       `(hlt-regexp-level-3 ((t (:background ,(color green) :foreground ,(color bg)))))
       `(hlt-regexp-level-4 ((t (:background ,(color yellow) :foreground ,(color bg)))))
       `(hlt-regexp-level-5 ((t (:background ,(color purple) :foreground ,(color bg)))))
       `(hlt-regexp-level-6 ((t (:background ,(color cyan) :foreground ,(color bg)))))
       `(hlt-regexp-level-7 ((t (:background ,(color red) :foreground ,(color bg)))))
       `(hlt-regexp-level-8 ((t (:background ,(mix (color grey) (color fg) 0.5) :foreground ,(color bg)))))

       ;; highlight-changes
       `(highlight-changes ((t (:underline (:color ,(color yellow) :style line)))))
       `(highlight-changes-delete ((t (:underline (:color ,(color red) :style line)))))

       ;; symbol-overlay
       `(symbol-overlay-default-face ((t (:background ,(mix (color 1-red) (color 2-red) 0.85)))))
       `(symbol-overlay-face-1 ((t (:background ,(color blue) :foreground ,(color bg)))))
       `(symbol-overlay-face-2 ((t (:background ,(color orange) :foreground ,(color bg)))))
       `(symbol-overlay-face-3 ((t (:background ,(color green) :foreground ,(color bg)))))
       `(symbol-overlay-face-4 ((t (:background ,(color yellow) :foreground ,(color bg)))))
       `(symbol-overlay-face-5 ((t (:background ,(color purple) :foreground ,(color bg)))))
       `(symbol-overlay-face-6 ((t (:background ,(color cyan) :foreground ,(color bg)))))
       `(symbol-overlay-face-7 ((t (:background ,(color red) :foreground ,(color bg)))))
       `(symbol-overlay-face-8 ((t (:background ,(mix (color grey) (color fg) 0.5) :foreground ,(color bg)))))

       ;; bookmark
       `(bookmark-face ((t (:background ,(mix (color 1-yellow) (color 2-yellow) 0.85) :extend t))))

       ;; bm
       `(bm-face ((t (:background ,(mix (color 1-purple) (color 2-purple) 0.7) :extend t))))
       `(bm-fringe-face ((t (:inherit (bm-face) :foreground ,(color purple)))))
       `(bm-persistent-face ((t (:background ,(mix (color 1-cyan) (color 2-cyan) 0.6) :extend t))))
       `(bm-fringe-persistent-face ((t (:inherit (bm-persistent-face) :foreground ,(color cyan)))))

       ;; sh
       `(sh-quoted-exec ((t (:foreground ,(color red)))))
       `(sh-heredoc ((t (:foreground ,(color cyan)))))

       ;; auctex
       `(font-latex-bold-face                 ((t (:weight bold))))
       `(font-latex-doctex-documentation-face ((t (:foreground ,(color blue)))))
       `(font-latex-doctex-preprocessor-face  ((t (:inherit font-latex-doctex-documentation-face))))
       `(font-latex-italic-face               ((t (:slant italic))))
       `(font-latex-math-face                 ((t (:foreground ,(color green)))))
       `(font-latex-script-char-face          ((t (:foreground ,(color cyan)))))
       `(font-latex-sectioning-0-face         ((t (:inherit (font-latex-sectioning-1-face) :height 1.1))))
       `(font-latex-sectioning-1-face         ((t (:inherit (font-latex-sectioning-2-face) :height 1.1))))
       `(font-latex-sectioning-2-face         ((t (:inherit (font-latex-sectioning-3-face) :height 1.1))))
       `(font-latex-sectioning-3-face         ((t (:inherit (font-latex-sectioning-4-face) :height 1.1))))
       `(font-latex-sectioning-4-face         ((t (:inherit (font-latex-sectioning-5-face) :height 1.1))))
       `(font-latex-sectioning-5-face         ((t (:inherit (variable-pitch) :foreground ,(color yellow) :weight bold :height 1.2))))
       `(font-latex-sedate-face               ((t (:foreground ,(color orange)))))
       `(font-latex-string-face               ((t (:foreground ,(color green)))))
       `(font-latex-verbatim-face             ((t (:foreground ,(color yellow)))))
       `(font-latex-warning-face              ((t (:foreground ,(color red)))))
       `(TeX-error-description-error          ((t (:inherit (error)))))
       `(TeX-error-description-tex-said       ((t (:inherit (font-lock-function-name-face)))))
       `(TeX-error-description-warning        ((t (:inherit (warning)))))

       ;; outline
       `(outline-1 ((t (:inherit outline-2 :foreground ,(mix (color blue) (color fg) 0.3)))))
       `(outline-2 ((t (:inherit outline-3 :foreground ,(mix (color purple) (color fg) 0.3)))))
       `(outline-3 ((t (:inherit outline-4 :foreground ,(mix (color yellow) (color fg) 0.3)))))
       `(outline-4 ((t (:inherit outline-5 :foreground ,(mix (color blue) (color fg) 0.3)))))
       `(outline-5 ((t (:inherit outline-6 :foreground ,(mix (color purple) (color fg) 0.3)))))
       `(outline-6 ((t (:inherit outline-7 :foreground ,(mix (color yellow) (color fg) 0.3)))))
       `(outline-7 ((t (:inherit outline-8 :foreground ,(mix (color blue) (color fg) 0.3)))))
       `(outline-8 ((t (:weight bold :height ,hannover-night-outline-height :overline ,(color bg) :foreground ,(mix (color purple) (color fg) 0.3)))))

       ;; org
       `(org-agenda-calendar-event ((t nil)))
       `(org-agenda-calendar-sexp ((t nil)))
       `(org-agenda-clocking ((t (:background ,(mix (color 1-orange) (color 2-orange) 0.5)))))
       `(org-agenda-column-dateline ((t (:background ,(mix (color bg) (color grey) 0.5)))))
       `(org-agenda-current-time ((t (:foreground ,(color yellow)))))
       `(org-agenda-date ((t (:inherit (org-agenda-structure)))))
       `(org-agenda-date-today ((t (:inherit (org-agenda-structure) :weight bold :slant italic))))
       `(org-agenda-date-weekend ((t (:inherit (org-agenda-structure) :weight bold))))
       `(org-agenda-diary ((t nil)))
       `(org-agenda-dimmed-todo-face ((t (:foreground ,(color grey)))))
       `(org-agenda-done ((t (:foreground ,(color green)))))
       `(org-agenda-filter-category ((t (:background ,(color bg-mode-line)))))
       `(org-agenda-filter-effort ((t (:background ,(color bg-mode-line)))))
       `(org-agenda-filter-regexp ((t (:background ,(color bg-mode-line)))))
       `(org-agenda-filter-tags ((t (:background ,(color bg-mode-line)))))
       `(org-agenda-restriction-lock ((t (:inherit (org-agenda-column-dateline)))))
       `(org-agenda-structure ((t (:foreground ,(color blue)))))
       `(org-archived ((t (:foreground ,(color grey)))))
       `(org-block ((t (:background ,(shade (color grey) 0.2) :extend t))))
       `(org-block-begin-line ((t (:inherit (org-block) :height ,hannover-night-org-block-begin-end-height :foreground ,(color grey) :slant italic))))
       `(org-block-end-line ((t (:inherit (org-block) :height ,hannover-night-org-block-begin-end-height :foreground ,(color grey) :slant italic))))
       `(org-checkbox ((t (:foreground ,(color fg) :background ,(shade (color grey) 0.3)))))
       `(org-checkbox-statistics-done ((t (:inherit (org-done)))))
       `(org-checkbox-statistics-todo ((t (:inherit (org-todo)))))
       `(org-clock-overlay ((t (:foreground ,(color bg) :background ,(color blue)))))
       `(org-code ((t (:background ,(shade (color grey) 0.4) :extend t))))
       `(org-column ((t (:background ,(shade (color grey) 0.65)))))
       `(org-column-title ((t (:inherit (org-column) :weight bold :underline t))))
       `(org-date ((t (:underline t :foreground ,(color cyan)))))
       `(org-date-selected ((t (:inherit (org-date) :foreground ,(color orange)))))
       `(org-default ((t (:inherit (default)))))
       `(org-document-info ((t (:foreground ,(color purple)))))
       `(org-document-info-keyword ((t (:foreground ,(color grey)))))
       `(org-document-title ((t (:weight bold :foreground ,(color yellow)))))
       `(org-drawer ((t (:weight bold :foreground ,(color blue)))))
       `(org-ellipsis ((t (:underline t :foreground ,(color yellow)))))
       `(org-footnote ((t (:underline t :foreground ,(color cyan)))))
       `(org-formula ((t (:foreground ,(color green)))))
       `(org-habit-alert-face ((t :background ,(color yellow) :foreground ,(color bg))))
       `(org-habit-alert-future-face ((t :background ,(color 1-yellow) :foreground ,(color bg))))
       `(org-habit-clear-face ((t :background ,(color blue) :foreground ,(color bg))))
       `(org-habit-clear-future-face ((t :background ,(color 1-blue) :foreground ,(color bg))))
       `(org-habit-overdue-face ((t :background ,(color red) :foreground ,(color bg))))
       `(org-habit-overdue-future-face ((t :background ,(color 1-red) :foreground ,(color bg))))
       `(org-habit-ready-face ((t :background ,(color green) :foreground ,(color bg))))
       `(org-habit-ready-future-face ((t :background ,(color 1-green) :foreground ,(color bg))))
       `(org-hide ((t (:foreground ,(color bg)))))
       `(org-indent ((t (:inherit (org-hide)))))
       `(org-inlinetask ((t (:foreground ,(color orange)))))
       `(org-latex-and-related ((t (:background ,(color 2-cyan) :foreground ,(color cyan)))))
       `(org-level-1 ((t (:inherit outline-1))))
       `(org-level-2 ((t (:inherit outline-2))))
       `(org-level-3 ((t (:inherit outline-3))))
       `(org-level-4 ((t (:inherit outline-4))))
       `(org-level-5 ((t (:inherit outline-5))))
       `(org-level-6 ((t (:inherit outline-6))))
       `(org-level-7 ((t (:inherit outline-7))))
       `(org-level-8 ((t (:inherit outline-8))))
       `(org-link ((t (:inherit (link)))))
       `(org-list-dt ((t (:weight bold))))
       `(org-macro ((t (:background ,(color 2-green) :foreground ,(color green)))))
       `(org-meta-line ((t (:slant italic :foreground ,(color grey)))))
       `(org-mode-line-clock ((t (:background ,(color bg-mode-line)))))
       `(org-mode-line-clock-overrun ((t (:inherit (org-mode-line-clock) :background ,(color 1-red) :foreground ,(color bg)))))
       `(org-priority ((t (:inherit (font-lock-keyword-face)))))
       `(org-property-value ((t nil)))
       `(org-quote ((t (:foreground ,(color purple)))))
       `(org-scheduled ((t (:foreground ,(color green)))))
       `(org-scheduled-previously ((t (:foreground ,(color orange)))))
       `(org-scheduled-today ((t (:foreground ,(color green)))))
       `(org-sexp-date ((t (:foreground ,(color cyan)))))
       `(org-special-keyword ((t (:foreground ,(color blue)))))
       `(org-table ((t (:background ,(shade (color grey) 0.2)))))
       `(org-tag ((t nil)))
       `(org-tag-group ((t (:inherit (org-tag)))))
       `(org-target ((t (:underline t))))
       `(org-time-grid ((t (:foreground ,(color yellow)))))
       `(org-upcoming-deadline ((t (:inherit (org-default)))))
       `(org-verbatim ((t (:foreground ,(color green)))))
       `(org-verse ((t (:inherit (org-block)))))
       `(org-warning ((t (:inherit (font-lock-warning-face)))))
       `(org-todo ((t (:weight normal :foreground ,(color orange) :background ,(mix (color 1-orange) (color 2-orange) 0.7)))))
       `(org-done ((t (:weight normal :foreground ,(color green) :background ,(mix (color 1-green) (color 2-green) 0.7)))))
       `(org-headline-todo ((t (:foreground ,(color orange)))))
       `(org-headline-done ((t (:foreground ,(color green)))))

       ;; hyperbole
       `(hbut-face ((t (:foreground ,(color orange)))))
       `(hbut-flash ((t (:background ,(color 1-orange)))))
       `(hbut-item-face ((t (:background ,(color 2-orange) :foreground ,(color orange)))))

       ;; devdocs
       `(devdocs-code-block ((t (:background ,(shade (color grey) 0.2) :extend t))))

       ;; markdown
       `(markdown-blockquote-face ((t (:foreground ,(color purple)))))
       `(markdown-gfm-checkbox-face ((t (:foreground ,(color fg) :background ,(shade (color grey) 0.3)))))
       `(markdown-header-face ((t (:inherit outline-1))))
       `(markdown-header-face-1 ((t (:inherit outline-1))))
       `(markdown-header-face-2 ((t (:inherit outline-2))))
       `(markdown-header-face-3 ((t (:inherit outline-3))))
       `(markdown-header-face-4 ((t (:inherit outline-4))))
       `(markdown-header-face-5 ((t (:inherit outline-5))))
       `(markdown-header-face-6 ((t (:inherit outline-6))))
       `(markdown-header-face-7 ((t (:inherit outline-7))))
       `(markdown-header-face-8 ((t (:inherit outline-8))))
       `(markdown-highlighting-face ((t (:background ,(color 1-yellow)))))
       `(markdown-language-keyword-face ((t (:inherit (markdown-markup-face)))))
       `(markdown-line-break-face ((t (:inherit (markdown-markup-face)))))

       ;; flymake
       `(flymake-error ((((supports :underline (:style wave))) (:underline (:color ,(color red) :style wave))) (t (:inherit (error)))))
       `(flymake-warning ((((supports :underline (:style wave))) (:underline (:color ,(color yellow) :style wave))) (t (:inherit (warning)))))
       `(flymake-note ((((supports :underline (:style wave))) (:underline (:color ,(color green) :style wave))) (t (:inherit (success)))))

       ;; Flyspell
       `(flyspell-duplicate ((((supports :underline (:style wave))) (:underline (:color ,(color yellow) :style wave))) (t (:inherit (warning)))))
       `(flyspell-incorrect ((((supports :underline (:style wave))) (:underline (:color ,(color red) :style wave))) (t (:inherit (error)))))

       ;; Flycheck
       `(flycheck-error ((((supports :underline (:style wave))) (:underline (:color ,(color red) :style wave))) (t (:inherit (error)))))
       `(flycheck-warning ((((supports :underline (:style wave))) (:underline (:color ,(color yellow) :style wave))) (t (:inherit (warning)))))
       `(flycheck-info ((((supports :underline (:style wave))) (:underline (:color ,(color green) :style wave))) (t (:inherit (success)))))

       ;; hexl
       `(hexl-address-region ((t (:foreground ,(shade (color blue) 0.8)))))
       `(hexl-ascii-region ((t (:foreground ,(shade (color green) 0.8)))))

       ;; imenu-list
       `(imenu-list-entry-face-0 ((t (:inherit (imenu-list-entry-face)))))
       `(imenu-list-entry-face-1 ((t (:inherit (imenu-list-entry-face)))))
       `(imenu-list-entry-face-2 ((t (:inherit (imenu-list-entry-face)))))
       `(imenu-list-entry-face-3 ((t (:inherit (imenu-list-entry-face)))))
       `(imenu-list-entry-subalist-face-0 ((t (:inherit (imenu-list-entry-face-0)))))
       `(imenu-list-entry-subalist-face-1 ((t (:inherit (imenu-list-entry-face-1)))))
       `(imenu-list-entry-subalist-face-2 ((t (:inherit (imenu-list-entry-face-2)))))
       `(imenu-list-entry-subalist-face-3 ((t (:inherit (imenu-list-entry-face-3)))))

       ;; ledger
       `(ledger-font-xact-highlight-face ((t (:inherit (ledger-occur-xact-face)))))
       `(ledger-occur-xact-face ((t (:background ,(color 2-blue)))))
       `(ledger-font-payee-name-face ((t (:foreground ,(color yellow)))))
       `(ledger-font-payee-uncleared-face ((t (:foreground ,(color orange)))))
       `(ledger-font-payee-cleared-face ((t (:foreground ,(color green)))))
       `(ledger-font-pending-face ((t (:foreground ,(color red)))))
       `(ledger-font-posting-account-face ((t ())))

       ;; neotree
       `(neo-banner-face ((t (:foreground ,(color blue) :weight bold))))
       `(neo-dir-link-face ((t (:foreground ,(color blue)))))
       `(neo-expand-btn-face ((t (:foreground ,(color purple)))))
       `(neo-file-link-face ((t nil)))
       `(neo-header-face ((t nil)))

       ;; goggles
       `(goggles-added ((t (:inherit diff-added))))
       `(goggles-changed ((t (:inherit diff-changed))))
       `(goggles-removed ((t (:inherit diff-removed))))

       ;; tree-sitter
       `(tree-sitter-hl-face:variable.special ((t (:inherit font-lock-variable-name-face))))
       `(tree-sitter-hl-face:function.call ((t (:inherit (font-lock-function-name-face)))))
       `(tree-sitter-hl-face:method.call ((t (:inherit (font-lock-function-name-face)))))
       `(tree-sitter-hl-face:property ((t (:slant italic))))
       `(tree-sitter-hl-face:punctuation ((t (:foreground ,(mix (color grey) (color fg) 0.25)))))
       `(tree-sitter-hl-face:label ((t (:inherit (font-lock-constant-face)))))
       `(tree-sitter-hl-face:string.special ((t (:inherit (tree-sitter-hl-face:string)))))
       `(tree-sitter-hl-face:embedded ((t ())))

       ;; eglot
       `(eglot-highlight-symbol-face ((t (:underline (:color ,(color cyan) :style line)))))

       ;; lsp
       `(lsp-face-highlight-textual ((t (:underline (:color ,(color yellow) :style line)))))
       `(lsp-face-highlight-read ((t (:underline (:color ,(color cyan) :style line)))))
       `(lsp-face-highlight-write ((t (:underline (:color ,(color red) :style line)))))

       `(lsp-headerline-breadcrumb-path-error-face ((t (:inherit (lsp-headerline-breadcrumb-path-face) :underline (:color ,(color red) :style wave)))))
       `(lsp-headerline-breadcrumb-path-face ((t ())))
       `(lsp-headerline-breadcrumb-path-hint-face ((t (:inherit (lsp-headerline-breadcrumb-path-face) :underline (:color ,(color green) :style wave)))))
       `(lsp-headerline-breadcrumb-path-info-face ((t (:inherit (lsp-headerline-breadcrumb-path-hint-face)))))
       `(lsp-headerline-breadcrumb-path-warning-face ((t (:inherit (lsp-headerline-breadcrumb-path-face) :underline (:color ,(color yellow) :style wave)))))
       `(lsp-headerline-breadcrumb-project-prefix-face ((t (:weight bold))))
       `(lsp-headerline-breadcrumb-symbols-error-face ((t (:inherit (lsp-headerline-breadcrumb-symbols-face) :underline (:color ,(color red) :style wave)))))
       `(lsp-headerline-breadcrumb-symbols-face ((t ())))
       `(lsp-headerline-breadcrumb-symbols-hint-face ((t (:inherit (lsp-headerline-breadcrumb-symbols-face) :underline (:color ,(color green) :style wave)))))
       `(lsp-headerline-breadcrumb-symbols-info-face ((t (:inherit (lsp-headerline-breadcrumb-symbols-hint-face)))))
       `(lsp-headerline-breadcrumb-symbols-warning-face ((t (:inherit (lsp-headerline-breadcrumb-symbols-face) :underline (:color ,(color yellow) :style wave)))))

       `(lsp-ui-doc-header ((t (:foreground ,(color bg) :background ,(color purple)))))

       `(lsp-ui-sideline-code-action ((t (:foreground ,(color yellow)))))
       `(lsp-ui-sideline-global ((t (:slant italic))))

       `(lsp-ui-peek-filename ((t (:foreground ,(color blue)))))
       `(lsp-ui-peek-footer ((t (:background ,(color bg-highlight)))))
       `(lsp-ui-peek-header ((t (:background ,(color bg-highlight) :box (:line-width 8 :color ,(color bg-highlight))))))
       `(lsp-ui-peek-highlight ((t (:background ,(color 1-blue)))))
       `(lsp-ui-peek-line-number ((t nil)))
       `(lsp-ui-peek-list ((t (:background ,(color bg-header-line)))))
       `(lsp-ui-peek-peek ((t (:background ,(color bg-header-line)))))
       `(lsp-ui-peek-selection ((t (:background ,(color grey) :distant-foreground ,(color fg-distant)))))

       ;; treemacs
       `(treemacs-all-the-icons-file-face ((t (:foreground ,(color yellow)))))
       `(treemacs-all-the-icons-root-face ((t (:foreground ,(color orange)))))
       `(treemacs-directory-face ((t (:inherit (default)))))
       `(treemacs-git-added-face ((t (:foreground ,(color green)))))
       `(treemacs-git-modified-face ((t (:foreground ,(color yellow)))))
       `(treemacs-git-renamed-face ((t (:foreground ,(color orange)))))
       `(treemacs-git-untracked-face ((t (:foreground ,(color cyan)))))
       `(treemacs-root-face ((t (:inherit (default)))))
       `(treemacs-root-remote-face ((t (:inherit (treemacs-root-face) :foreground ,(color cyan)))))
       `(treemacs-term-node-face ((t (:foreground ,(color blue)))))

       ;; yascroll
       `(yascroll:thumb-fringe ((t (:background ,(color dim) :foreground ,(color dim)))))
       `(yascroll:thumb-text-area ((t (:background ,(color dim)))))

       `(rmsbolt-current-line-face ((t (:background ,(color dim) :extend t))))

       ;; which-func
       `(which-func ((t ())))

       `(persp-selected-face ((t (:foreground ,(color yellow))))))))

(provide-theme 'hannover-night)

;;; hannover-night-theme.el ends here
