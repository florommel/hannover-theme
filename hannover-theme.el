;;; hannover-theme.el --- Dark and light themes with moderate contrast
;; Copyright (C) 2021-2022, Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; Url: https://github.com/florommel/hannover-theme  ;; TODO
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
;; (See the hannover-theme group).
;;
;; The main colors are exported as global variables, so they can be used
;; for customizations.

;;; Code:

(require 'cl-macs)

(defgroup hannover-themes nil
  "Hannover night theme customizations"
  :group 'faces)

(defcustom hannover-mode-line-box-width 8
  "Determines the height of the mode-line.
Set it to 0 or 1 if you use powerline or similar packages."
  :type 'natnum
  :group 'hannover-theme)

(defcustom hannover-header-line-box-width 8
  "Determines the height of the header-line"
  :type 'natnum
  :group 'hannover-theme)

(defcustom hannover-tab-line-box-width 8
  "Determines the height of the tab-line"
  :type 'natnum
  :group 'hannover-theme)

(defcustom hannover-tab-bar-box-width 8
  "Determines the height of the tab-bar"
  :type 'natnum
  :group 'hannover-theme)

(defcustom hannover-org-block-begin-end-height 0.8
  "Determines the text height of org-mode block begin and end lines"
  :type 'number
  :group 'hannover-theme)

(defcustom hannover-outline-height 1.0
  "Determines the text height of outlines (including org-mode headers)"
  :type 'number
  :group 'hannover-theme)

(defun hannover-color (color)
  (if (symbolp color)
      (cdr (assq color hannover-colors))
    color))

(defun hannover-light (color)
  (car (hannover-color color)))

(defun hannover-dark (color)
  (cdr (hannover-color color)))

(defun hannover-flip (color)
  (let ((c (hannover-color color)))
    (cons (cdr c) (car c))))

(defun hannover-mix (color1 color2 ratio)
  "Mix COLOR1 and COLOR2 depending on RATIO."
  (cl-flet
      ((mix1 (c1 c2 ratio)
         (let ((c1 (color-name-to-rgb c1))
               (c2 (color-name-to-rgb c2)))
           (apply #'color-rgb-to-hex
                  (list (+ (* (- 1 ratio) (nth 0 c1)) (* ratio (nth 0 c2)))
                        (+ (* (- 1 ratio) (nth 1 c1)) (* ratio (nth 1 c2)))
                        (+ (* (- 1 ratio) (nth 2 c1)) (* ratio (nth 2 c2))))))))
    (let ((c1 (hannover-color color1))
          (c2 (hannover-color color2)))
      (when (and (listp c1) (nlistp c2))
        (setq c2 (cons c2 c2)))
      (when (and (listp c2) (nlistp c1))
        (setq c1 (cons c1 c1)))
      (if (listp c1)
          (cons (mix1 (car c1) (car c2) ratio)
                (mix1 (cdr c1) (cdr c2) ratio))
        (mix1 c1 c2 ratio)))))

(defmacro hannover-alist-let* (&rest form)
  ""
  (list
   'let*
   (mapcar
    (lambda (def) (list (car def) (cadr def)))
    form)
   ;; body
   (cons 'list
         (mapcar
          (lambda (def) (list 'cons (list 'quote (car def)) (cadr def)))
          form))))

(defvar hannover-colors
  (cl-flet ((mix #'hannover-mix)
            (dark #'hannover-dark)
            (light #'hannover-light)
            (flip #'hannover-flip))
    (hannover-alist-let*
     (fg                     '("#222222" . "#d8d8d8"))
     (bg                     '("#ffffff" . "#292929"))
     (blue                   '("#0059b3" . "#72ace5"))
     (red                    '("#99002e" . "#ee7777"))
     (orange                 '("#993800" . "#e59e8d"))
     (green                  '("#4a8000" . "#9eca72"))
     (yellow                 '("#8f6b00" . "#e7cc99"))
     (purple                 '("#590094" . "#b49ce2"))
     (cyan                   '("#088691" . "#8ff0f0"))

     (high-red               (cons "#e60073" (dark red)))
     (high-yellow            (cons "#c79f00" (dark yellow)))
     (high-cyan              (cons "#09a4aa" (dark cyan)))

     (1-blue                 (mix bg blue   0.6))
     (1-red                  (mix bg red    0.6))
     (1-orange               (mix bg orange 0.6))
     (1-green                (mix bg green  0.6))
     (1-yellow               (mix bg yellow 0.6))
     (1-purple               (mix bg purple 0.6))
     (1-cyan                 (mix bg cyan   0.6))

     (2-blue                 (mix bg blue   0.1))
     (2-red                  (mix bg red    0.1))
     (2-orange               (mix bg orange 0.1))
     (2-green                (mix bg green  0.1))
     (2-yellow               (mix bg yellow 0.1))
     (2-purple               (mix bg purple 0.1))
     (2-cyan                 (mix bg cyan   0.1))

     (grey                   (mix bg "#808080" 0.8))
     (dim                    (mix bg "#808080" 0.4))
     (fg-distant             (mix grey fg 0.5))
     (pale-blue              (cons (light blue)
                                   (mix (dark blue) (dark fg) 0.55)))

     (fg-tooltip             "black")
     (bg-tooltip             (dark yellow))

     (cursor                 blue)
     (bg-highlight           (mix bg fg 0.2))
     (bg-mode-line           '("#c0d5f2" . "#3d4a74"))
     (bg-mode-line-inactive  bg-highlight)
     (bg-header-line         (mix bg-mode-line-inactive bg 0.4))
     (fg-mode-line           (mix bg-mode-line fg 0.6))
     (fg-mode-line-inactive  (mix bg-mode-line-inactive fg 0.55))

     (bg-region              '("#cce1ff" . "#4c64ad"))
     (fg-escape              yellow)
     (fg-shadow              grey)
     (bg-secondary-selection (mix 2-orange 1-orange 0.3))
     (fl-comment             grey)
     (fl-doc                 (mix grey green 0.5))
     (fl-keyword             blue)
     (fl-preprocessor        blue)
     (fl-string              green)
     (fl-type                purple)
     (fl-function            (cons (light red) (dark orange)))
     (fl-variable            yellow)
     (fl-const               pale-blue)
     (fl-neg                 red)
     (fl-warning             red)
     (bg-warning             yellow))))

(defun hannover-face (default face)
  "Build a face definition with hannover-night color." ;; TODO descibe h- extensions
  (when-let (h-branch (assq 'h (cadr face)))
    (setf (cadr face) (assq-delete-all 'h (cadr face)))
    (when (or (assoc '((background dark)) (cadr face))
              (assoc '((background light)) (cadr face))
              (assq t (cadr face)))
      (error "(background light), (background dark) and t is forbidden when using h"))
    (let* ((dark-branch (cons '((background light)) (copy-tree (cdr h-branch))))
           (light-branch (cons '((background light)) (copy-tree (cdr h-branch))))
           (has-dark-light nil))

      (cl-flet ((setcolor (key)
                  (when-let (color (plist-get (cdr h-branch) key))
                    (if (nlistp color)
                        (progn (setf (plist-get (cdr light-branch) key) color)
                               (setf (plist-get (cdr dark-branch) key) color)
                               ;; TODO? siehe unten
                               (setf (plist-get (cdr h-branch) key) color))
                      (setf (plist-get (cdr light-branch) key) (car color))
                      (setf (plist-get (cdr dark-branch) key) (cdr color))
                      ;; TODO? siehe unten
                      (setf (plist-get (cdr h-branch) key)
                            (if (eq default 'dark) (cdr color) (car color)))
                      ;; ---
                      (setq has-dark-light t)))))
        (setcolor :background)
        (setcolor :foreground)
        (setcolor :distant-foreground)
        (setcolor :overline))

      (cl-flet ((setcolor (prop-key key)
                  (let ((val (plist-get (cdr h-branch) prop-key)))
                    (when (listp val) ;; may be t
                      (when-let (val (copy-sequence val))
                        (when-let (color (plist-get val key))
                          (if (nlistp color)
                              (progn
                                (setf (plist-get val key) color)
                                (setf (plist-get (cdr light-branch) prop-key) val)
                                (setq val (copy-sequence val))
                                (setf (plist-get val key) color)
                                (setf (plist-get (cdr dark-branch) prop-key) val)
                                ;; TODO? siehe unten
                                (setq val (copy-sequence val))
                                (setf (plist-get val key) color)
                                (setf (plist-get (cdr h-branch) prop-key) val))
                            ;; TODO
                            (setf (plist-get val key)
                                  (if (eq default 'dark) (cdr color) (car color)))
                            (setf (plist-get (cdr h-branch) prop-key) val)
                            (setq val (copy-sequence val))
                            ;; ---
                            (setf (plist-get val key) (car color))
                            (setf (plist-get (cdr light-branch) prop-key) val)
                            (setq val (copy-sequence val))
                            (setf (plist-get val key) (cdr color))
                            (setf (plist-get (cdr dark-branch) prop-key) val))))))))
        (setcolor :underline :color)
        (setcolor :box :color))

      ;; TODO?
      (setf (alist-get t (cadr face))
            (cdr h-branch))
      ;; ---
      (setf (alist-get '((background dark)) (cadr face) nil nil 'equal)
            (cdr dark-branch))
      (setf (alist-get '((background light)) (cadr face) nil nil 'equal)
            (cdr light-branch))))
  face)

(defun hannover-theme-faces (variant)
  "TODO"
  (cl-flet ((faces (&rest faces-def)
              (mapcar
               (lambda (face) (hannover-face variant face))
               faces-def))
            (mix #'hannover-mix)
            (dark #'hannover-dark)
            (light #'hannover-light)
            (flip #'hannover-flip))
    (let-alist hannover-colors
      (faces
       `(default ((h :foreground ,.fg
                     :background ,(if (eq variant 'dark) (dark .bg) (light .bg)))))
       `(cursor ((h :background ,.cursor)))
       `(escape-glyph ((h :foreground ,.fg-escape)))
       `(homoglyph ((h :foreground ,.fg-escape)))
       `(minibuffer-prompt ((h :foreground ,.orange :slant italic)))
       `(highlight ((h :underline (:color ,.yellow :style line) :foreground ,.yellow)))
       `(region ((h :extend t :background ,.bg-region :distant-foreground ,.fg-distant)))
       `(shadow ((h :foreground ,.fg-shadow)))
       `(secondary-selection ((h :extend t :background ,.bg-secondary-selection :distant-foreground ,.fg-distant)))
       `(trailing-whitespace ((h :background ,.bg-warning)))
       `(scroll-bar ((h :foreground ,.grey :background ,.bg)))
       `(pulse-highlight-start-face ((h :background ,.1-yellow)))
       `(pulse-highlight-face ((h :background ,.1-yellow)))

       `(italic ((h :slant italic)))
       `(bold ((h :weight bold)))
       `(bold-italic ((h :weight bold :slant italic)))
       `(underline ((h :underline (:color foreground-color :style line))))

       `(font-lock-builtin-face ((h :foreground ,.fl-type)))
       `(font-lock-comment-delimiter-face ((h :foreground ,.fl-comment)))
       `(font-lock-comment-face ((h :foreground ,.fl-comment :slant italic)))
       `(font-lock-constant-face ((h :foreground ,.fl-const)))
       `(font-lock-doc-face ((h :foreground ,.fl-doc)))
       `(font-lock-function-name-face ((h :foreground ,.fl-function)))
       `(font-lock-keyword-face ((h :foreground ,.fl-keyword)))
       `(font-lock-negation-char-face ((h :foreground ,.fl-neg)))
       `(font-lock-preprocessor-face ((h :foreground ,.fl-preprocessor)))
       `(font-lock-regexp-grouping-backslash ((h :foreground ,.fl-neg)))
       `(font-lock-regexp-grouping-construct ((h :foreground ,.fl-keyword)))
       `(font-lock-string-face ((h :foreground ,.fl-string)))
       `(font-lock-type-face ((h :foreground ,.fl-type)))
       `(font-lock-variable-name-face ((h :foreground ,.fl-variable)))
       `(font-lock-warning-face ((h :foreground ,.fl-warning)))

       `(fringe ((h :background ,.bg :foreground ,.grey)))
       `(button ((h :inherit (link))))
       `(link ((h :underline (:color foreground-color :style line) :foreground ,.blue)))
       `(link-visited ((h :underline (:color foreground-color :style line) :foreground ,.purple)))
       `(tooltip ((h :inherit (default) :background ,.bg-tooltip :foreground ,.fg-tooltip)))
       `(isearch ((h :background ,.fg :foreground ,.bg :distant-foreground ,.bg)))
       `(isearch-fail ((h :inverse-video t :inherit (error))))
       `(lazy-highlight ((h :inherit isearch :distant-foreground ,.bg)))
       `(match ((((type nil) (background light)) :foreground ,(light .yellow) :background ,(light .2-yellow) :underline t)
                (((type nil) (background dark))  :foreground ,(dark  .yellow) :background ,(dark  .2-yellow) :underline t)
                (((background light))            :foreground ,(light .yellow) :background ,(light .2-yellow) :box (:line-width (-1 . -1) :color ,.1-yellow))
                (((background dark))             :foreground ,(dark  .yellow) :background ,(dark  .2-yellow) :box (:line-width (-1 . -1) :color ,.1-yellow))))
       `(next-error ((h :inherit (region))))
       `(query-replace ((h :inverse-video t :background ,.bg :foreground ,.red)))
       `(hl-line ((h :background ,(mix .bg .bg-highlight 0.35))))
       `(fill-column-indicator ((h :foreground ,(mix .bg .fg 0.1))))
       `(nobreak-space ((h :foreground ,.yellow)))
       `(error ((h :foreground ,.red)))
       `(warning ((h :foreground ,.yellow)))
       `(success ((h :foreground ,.green)))

       `(header-line ((h :box (:line-width ,hannover-header-line-box-width :color ,.bg-header-line :style nil) :foreground ,.fg :background ,.bg-header-line)))
       `(mode-line ((h :box (:line-width ,hannover-mode-line-box-width :color ,.bg-mode-line :style nil) :foreground ,.fg-mode-line :background ,.bg-mode-line)))
       `(mode-line-inactive ((h :box (:line-width ,hannover-mode-line-box-width :color ,.bg-mode-line-inactive :style nil) :foreground ,.fg-mode-line-inactive :background ,.bg-mode-line-inactive)))
       `(mode-line-buffer-id ((h :foreground ,.fg)))
       `(mode-line-emphasis ((h :foreground ,.yellow :distant-foreground ,(flip .yellow) :weight bold)))
       `(mode-line-highlight ((h :foreground ,.yellow)))
       `(mode-line-special ((h :background ,.yellow :foreground ,.bg :box (:line-width ,hannover-mode-line-box-width :color ,.yellow :style nil))))

       ;; tab-line
       `(tab-line ((h :inherit (mode-line-inactive) :box nil :foreground ,.fg :overline ,.grey)))
       `(tab-line-close-highlight ((h :foreground ,.red)))
       `(tab-line-highlight ((h :foreground ,.pale-blue)))
       `(tab-line-tab ((h :inherit (tab-line) :box (:line-width (,(round (* 1.5 hannover-tab-line-box-width)) . ,hannover-tab-line-box-width) :color ,.bg) :background ,.bg :overline ,.grey)))
       `(tab-line-tab-current ((h :inherit (tab-line-tab) :box (:line-width (,(round (* 1.5 hannover-tab-line-box-width)) . ,hannover-tab-line-box-width) :color ,.bg) :background ,.bg :overline ,.1-blue)))
       `(tab-line-tab-group ((h :inherit (tab-line-tab) :box (:line-width (,(round (* 1.5 hannover-tab-line-box-width)) . ,hannover-tab-line-box-width) :color ,.2-purple) :background ,.2-purple :overline ,.1-purple)))
       `(tab-line-tab-inactive ((h :inherit (tab-line-tab) :box (:line-width (,(round (* 1.5 hannover-tab-line-box-width)) . ,hannover-tab-line-box-width) :color ,.bg-mode-line-inactive) :background ,.bg-mode-line-inactive  :overline ,.grey)))
       `(tab-line-tab-inactive-alternate ((h :inherit (tab-line-tab-inactive))))
       `(tab-line-tab-modified ((h :foreground ,.orange)))
       `(tab-line-tab-special ((h :slant italic)))

       ;; tab-bar
       `(tab-bar ((h :inherit (mode-line-inactive) :box nil :foreground ,.fg)))
       `(tab-bar-tab ((h :inherit (tab-bar) :box (:line-width (,(round (* 1.5 hannover-tab-bar-box-width)) . ,hannover-tab-bar-box-width) :color ,.bg) :background ,.bg :overline ,.1-blue)))
       `(tab-bar-tab-group-current ((h :inherit (tab-bar-tab) :weight bold)))
       `(tab-bar-tab-inactive ((h :inherit (tab-bar) :box (:line-width (,(round (* 1.5 hannover-tab-bar-box-width)) . ,hannover-tab-bar-box-width) :color ,.bg-mode-line-inactive) :background ,.bg-mode-line-inactive)))
       `(tab-bar-tab-group-inactive ((h :inherit (tab-bar-tab-inactive) :foreground ,.grey)))
       `(tab-bar-tab-ungrouped ((h :inherit (tab-bar-tab-inactive) :foreground ,.grey)))

       ;; powerline
       `(powerline-active0 ((h :inherit mode-line :box nil)))
       `(powerline-active1 ((h :inherit powerline-active0 :background ,(mix .bg .bg-mode-line-inactive 0.6))))
       `(powerline-active2 ((h :inherit powerline-active0)))
       `(powerline-inactive0 ((h :inherit mode-line-inactive :box nil)))
       `(powerline-inactive1 ((h :inherit powerline-active1)))
       `(powerline-inactive2 ((h :inherit powerline-inactive0)))

       ;; window-divider
       `(window-divider ((h :foreground ,.bg-mode-line-inactive)))
       `(window-divider-first-pixel ((h :foreground ,.bg)))
       `(window-divider-last-pixel ((h :foreground ,.bg)))

       ;; term
       `(term-color-black ((h :background ,.dim :foreground ,.dim)))
       `(term-color-white ((h :background "white" :foreground "white")))
       `(term-color-blue ((h :background ,.blue :foreground ,.blue)))
       `(term-color-cyan ((h :background ,.cyan :foreground ,.cyan)))
       `(term-color-green ((h :background ,.green :foreground ,.green)))
       `(term-color-magenta ((h :background ,.purple :foreground ,.purple)))
       `(term-color-red ((h :background ,.red :foreground ,.red)))
       `(term-color-yellow ((h :background ,.yellow :foreground ,.yellow)))

       ;; eshell
       `(eshell-prompt ((h :foreground ,.blue :bold t)))
       `(eshell-ls-archive ((h :foreground ,.yellow)))
       `(eshell-ls-backup ((h :foreground ,.purple)))
       `(eshell-ls-clutter ((h :foreground ,.grey)))
       `(eshell-ls-directory ((h :foreground ,.blue)))
       `(eshell-ls-executable ((h :foreground ,.cyan)))
       `(eshell-ls-missing ((h :foreground ,.red)))
       `(eshell-ls-product ((h :foreground ,.grey)))
       `(eshell-ls-readonly ((h :foreground ,.purple)))
       `(eshell-ls-special ((h :foreground ,.orange)))
       `(eshell-ls-symlink ((h :foreground ,.pale-blue)))
       `(eshell-ls-unreadable ((h :foreground ,.grey)))

       ;; multiple cursors
       `(mc/cursor-face ((h :inverse-video t :background ,.bg :foreground ("black" . "white") :distant-foreground ,.bg)))

       ;; dired
       `(dired-directory  ((h :foreground ,.blue :weight normal)))
       `(dired-flagged  ((h :background ,.1-red)))
       `(dired-header ((h :foreground ,.blue :weight bold)))
       `(dired-ignored ((h :foreground ,(mix .grey .fg 0.2))))
       `(dired-mark  ((h :background ,.1-red)))
       `(dired-marked  ((h :background ,.1-red)))
       `(dired-perm-write  ((h :inherit (dired-ignored))))
       `(dired-set-id  ((h :foreground ,.red)))
       `(dired-special  ((h :foreground ,.orange)))
       `(dired-symlink  ((h :foreground ,.purple)))
       `(dired-warning  ((h :foreground ,.red)))

       ;; diredfl
       `(diredfl-autofile-name ((h :foreground ,.purple)))
       `(diredfl-compressed-file-name ((h :foreground ,.yellow)))
       `(diredfl-compressed-extensions ((h :inherit diredfl-compressed-file-name)))
       `(diredfl-compressed-file-suffix ((h :inherit diredfl-compressed-file-name)))
       `(diredfl-date-time ((h :foreground ,.orange)))
       `(diredfl-deletion ((h :inherit (diredfl-flag-mark))))
       `(diredfl-dir-heading ((h :inherit dired-header)))
       `(diredfl-dir-name ((h :inherit dired-directory)))
       `(diredfl-dir-priv ((h :inherit dired-directory)))
       `(diredfl-exec-priv ((h :foreground ,.yellow)))
       `(diredfl-executable-tag ((h :inherit dired-flagged)))
       `(diredfl-file-name ((h nil)))
       `(diredfl-file-suffix ((h :foreground ,.purple)))
       `(diredfl-flag-mark ((h :background ,.1-red)))
       `(diredfl-flag-mark-line ((h :background ,(mix .1-red .2-red 0.7))))
       `(diredfl-ignored-file-name ((h :inherit (dired-ignored))))
       `(diredfl-link-priv ((h :inherit diredfl-symlink)))
       `(diredfl-no-priv ((h :foreground ,.grey)))
       `(diredfl-number ((h :foreground ,.purple)))
       `(diredfl-other-priv ((h :foreground ,.cyan)))
       `(diredfl-rare-priv ((h :foreground ,.cyan)))
       `(diredfl-read-priv ((h :foreground ,.green)))
       `(diredfl-symlink ((h :foreground ,.cyan)))
       `(diredfl-tagged-autofile-name ((h nil)))
       `(diredfl-write-priv ((h :foreground ,.orange)))
       `(diredfl-deletion-file-name ((h :inherit (diredfl-flag-mark-line))))

       ;; magit
       `(magit-bisect-bad ((h :foreground ,.red)))
       `(magit-bisect-good ((h :foreground ,.green)))
       `(magit-bisect-skip ((h :foreground ,.yellow)))
       `(magit-blame-highlight ((h :background ,(mix .bg .grey 0.5) :foreground ,.fg)))
       `(magit-branch-local ((h :foreground ,.blue)))
       `(magit-branch-remote ((h :foreground ,.green)))
       `(magit-cherry-equivalent ((h :foreground ,.purple)))
       `(magit-cherry-unmatched ((h :foreground ,.cyan)))
       `(magit-diff-added ((h :background ,(mix .1-green .2-green 0.6))))
       `(magit-diff-added-highlight ((h :inherit (magit-diff-added))))
       `(magit-diff-base ((h :background ,(mix .1-yellow .2-yellow 0.6))))
       `(magit-diff-base-highlight ((h :inherit (magit-diff-base))))
       `(magit-diff-context ((h :background ,(mix .bg .grey 0.1) :foreground ,(mix .bg .grey 1.5))))
       `(magit-diff-context-highlight ((h :inherit (magit-diff-context) :background ,(mix .bg .grey 0.2))))
       `(magit-diff-file-heading-highlight ((h :inherit (magit-section-highlight))))
       `(magit-diff-file-heading-selection ((h :inherit (magit-diff-file-heading-highlight) :foreground ,.orange)))
       `(magit-diff-hunk-heading ((h :background ,(mix .bg .grey 0.5) :overline ,.bg :extend t)))
       `(magit-diff-hunk-heading-highlight ((h :background ,(mix .bg .1-blue 0.7) :extend t)))
       `(magit-diff-hunk-heading-selection ((h :inherit (magit-diff-hunk-heading-highlight) :foreground ,.orange :extend t)))
       `(magit-diff-lines-boundary ((h :inherit (magit-diff-lines-heading))))
       `(magit-diff-lines-heading ((h :inherit (magit-diff-hunk-heading-highlight) :background ,(mix .1-orange .2-orange 0.4) :extend t)))
       `(magit-diff-removed ((h :background ,(mix .1-red .2-red 0.7))))
       `(magit-diff-removed-highlight ((h :inherit (magit-diff-removed))))
       `(magit-diffstat-added ((h :foreground ,.1-green)))
       `(magit-diffstat-removed ((h :foreground ,.1-red)))
       `(magit-dimmed ((h :foreground ,.grey)))
       `(magit-hash ((h :foreground ,.grey)))
       `(magit-log-author ((h :foreground ,.orange)))
       `(magit-process-ng ((h :inherit (magit-section-heading) :foreground ,.red)))
       `(magit-process-ok ((h :inherit (magit-section-heading) :foreground ,.green)))
       `(magit-reflog-amend ((h :foreground ,.purple)))
       `(magit-reflog-checkout ((h :foreground ,.blue)))
       `(magit-reflog-cherry-pick ((h :foreground ,.green)))
       `(magit-reflog-commit ((h :foreground ,.green)))
       `(magit-reflog-merge ((h :foreground ,.green)))
       `(magit-reflog-other ((h :foreground ,.cyan)))
       `(magit-reflog-rebase ((h :foreground ,.purple)))
       `(magit-reflog-remote ((h :foreground ,.cyan)))
       `(magit-reflog-reset ((h :foreground ,.red)))
       `(magit-refname ((h :foreground ,(mix .bg .grey 2))))
       `(magit-sequence-drop ((h :foreground ,.red)))
       `(magit-sequence-head ((h :foreground ,.pale-blue)))
       `(magit-sequence-part ((h :foreground ,.yellow)))
       `(magit-sequence-stop ((h :foreground ,.green)))
       `(magit-signature-bad ((h :foreground ,.red :weight bold)))
       `(magit-signature-error ((h :foreground ,.pale-blue)))
       `(magit-signature-expired ((h :foreground ,.orange)))
       `(magit-signature-good ((h :foreground ,.green)))
       `(magit-signature-revoked ((h :foreground ,.purple)))
       `(magit-signature-untrusted ((h :foreground ,.cyan)))
       `(magit-tag ((h :foreground ,.yellow)))
       `(magit-section-heading ((h :foreground ,.yellow :weight bold :extend t)))
       `(magit-section-heading-selection ((h :foreground ,.orange :extend t)))
       `(magit-section-highlight ((h :background ,(mix .bg .grey 0.2) :extend t)))

       ;; diff
       `(diff-added ((h :background ,.2-green)))
       `(diff-removed ((h :background ,.2-red)))
       `(diff-changed ((h :background ,.2-yellow)))
       `(diff-refine-added ((h :background ,(mix .2-green .1-green 0.5) :foreground ,.fg)))
       `(diff-refine-removed ((h :background ,(mix .2-red .1-red 0.5) :foreground ,.fg)))
       `(diff-refine-changed ((h :background ,(mix .2-yellow .1-yellow 0.5) :foreground ,.fg)))
       `(diff-indicator-added ((h :inherit diff-added :foreground ,.green)))
       `(diff-indicator-removed ((h :inherit diff-removed :foreground ,.orange)))
       `(diff-indicator-changed ((h :inherit diff-changed :foreground ,.yellow)))
       `(diff-header ((h nil)))
       `(diff-file-header ((h :foreground ,.fl-string)))
       `(diff-function ((h :inherit diff-header :background ,.2-blue :box (:line-width (1 . 1) :color ,.1-blue))))
       `(diff-hunk-header ((h :inherit diff-function :foreground ,.blue :weight bold)))
       `(diff-index ((h :inherit diff-file-header)))

       ;; diff-hl
       `(diff-hl-change ((h :foreground ,(mix .blue .1-blue 0.5))))
       `(diff-hl-delete ((h :foreground ,.1-red)))
       `(diff-hl-insert ((h :foreground ,.1-green)))
       `(diff-hl-dired-change ((h :inherit diff-hl-change :foreground ,.bg)))
       `(diff-hl-dired-delete ((h :inherit diff-hl-delete :foreground ,.bg)))
       `(diff-hl-dired-insert ((h :inherit diff-hl-insert :foreground ,.bg)))
       `(diff-hl-dired-unknown ((h :background ,.1-blue :foreground ,.bg)))

       ;; git-gutter
       `(git-gutter:added ((h :foreground ,.1-green)))
       `(git-gutter:deleted ((h :foreground ,.1-red)))
       `(git-gutter:modified ((h :foreground ,(mix .blue .1-blue 0.5))))
       `(git-gutter:separator ((h :foreground ,.grey)))
       `(git-gutter:unchanged ((h :foreground ,.grey)))

       ;; wgrep
       `(wgrep-delete-face ((h :background ,.1-red)))
       `(wgrep-done-face ((h :foreground ,.blue :distant-foreground ,.2-blue)))
       `(wgrep-face ((h :background ,(mix .1-yellow .2-yellow 0.6) :foreground ,.fg)))
       `(wgrep-file-face ((h :inherit (wgrep-face))))
       `(wgrep-reject-face ((h :foreground ,.red :distant-foreground ,.2-red :weight bold)))

       ;; avy
       `(avy-lead-face ((h :background ,(mix .cyan .bg 0.6) :foreground ,.fg)))
       `(avy-lead-face-0 ((h :background ,(mix .grey .bg 0.5) :foreground ,.fg)))
       `(avy-lead-face-1 ((h :background ,(mix .blue .bg 0.6) :foreground ,.fg)))
       `(avy-lead-face-2 ((h :background ,(mix .yellow .bg 0.6) :foreground ,.fg)))

       ;; anzu
       `(anzu-mode-line ((h :foreground ,.yellow)))
       `(anzu-mode-line-no-match ((h :foreground ,.red)))
       `(anzu-match-1 ((h :background ,.1-green :foreground ,.bg)))
       `(anzu-match-2 ((h :background ,.1-yellow :foreground ,.bg)))
       `(anzu-match-3 ((h :background ,.1-purple :foreground ,.bg)))
       `(anzu-replace-highlight ((h :inherit (query-replace) :strike-through t)))
       `(anzu-replace-to ((h :foreground ,.cyan :box (:line-width (-1 . -1) :color ,.1-cyan))))

       ;; ivy/swiper
       `(ivy-action ((h :foreground ,.orange)))
       `(ivy-completions-annotations ((h :inherit (completions-annotations))))
       `(ivy-confirm-face ((h :inherit (minibuffer-prompt) :foreground ,.green)))
       `(ivy-current-match ((h :background ,.bg-highlight  :distant-foreground ,.fg-distant :extend t)))
       `(ivy-cursor ((h :background ,.fg :foreground ,.bg)))
       `(ivy-grep-info ((h :inherit (compilation-info))))
       `(ivy-grep-line-number ((h :inherit (compilation-line-number))))
       `(ivy-highlight-face ((h :inherit (highlight))))
       `(ivy-match-required-face ((h :inherit (minibuffer-prompt) :foreground ,.red)))
       `(ivy-minibuffer-match-face-1 ((h :underline (:color ,.grey :style line))))
       `(ivy-minibuffer-match-face-2 ((h :weight bold :foreground ,.cyan :underline (:color ,.cyan :style line))))
       `(ivy-minibuffer-match-face-3 ((h :weight bold :foreground ,.yellow :underline (:color ,.yellow :style line))))
       `(ivy-minibuffer-match-face-4 ((h :weight bold :foreground ,.orange :underline (:color ,.orange :style line))))
       `(ivy-minibuffer-match-highlight ((h :background ,.bg-highlight :extend t)))
       `(ivy-modified-buffer ((h :foreground ,.orange)))
       `(ivy-modified-outside-buffer ((h :foreground ,.red)))
       `(ivy-org ((h :foreground ,.yellow)))
       `(ivy-prompt-match ((h :inherit (ivy-current-match))))
       `(ivy-remote ((h :foreground ,.cyan)))
       `(ivy-separator ((h :inherit (font-lock-doc-face))))
       `(ivy-subdir ((h :inherit (dired-directory))))
       `(ivy-virtual ((h :foreground ,.purple)))
       `(ivy-yank-word ((h :inherit (highlight))))
       `(ivy-background-match-face-1 ((h :inherit (swiper-match-face-1))))
       `(ivy-background-match-face-2 ((h :inherit (swiper-match-face-2))))
       `(ivy-background-match-face-3 ((h :inherit (swiper-match-face-3))))
       `(ivy-background-match-face-4 ((h :inherit (swiper-match-face-4))))
       `(swiper-line-face ((h :background ,.bg-highlight :extend t)))
       `(swiper-match-face-1 ((h :inherit (ivy-minibuffer-match-face-1) :background ,.bg-mode-line :distant-foreground ,.fg-distant :box (:color ,.grey :line-width (-1 . -1)))))
       `(swiper-match-face-2 ((h :inherit (ivy-minibuffer-match-face-2) :background ,.bg-mode-line :box (:color ,.grey :line-width (-1 . -1)))))
       `(swiper-match-face-3 ((h :inherit (ivy-minibuffer-match-face-3) :background ,.bg-mode-line :box (:color ,.grey :line-width (-1 . -1)))))
       `(swiper-match-face-4 ((h :inherit (ivy-minibuffer-match-face-4) :background ,.bg-mode-line :box (:color ,.grey :line-width (-1 . -1)))))
       `(swiper-background-match-face-1 ((h :inherit (swiper-match-face-1) :background ,.bg-highlight)))
       `(swiper-background-match-face-2 ((h :inherit (swiper-match-face-2) :background ,.bg-highlight)))
       `(swiper-background-match-face-3 ((h :inherit (swiper-match-face-3) :background ,.bg-highlight)))
       `(swiper-background-match-face-4 ((h :inherit (swiper-match-face-4) :background ,.bg-highlight)))

       ;; selectrum
       `(selectrum-current-candidate ((h :background ,.bg-highlight :distant-foreground ,.fg-distant)))
       `(selectrum-primary-highlight ((h :weight bold :foreground ,.cyan :underline (:color ,.cyan :style line))))
       `(selectrum-prescient-current-candidate ((h :background ,.bg-highlight)))
       `(selectrum-mouse-highlight ((h :background ,.bg-region :foreground ,.fg)))
       `(selectrum-prescient-primary-highlight ((h :weight bold :foreground ,.cyan :underline (:color ,.cyan :style line))))

       ;; vertico
       `(vertico-current ((h :background ,.bg-highlight :distant-foreground ,.fg-distant)))

       ;; orderless
       `(orderless-match-face-0 ((h :weight bold :foreground ,.cyan :underline (:color ,.cyan :style line))))
       `(orderless-match-face-1 ((h :weight bold :foreground ,.yellow :underline (:color ,.yellow :style line))))
       `(orderless-match-face-2 ((h :weight bold :foreground ,.orange :underline (:color ,.orange :style line))))
       `(orderless-match-face-3 ((h :weight bold :foreground ,.blue :underline (:color ,.blue :style line))))

       ;; paren showing
       `(show-paren-match ((h :background ,.1-purple :foreground ,.fg)))
       `(show-paren-match-expression ((h :inherit (show-paren-match))))
       `(show-paren-mismatch ((h :background ,.1-red :foreground ,.fg)))

       ;; compilation
       `(compilation-column-number ((h :foreground ,.purple)))
       `(compilation-info ((h :foreground ,.green)))
       `(compilation-line-number ((h :foreground ,.blue)))
       `(compilation-mode-line-exit ((h :foreground ,.green)))
       `(compilation-mode-line-fail ((h :foreground ,.red)))
       `(compilation-mode-line-run ((h :foreground ,.yellow)))
       `(compilation-warning ((h :foreground ,.yellow)))
       `(compilation-error ((h :foreground ,.red)))

       ;; display-line-numbers
       `(line-number ((h :inherit (shadow))))
       `(line-number-current-line ((h :inherit (line-number) :foreground ,.1-blue)))
       `(line-number-major-tick ((h :inherit (line-number) :background ,.2-blue :foreground ,.yellow)))
       `(line-number-minor-tick ((h :inherit (line-number-major-tick) :foreground ,.orange)))

       ;; company
       `(company-echo-common ((h :foreground ,.orange)))
       `(company-preview ((h :background ,.bg-highlight :foreground ,.fg)))
       `(company-preview-common ((h :background ,.bg-highlight :foreground ,.blue)))
       `(company-preview-search ((h :background ,.bg-highlight :foreground ,.yellow)))
       `(company-tooltip-scrollbar-track ((h :background ,(mix .fg-tooltip .bg-tooltip 0.9))))
       `(company-tooltip-scrollbar-thumb ((h :background ,(mix .fg-tooltip .bg-tooltip 0.6))))
       `(company-template-field ((h :background ,.1-orange :foreground ,.fg)))
       `(company-tooltip ((h :background ,.bg-tooltip :foreground ,.fg-tooltip)))
       `(company-tooltip-annotation ((h :inherit (company-tooltip) :foreground ,(mix .fg-tooltip (dark .blue) 0.9) :slant italic)))
       `(company-tooltip-annotation-selection ((h :inherit (company-tooltip-selection company-tooltip-annotation))))
       `(company-tooltip-common ((h :inherit (company-tooltip) :foreground ,(mix .fg-tooltip (dark .red) 0.6))))
       `(company-tooltip-common-selection ((h :inherit (company-tooltip-selection company-tooltip-common))))
       `(company-tooltip-mouse ((h :background ,(mix .fg-tooltip .bg-tooltip 0.95) :foreground ,.fg-tooltip)))
       `(company-tooltip-search ((h :inherit (company-tooltip) :foreground ,(mix .fg-tooltip .green 0.6) :underline t)))
       `(company-tooltip-search-selection ((h :inherit (company-tooltip-selection company-tooltip-search))))
       `(company-tooltip-selection ((h :background ,(mix .fg-tooltip .bg-tooltip 0.85))))

       ;; company-posframe
       `(company-posframe-active-backend-name ((h :background ,.bg-mode-line-inactive :foreground ,.yellow)))
       `(company-posframe-inactive-backend-name ((h :background ,.bg-mode-line-inactive :foreground ,.fg-mode-line-inactive)))
       `(company-posframe-metadata ((h :background ,.bg-mode-line-inactive :foreground ,.fg-mode-line-inactive :slant italic)))
       `(company-posframe-quickhelp ((h :background ,.2-yellow)))
       `(company-posframe-quickhelp-header ((h :background ,.2-yellow :foreground ,.blue)))

       ;; popup
       `(popup-tip-face ((h :inherit company-tooltip :underline nil :weight normal :slant normal :box nil :overline nil :strike-through nil)))

       ;; corfu
       `(corfu-default ((h :background ,(mix '("white" . "black") .2-blue 0.6))))
       `(corfu-border ((h :background ,.1-blue)))
       `(corfu-annotation ((h :inherit shadow :slant italic)))
       `(corfu-bar ((h :background ,.blue)))
       `(corfu-current ((h :background ,(mix .2-blue .fg 0.05))))
       `(corfu-deprecated ((h :inherit shadow :strike-through t)))
       `(corfu-echo ((h :inherit shadow)))

       ;; rainbow-delimiters
       `(rainbow-delimiters-base-error-face ((h :foreground ,.red)))
       `(rainbow-delimiters-depth-1-face ((h :foreground ,(mix .1-blue .fg 0.35))))
       `(rainbow-delimiters-depth-2-face ((h :foreground ,(mix .1-orange .fg 0.35))))
       `(rainbow-delimiters-depth-3-face ((h :foreground ,(mix .1-green .fg 0.35))))
       `(rainbow-delimiters-depth-4-face ((h :foreground ,(mix .1-yellow .fg 0.35))))
       `(rainbow-delimiters-depth-5-face ((h :foreground ,(mix .1-purple .fg 0.35))))
       `(rainbow-delimiters-depth-6-face ((h :foreground ,(mix .1-cyan .fg 0.35))))
       `(rainbow-delimiters-depth-7-face ((h :foreground ,(mix .1-blue .fg 0.35))))
       `(rainbow-delimiters-depth-8-face ((h :foreground ,(mix .1-orange .fg 0.35))))
       `(rainbow-delimiters-depth-9-face ((h :foreground ,(mix .1-green .fg 0.35))))

       ;; highlight
       `(hlt-property-highlight ((h :background ,(mix .1-red .2-red 0.85))))
       `(hlt-regexp-level-1 ((h :background ,.blue :foreground ,.bg)))
       `(hlt-regexp-level-2 ((h :background ,.orange :foreground ,.bg)))
       `(hlt-regexp-level-3 ((h :background ,.green :foreground ,.bg)))
       `(hlt-regexp-level-4 ((h :background ,.yellow :foreground ,.bg)))
       `(hlt-regexp-level-5 ((h :background ,.purple :foreground ,.bg)))
       `(hlt-regexp-level-6 ((h :background ,.cyan :foreground ,.bg)))
       `(hlt-regexp-level-7 ((h :background ,.red :foreground ,.bg)))
       `(hlt-regexp-level-8 ((h :background ,(mix .grey .fg 0.5) :foreground ,.bg)))

       ;; highlight-changes
       `(highlight-changes ((h :underline (:color ,.yellow :style line))))
       `(highlight-changes-delete ((h :underline (:color ,.red :style line))))

       ;; symbol-overlay
       `(symbol-overlay-default-face ((h :background ,(mix .1-red .2-red 0.85))))
       `(symbol-overlay-face-1 ((h :background ,.blue :foreground ,.bg)))
       `(symbol-overlay-face-2 ((h :background ,.orange :foreground ,.bg)))
       `(symbol-overlay-face-3 ((h :background ,.green :foreground ,.bg)))
       `(symbol-overlay-face-4 ((h :background ,.yellow :foreground ,.bg)))
       `(symbol-overlay-face-5 ((h :background ,.purple :foreground ,.bg)))
       `(symbol-overlay-face-6 ((h :background ,.cyan :foreground ,.bg)))
       `(symbol-overlay-face-7 ((h :background ,.red :foreground ,.bg)))
       `(symbol-overlay-face-8 ((h :background ,(mix .grey .fg 0.5) :foreground ,.bg)))

       ;; bookmark
       `(bookmark-face ((h :background ,(mix .1-yellow .2-yellow 0.85) :extend t)))

       ;; bm
       `(bm-face ((h :background ,(mix .1-purple .2-purple 0.7) :extend t)))
       `(bm-fringe-face ((h :inherit (bm-face) :foreground ,.purple)))
       `(bm-persistent-face ((h :background ,(mix .1-cyan .2-cyan 0.6) :extend t)))
       `(bm-fringe-persistent-face ((h :inherit (bm-persistent-face) :foreground ,.cyan)))

       ;; sh
       `(sh-quoted-exec ((h :foreground ,.red)))
       `(sh-heredoc ((h :foreground ,.cyan)))

       ;; auctex
       `(font-latex-bold-face                 ((h :weight bold)))
       `(font-latex-doctex-documentation-face ((h :foreground ,.blue)))
       `(font-latex-doctex-preprocessor-face  ((h :inherit font-latex-doctex-documentation-face)))
       `(font-latex-italic-face               ((h :slant italic)))
       `(font-latex-math-face                 ((h :foreground ,.green)))
       `(font-latex-script-char-face          ((h :foreground ,.cyan)))
       `(font-latex-sectioning-0-face         ((h :inherit (font-latex-sectioning-1-face) :height 1.1)))
       `(font-latex-sectioning-1-face         ((h :inherit (font-latex-sectioning-2-face) :height 1.1)))
       `(font-latex-sectioning-2-face         ((h :inherit (font-latex-sectioning-3-face) :height 1.1)))
       `(font-latex-sectioning-3-face         ((h :inherit (font-latex-sectioning-4-face) :height 1.1)))
       `(font-latex-sectioning-4-face         ((h :inherit (font-latex-sectioning-5-face) :height 1.1)))
       `(font-latex-sectioning-5-face         ((h :inherit (variable-pitch) :foreground ,.yellow :weight bold :height 1.2)))
       `(font-latex-sedate-face               ((h :foreground ,.orange)))
       `(font-latex-string-face               ((h :foreground ,.green)))
       `(font-latex-verbatim-face             ((h :foreground ,.yellow)))
       `(font-latex-warning-face              ((h :foreground ,.red)))
       `(TeX-error-description-error          ((h :inherit (error))))
       `(TeX-error-description-tex-said       ((h :inherit (font-lock-function-name-face))))
       `(TeX-error-description-warning        ((h :inherit (warning))))

       ;; outline
       `(outline-1 ((h :inherit outline-2 :foreground ,(mix .blue .fg 0.3))))
       `(outline-2 ((h :inherit outline-3 :foreground ,(mix .purple .fg 0.3))))
       `(outline-3 ((h :inherit outline-4 :foreground ,(mix .yellow .fg 0.3))))
       `(outline-4 ((h :inherit outline-5 :foreground ,(mix .blue .fg 0.3))))
       `(outline-5 ((h :inherit outline-6 :foreground ,(mix .purple .fg 0.3))))
       `(outline-6 ((h :inherit outline-7 :foreground ,(mix .yellow .fg 0.3))))
       `(outline-7 ((h :inherit outline-8 :foreground ,(mix .blue .fg 0.3))))
       `(outline-8 ((h :weight bold :height ,hannover-outline-height :overline ,.bg :foreground ,(mix .purple .fg 0.3))))

       ;; org
       `(org-agenda-calendar-event ((h nil)))
       `(org-agenda-calendar-sexp ((h nil)))
       `(org-agenda-clocking ((h :background ,(mix .1-orange .2-orange 0.5))))
       `(org-agenda-column-dateline ((h :background ,(mix .bg .grey 0.5))))
       `(org-agenda-current-time ((h :foreground ,.yellow)))
       `(org-agenda-date ((h :inherit (org-agenda-structure))))
       `(org-agenda-date-today ((h :inherit (org-agenda-structure) :weight bold :slant italic)))
       `(org-agenda-date-weekend ((h :inherit (org-agenda-structure) :weight bold)))
       `(org-agenda-diary ((h nil)))
       `(org-agenda-dimmed-todo-face ((h :foreground ,.grey)))
       `(org-agenda-done ((h :foreground ,.green)))
       `(org-agenda-filter-category ((h :background ,.bg-mode-line)))
       `(org-agenda-filter-effort ((h :background ,.bg-mode-line)))
       `(org-agenda-filter-regexp ((h :background ,.bg-mode-line)))
       `(org-agenda-filter-tags ((h :background ,.bg-mode-line)))
       `(org-agenda-restriction-lock ((h :inherit (org-agenda-column-dateline))))
       `(org-agenda-structure ((h :foreground ,.blue)))
       `(org-archived ((h :foreground ,.grey)))
       `(org-block ((h :background ,(mix .bg .grey 0.2) :extend t)))
       `(org-block-begin-line ((h :inherit (org-block) :height ,hannover-org-block-begin-end-height :foreground ,.grey :slant italic)))
       `(org-block-end-line ((h :inherit (org-block) :height ,hannover-org-block-begin-end-height :foreground ,.grey :slant italic)))
       `(org-checkbox ((h :foreground ,.fg :background ,(mix .bg .grey 0.3))))
       `(org-checkbox-statistics-done ((h :inherit (org-done))))
       `(org-checkbox-statistics-todo ((h :inherit (org-todo))))
       `(org-clock-overlay ((h :foreground ,.bg :background ,.blue)))
       `(org-code ((h :background ,(mix .bg .grey 0.4) :extend t)))
       `(org-column ((h :background ,(mix .bg .grey 0.65))))
       `(org-column-title ((h :inherit (org-column) :weight bold :underline t)))
       `(org-date ((h :underline t :foreground ,.cyan)))
       `(org-date-selected ((h :inherit (org-date) :foreground ,.orange)))
       `(org-default ((h :inherit (default))))
       `(org-document-info ((h :foreground ,.purple)))
       `(org-document-info-keyword ((h :foreground ,.grey)))
       `(org-document-title ((h :weight bold :foreground ,.yellow)))
       `(org-drawer ((h :weight bold :foreground ,.blue)))
       `(org-ellipsis ((h :underline t :foreground ,.yellow)))
       `(org-footnote ((h :underline t :foreground ,.cyan)))
       `(org-formula ((h :foreground ,.green)))
       `(org-habit-alert-face ((h :background ,.yellow :foreground ,.bg)))
       `(org-habit-alert-future-face ((h :background ,.1-yellow :foreground ,.bg)))
       `(org-habit-clear-face ((h :background ,.blue :foreground ,.bg)))
       `(org-habit-clear-future-face ((h :background ,.1-blue :foreground ,.bg)))
       `(org-habit-overdue-face ((h :background ,.red :foreground ,.bg)))
       `(org-habit-overdue-future-face ((h :background ,.1-red :foreground ,.bg)))
       `(org-habit-ready-face ((h :background ,.green :foreground ,.bg)))
       `(org-habit-ready-future-face ((h :background ,.1-green :foreground ,.bg)))
       `(org-hide ((h :foreground ,.bg)))
       `(org-indent ((h :inherit (org-hide))))
       `(org-inlinetask ((h :foreground ,.orange)))
       `(org-latex-and-related ((h :background ,.2-cyan :foreground ,.cyan)))
       `(org-level-1 ((h :inherit outline-1)))
       `(org-level-2 ((h :inherit outline-2)))
       `(org-level-3 ((h :inherit outline-3)))
       `(org-level-4 ((h :inherit outline-4)))
       `(org-level-5 ((h :inherit outline-5)))
       `(org-level-6 ((h :inherit outline-6)))
       `(org-level-7 ((h :inherit outline-7)))
       `(org-level-8 ((h :inherit outline-8)))
       `(org-link ((h :inherit (link))))
       `(org-list-dt ((h :weight bold)))
       `(org-macro ((h :background ,.2-green :foreground ,.green)))
       `(org-meta-line ((h :slant italic :foreground ,.grey)))
       `(org-mode-line-clock ((h :background ,.bg-mode-line)))
       `(org-mode-line-clock-overrun ((h :inherit (org-mode-line-clock) :background ,.1-red :foreground ,.bg)))
       `(org-priority ((h :inherit (font-lock-keyword-face))))
       `(org-property-value ((h nil)))
       `(org-quote ((h :foreground ,.purple)))
       `(org-scheduled ((h :foreground ,.green)))
       `(org-scheduled-previously ((h :foreground ,.orange)))
       `(org-scheduled-today ((h :foreground ,.green)))
       `(org-sexp-date ((h :foreground ,.cyan)))
       `(org-special-keyword ((h :foreground ,.blue)))
       `(org-table ((h :background ,(mix .bg .grey 0.2))))
       `(org-tag ((h nil)))
       `(org-tag-group ((h :inherit (org-tag))))
       `(org-target ((h :underline t)))
       `(org-time-grid ((h :foreground ,.yellow)))
       `(org-upcoming-deadline ((h :inherit (org-default))))
       `(org-verbatim ((h :foreground ,.green)))
       `(org-verse ((h :inherit (org-block))))
       `(org-warning ((h :inherit (font-lock-warning-face))))
       `(org-todo ((h :weight normal :foreground ,.orange :background ,(mix .1-orange .2-orange 0.7))))
       `(org-done ((h :weight normal :foreground ,.green :background ,(mix .1-green .2-green 0.7))))
       `(org-headline-todo ((h :foreground ,.orange)))
       `(org-headline-done ((h :foreground ,.green)))

       ;; hyperbole
       `(hbut-face ((h :foreground ,.orange)))
       `(hbut-flash ((h :background ,.1-orange)))
       `(hbut-item-face ((h :background ,.2-orange :foreground ,.orange)))

       ;; devdocs
       `(devdocs-code-block ((h :background ,(mix .bg .grey 0.2) :extend t)))

       ;; markdown
       `(markdown-blockquote-face ((h :foreground ,.purple)))
       `(markdown-gfm-checkbox-face ((h :foreground ,.fg :background ,(mix .bg .grey 0.3))))
       `(markdown-header-face ((h :inherit outline-1)))
       `(markdown-header-face-1 ((h :inherit outline-1)))
       `(markdown-header-face-2 ((h :inherit outline-2)))
       `(markdown-header-face-3 ((h :inherit outline-3)))
       `(markdown-header-face-4 ((h :inherit outline-4)))
       `(markdown-header-face-5 ((h :inherit outline-5)))
       `(markdown-header-face-6 ((h :inherit outline-6)))
       `(markdown-header-face-7 ((h :inherit outline-7)))
       `(markdown-header-face-8 ((h :inherit outline-8)))
       `(markdown-highlighting-face ((h :background ,.1-yellow)))
       `(markdown-language-keyword-face ((h :inherit (markdown-markup-face))))
       `(markdown-line-break-face ((h :inherit (markdown-markup-face))))

       ;; ;; flymake
       `(flymake-error   ((((background light) (supports :underline (:style wave))) (:underline (:color ,(light .high-red) :style wave)))
                          (((background dark)  (supports :underline (:style wave))) (:underline (:color ,(dark  .high-red) :style wave)))
                          (t :inherit (error))))
       `(flymake-warning ((((background light) (supports :underline (:style wave))) (:underline (:color ,(light .high-yellow) :style wave)))
                          (((background dark)  (supports :underline (:style wave))) (:underline (:color ,(dark  .high-yellow) :style wave)))
                          (t :inherit (warning))))
       `(flymake-note    ((((background light) (supports :underline (:style wave))) (:underline (:color ,(light .green) :style wave)))
                          (((background dark)  (supports :underline (:style wave))) (:underline (:color ,(dark  .green) :style wave)))
                          (t :inherit (success))))

       ;; ;; Flyspell
       `(flyspell-duplicate ((((background light) (supports :underline (:style wave))) (:underline (:color ,(light .high-yellow) :style wave)))
                             (((background dark)  (supports :underline (:style wave))) (:underline (:color ,(dark  .high-yellow) :style wave)))
                             (t :inherit (warning))))
       `(flyspell-incorrect ((((background light) (supports :underline (:style wave))) (:underline (:color ,(light .high-red) :style wave)))
                             (((background dark)  (supports :underline (:style wave))) (:underline (:color ,(dark  .high-red) :style wave)))
                             (t :inherit (error))))

       ;; ;; Flycheck
       `(flycheck-error   ((((background light) (supports :underline (:style wave))) (:underline (:color ,(light .high-red) :style wave)))
                           (((background dark)  (supports :underline (:style wave))) (:underline (:color ,(dark  .high-red) :style wave)))
                           (t :inherit (error))))
       `(flycheck-warning ((((background light) (supports :underline (:style wave))) (:underline (:color ,(light .high-yellow) :style wave)))
                           (((background dark)  (supports :underline (:style wave))) (:underline (:color ,(dark  .high-yellow) :style wave)))
                           (t :inherit (warning))))
       `(flycheck-info    ((((background light) (supports :underline (:style wave))) (:underline (:color ,(light .green) :style wave)))
                           (((background dark)  (supports :underline (:style wave))) (:underline (:color ,(dark  .green) :style wave)))
                           (t :inherit (success))))

       ;; hexl
       `(hexl-address-region ((h :foreground ,(mix .bg .blue 0.8))))
       `(hexl-ascii-region ((h :foreground ,(mix .bg .green 0.8))))

       ;; imenu-list
       `(imenu-list-entry-face-0 ((h :inherit (imenu-list-entry-face))))
       `(imenu-list-entry-face-1 ((h :inherit (imenu-list-entry-face))))
       `(imenu-list-entry-face-2 ((h :inherit (imenu-list-entry-face))))
       `(imenu-list-entry-face-3 ((h :inherit (imenu-list-entry-face))))
       `(imenu-list-entry-subalist-face-0 ((h :inherit (imenu-list-entry-face-0))))
       `(imenu-list-entry-subalist-face-1 ((h :inherit (imenu-list-entry-face-1))))
       `(imenu-list-entry-subalist-face-2 ((h :inherit (imenu-list-entry-face-2))))
       `(imenu-list-entry-subalist-face-3 ((h :inherit (imenu-list-entry-face-3))))

       ;; ledger
       `(ledger-font-xact-highlight-face ((h :inherit (ledger-occur-xact-face))))
       `(ledger-occur-xact-face ((h :background ,.2-blue)))
       `(ledger-font-payee-name-face ((h :foreground ,.yellow)))
       `(ledger-font-payee-uncleared-face ((h :foreground ,.orange)))
       `(ledger-font-payee-cleared-face ((h :foreground ,.green)))
       `(ledger-font-pending-face ((h :foreground ,.red)))
       `(ledger-font-posting-account-face ((h nil)))

       ;; neotree
       `(neo-banner-face ((h :foreground ,.blue :weight bold)))
       `(neo-dir-link-face ((h :foreground ,.blue)))
       `(neo-expand-btn-face ((h :foreground ,.purple)))
       `(neo-file-link-face ((h nil)))
       `(neo-header-face ((h nil)))

       ;; goggles
       `(goggles-added ((h :inherit diff-added)))
       `(goggles-changed ((h :inherit diff-changed)))
       `(goggles-removed ((h :inherit diff-removed)))

       ;; tree-sitter
       `(tree-sitter-hl-face:variable.special ((h :inherit font-lock-variable-name-face)))
       `(tree-sitter-hl-face:function.call ((h :inherit (font-lock-function-name-face))))
       `(tree-sitter-hl-face:method.call ((h :inherit (font-lock-function-name-face))))
       `(tree-sitter-hl-face:property ((h :slant italic)))
       `(tree-sitter-hl-face:punctuation ((h :foreground ,(mix .grey .fg 0.25))))
       `(tree-sitter-hl-face:label ((h :inherit (font-lock-constant-face))))
       `(tree-sitter-hl-face:string.special ((h :inherit (tree-sitter-hl-face:string))))
       `(tree-sitter-hl-face:embedded ((h nil)))

       ;; eglot
       `(eglot-highlight-symbol-face ((h :underline (:color ,.high-cyan :style line))))

       ;; lsp
       `(lsp-face-highlight-textual ((h :underline (:color ,.high-yellow :style line))))
       `(lsp-face-highlight-read ((h :underline (:color ,.high-cyan :style line))))
       `(lsp-face-highlight-write ((h :underline (:color ,.high-red :style line))))

       `(lsp-headerline-breadcrumb-path-error-face ((h :inherit (lsp-headerline-breadcrumb-path-face) :underline (:color ,.red :style wave))))
       `(lsp-headerline-breadcrumb-path-face ((h nil)))
       `(lsp-headerline-breadcrumb-path-hint-face ((h :inherit (lsp-headerline-breadcrumb-path-face) :underline (:color ,.green :style wave))))
       `(lsp-headerline-breadcrumb-path-info-face ((h :inherit (lsp-headerline-breadcrumb-path-hint-face))))
       `(lsp-headerline-breadcrumb-path-warning-face ((h :inherit (lsp-headerline-breadcrumb-path-face) :underline (:color ,.yellow :style wave))))
       `(lsp-headerline-breadcrumb-project-prefix-face ((h :weight bold)))
       `(lsp-headerline-breadcrumb-symbols-error-face ((h :inherit (lsp-headerline-breadcrumb-symbols-face) :underline (:color ,.red :style wave))))
       `(lsp-headerline-breadcrumb-symbols-face ((h nil)))
       `(lsp-headerline-breadcrumb-symbols-hint-face ((h :inherit (lsp-headerline-breadcrumb-symbols-face) :underline (:color ,.green :style wave))))
       `(lsp-headerline-breadcrumb-symbols-info-face ((h :inherit (lsp-headerline-breadcrumb-symbols-hint-face))))
       `(lsp-headerline-breadcrumb-symbols-warning-face ((h :inherit (lsp-headerline-breadcrumb-symbols-face) :underline (:color ,.yellow :style wave))))

       `(lsp-ui-doc-header ((h :foreground ,.bg :background ,.purple)))

       `(lsp-ui-sideline-code-action ((h :foreground ,.yellow)))
       `(lsp-ui-sideline-global ((h :slant italic)))

       `(lsp-ui-peek-filename ((h :foreground ,.blue)))
       `(lsp-ui-peek-footer ((h :background ,.bg-highlight)))
       `(lsp-ui-peek-header ((h :background ,.bg-highlight :box (:line-width 8 :color ,.bg-highlight))))
       `(lsp-ui-peek-highlight ((h :background ,.1-blue)))
       `(lsp-ui-peek-line-number ((h nil)))
       `(lsp-ui-peek-list ((h :background ,.bg-header-line)))
       `(lsp-ui-peek-peek ((h :background ,.bg-header-line)))
       `(lsp-ui-peek-selection ((h :background ,.grey :distant-foreground ,.fg-distant)))

       ;; treemacs
       `(treemacs-all-the-icons-file-face ((h :foreground ,.yellow)))
       `(treemacs-all-the-icons-root-face ((h :foreground ,.orange)))
       `(treemacs-directory-face ((h :inherit (default))))
       `(treemacs-git-added-face ((h :foreground ,.green)))
       `(treemacs-git-modified-face ((h :foreground ,.yellow)))
       `(treemacs-git-renamed-face ((h :foreground ,.orange)))
       `(treemacs-git-untracked-face ((h :foreground ,.cyan)))
       `(treemacs-root-face ((h :inherit (default))))
       `(treemacs-root-remote-face ((h :inherit (treemacs-root-face) :foreground ,.cyan)))
       `(treemacs-term-node-face ((h :foreground ,.blue)))

       ;; yascroll
       `(yascroll:thumb-fringe ((h :background ,.dim :foreground ,.dim)))
       `(yascroll:thumb-text-area ((h :background ,.dim)))

       `(rmsbolt-current-line-face ((h :background ,.dim :extend t)))

       ;; which-func
       `(which-func ((h nil)))

       `(persp-selected-face ((h :foreground ,.yellow)))))))

(provide 'hannover-theme)

;;; hannover-theme.el ends here
