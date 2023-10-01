;;; hannover-day-theme.el --- Dark and light themes with moderate contrast
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

;; A light theme with moderate contrast.

;;; Code:

(deftheme hannover-adaptive
  "An adaptive theme with moderate contrast.
The theme appears as light or dark depending on the background.
Set the frame background mode to 'dark or nil and reload the theme
to see the colors adapting to the background mode.
E.g.:
(progn
  (setq frame-background-mode 'dark)
  (load-theme 'hannover-adaptive t))"
  :family 'hannover
  :kind 'color-scheme)

(require 'hannover-theme)

(defvar hannover-adaptive-colors (hannover-default-colors))

(apply 'custom-theme-set-faces
       'hannover-adaptive
       (hannover-theme-faces hannover-adaptive-colors))

(provide-theme 'hannover-adaptive)

;;; hannover-adaptive-theme.el ends here
