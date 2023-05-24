;;; base-line.el --- A minimal mode-line forked from mood-line -*- lexical-binding: t; -*-

;; Author: M Cooper Healy <m.cooper.healy@gmail.com>
;; Homepage: https://github.com/noonels/base-line
;; Keywords: base-line faces
;; Version: 0.9
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; base-line is a minimal mode-line configuration that aims to replicate
;; some of the features of the doom-modeline package.
;;
;; This was initially based on jessiehildebrandt's wonderful mood-line package, but with some (mostly aesthetic) tweaks.
;;
;; Features offered:
;; * Clean, minimal design
;; * Anzu and multiple-cursors counter
;; * Version control status indicator
;; * Flycheck status indicator
;; * Flymake support
;; * Lightweight with no dependencies
;;
;; To enable base-line:
;; (base-line-mode)

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

;;
;; Variable declarations
;;

(defvar flycheck-current-errors)
(defvar flymake--mode-line-format)
(defvar anzu--state)
(defvar anzu--cached-count)
(defvar anzu--overflow-p)
(defvar anzu--current-position)
(defvar anzu--total-matched)
(defvar multiple-cursors-mode)

;;
;; Function prototypes
;;

(declare-function flycheck-count-errors "flycheck" (errors))
(declare-function mc/num-cursors "multiple-cursors" ())

;;
;; Config
;;

(defgroup base-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom base-line-show-eol-style nil
  "If t, the EOL style of the current buffer will be displayed in the mode-line."
  :group 'base-line
  :type 'boolean)

(defcustom base-line-show-encoding-information nil
  "If t, the encoding format of the current buffer will be displayed in the mode-line."
  :group 'base-line
  :type 'boolean)

(defcustom base-line-show-cursor-point nil
  "If t, the value of `point' will be displayed next to the cursor position in the mode-line."
  :group 'base-line
  :type 'boolean)

(defface base-line-buffer-name
  '((t (:inherit (mode-line-buffer-id))))
  "Face used for major mode indicator in the mode-line."
  :group 'base-line)

(defface base-line-major-mode
  '((t (:inherit (bold))))
  "Face used for major mode indicator in the mode-line."
  :group 'base-line)

(defface base-line-status-neutral
  '((t (:inherit (shadow))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'base-line)

(defface base-line-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'base-line)

(defface base-line-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line."
  :group 'base-line)

(defface base-line-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line."
  :group 'base-line)

(defface base-line-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line."
  :group 'base-line)

(defface base-line-unimportant
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements."
  :group 'base-line)

(defface base-line-modified
  '((t (:inherit (error))))
  "Face used for the `modified' indicator symbol in the mode-line."
  :group 'base-line)

;;
;; Helper functions
;;

(defun base-line--string-trim-left (string)
  "Remove whitespace at the beginning of STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun base-line--string-trim-right (string)
  "Remove whitespace at the end of STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defun base-line--string-trim (string)
  "Remove whitespace at the beginning and end of STRING."
  (base-line--string-trim-left (base-line--string-trim-right string)))

(defun base-line--format (left right)
  "Return a string of `window-width'  of the form `LEFT <witespace> RIGHT'."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display `((space :align-to (- right ,reserve))))
            right)))

;;
;; Update functions
;;

(defvar-local base-line--vc-text nil)
(defun base-line--update-vc-segment (&rest _)
  "Update `base-line--vc-text' against the current VCS state."
  (setq base-line--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'mode-line-neutral))
              (concat (cond ((memq state '(edited added))
                             (setq face 'base-line-status-info)
                             (propertize "+ " 'face face))
                            ((eq state 'needs-merge)
                             (setq face 'base-line-status-warning)
                             (propertize "⟷ " 'face face))
                            ((eq state 'needs-update)
                             (setq face 'base-line-status-warning)
                             (propertize "↑ " 'face face))
                            ((memq state '(removed conflict unregistered))
                             (setq face 'base-line-status-error)
                             (propertize "✖ " 'face face))
                            (t
                             (setq face 'base-line-status-neutral)
                             (propertize "✔ " 'face face)))
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face face
                                  'mouse-face face)
                      "  "))))))

(defvar-local base-line--flycheck-text nil)
(defun base-line--update-flycheck-segment (&optional status)
  "Update `base-line--flycheck-text' against the reported flycheck STATUS."
  (setq base-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "⚑ Issues: "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'base-line-status-error
                                                 'base-line-status-warning))))
                       (propertize "✔ Good  " 'face 'base-line-status-success)))
          ('running (propertize "Δ Checking  " 'face 'base-line-status-info))
          ('errored (propertize "✖ Error  " 'face 'base-line-status-error))
          ('interrupted (propertize "⏸ Paused  " 'face 'base-line-status-neutral))
          ('no-checker ""))))

;;
;; Segments
;;

(defun base-line-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (if (buffer-modified-p)
          (propertize "● " 'face 'base-line-modified)
        (if (and buffer-read-only (buffer-file-name))
            (propertize "𝛌 " 'face 'base-line-modified)
          "  "))
    "  "))

(defun base-line-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (propertize "%b " 'face 'base-line-buffer-name))

(defun base-line-segment-anzu ()
  "Displays color-coded anzu status information in the mode-line (if available)."
  (when (and (boundp 'anzu--state) anzu--state)
    (cond ((eq anzu--state 'replace-query)
           (format #("Replace: %d  " 0 11 (face base-line-status-warning)) anzu--cached-count))
          (anzu--overflow-p
           (format #("%d/%d+  " 0 3 (face base-line-status-info) 3 6 (face base-line-status-error)) anzu--current-position anzu--total-matched))
          (t
           (format #("%d/%d  " 0 5 (face base-line-status-info)) anzu--current-position anzu--total-matched)))))

(defun base-line-segment-multiple-cursors ()
  "Displays the number of active multiple-cursors in the mode-line (if available)."
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    (concat "MC"
            (format #("×%d  " 0 3 (face base-line-status-warning)) (mc/num-cursors)))))

(defun base-line-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat "%l:%c"
          (when base-line-show-cursor-point (propertize (format ":%d" (point)) 'face 'base-line-unimportant))
          (propertize " %p%%  " 'face 'base-line-unimportant)))

(defun base-line-segment-eol ()
  "Displays the EOL style of the current buffer in the mode-line."
  (when base-line-show-eol-style
    (pcase (coding-system-eol-type buffer-file-coding-system)
      (0 "LF  ")
      (1 "CRLF  ")
      (2 "CR  "))))

(defun base-line-segment-encoding ()
  "Displays the encoding and EOL style of the buffer in the mode-line."
  (when base-line-show-encoding-information
    (concat (let ((sys (coding-system-plist buffer-file-coding-system)))
              (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                     "UTF-8")
                    (t (upcase (symbol-name (plist-get sys :name))))))
            "  ")))

(defun base-line-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  base-line--vc-text)

(defun base-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (concat "//  " (format-mode-line mode-name 'base-line-major-mode) "  //  "))

(defun base-line-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (format-mode-line mode-line-misc-info 'base-line-unimportant)))
    (unless (string= (base-line--string-trim misc-info) "")
      (concat (base-line--string-trim misc-info) "  "))))

(defun base-line-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  base-line--flycheck-text)

(defun base-line-segment-flymake ()
  "Displays information about the current status of flymake in the mode-line."
  (when (and (boundp 'flymake-mode) flymake-mode)
    (concat (base-line--string-trim (format-mode-line flymake--mode-line-format)) "  ")))

(defun base-line-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (let ((process-info (format-mode-line mode-line-process)))
    (unless (string= (base-line--string-trim process-info) "")
      (concat (base-line--string-trim process-info) "  "))))

;;
;; Activation function
;;

;; Store the default mode-line format
(defvar base-line--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode base-line-mode
  "Toggle base-line on or off."
  :group 'base-line
  :global t
  :lighter nil
  (if base-line-mode
      (progn

        ;; Setup flycheck hooks
        (add-hook 'flycheck-status-changed-functions #'base-line--update-flycheck-segment)
        (add-hook 'flycheck-mode-hook #'base-line--update-flycheck-segment)

        ;; Setup VC hooks
        (add-hook 'find-file-hook #'base-line--update-vc-segment)
        (add-hook 'after-save-hook #'base-line--update-vc-segment)
        (advice-add #'vc-refresh-state :after #'base-line--update-vc-segment)

        ;; Set the new mode-line-format
        (setq-default mode-line-format
                      '((:eval
                         (base-line--format
                          ;; Left
                          (format-mode-line
                           '(" "
                             (:eval (base-line-segment-modified))
                             "{ "
                             (:eval (base-line-segment-buffer-name))
                             ":: "
                             (:eval (base-line-segment-anzu))
                             (:eval (base-line-segment-multiple-cursors))
                             (:eval (base-line-segment-position))
                             "}"))

                          ;; Right
                          (format-mode-line
                           '((:eval (base-line-segment-eol))
                             (:eval (base-line-segment-encoding))
                             (:eval (base-line-segment-vc))
                             (:eval (base-line-segment-major-mode))
                             (:eval (base-line-segment-misc-info))
                             (:eval (base-line-segment-flycheck))
                             (:eval (base-line-segment-flymake))
                             (:eval (base-line-segment-process))
                             " ")))))))
    (progn

      ;; Remove flycheck hooks
      (remove-hook 'flycheck-status-changed-functions #'base-line--update-flycheck-segment)
      (remove-hook 'flycheck-mode-hook #'base-line--update-flycheck-segment)

      ;; Remove VC hooks
      (remove-hook 'file-find-hook #'base-line--update-vc-segment)
      (remove-hook 'after-save-hook #'base-line--update-vc-segment)
      (advice-remove #'vc-refresh-state #'base-line--update-vc-segment)

      ;; Restore the original mode-line format
      (setq-default mode-line-format base-line--default-mode-line))))

;;
;; Provide base-line
;;

(provide 'base-line)

;;; base-line.el ends here
