;;; procress.el --- Process progress  -*- lexical-binding:t -*-

;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>
;; URL: https://github.com/haji-ali/procress.git
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))
;; Keywords: compile, progress, tex, svg

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package shows a progress indicator for a process in the modeline.
;; Typical usage:
;;
;; (require 'procress)
;; (procress-load-default-svg-images)
;; (add-hook 'LaTeX-mode-hook 'tex-procress-mode)
;;
;; The function `procress-load-default-svg-images' can be called to for SVG
;; images (an animations) to be used to indicate progress.
;;
;; The mode `tex-procress-mode' shows the progress for AUCTeX-created
;; processes.

;;; Code:
(defconst procress-animation-frames
  (let ((str1 "procress")
        (str2 "-ing"))
    (mapcar (lambda (x)
              (concat str1
                      (substring str2 0 x)
                      (make-string (- (length str2) x) 32)))
            (number-sequence 1 (length str2))))
  "The sequence of images to use to denote progress.")

(defconst procress-success-frames '("S")
  "The sequence of images to use to denote success.
Typically, only first frame is used.")

(defconst procress-failure-frames '("F")
  "The sequence of images to use to denote failure.
Typically, only first frame is used.")

(defconst procress-modeline-help-string
  "mouse-1: Go to compilation output"
  "String to use for the help-string in the modeline.")

(defvar-local procress-modeline-function 'idenity
  "Function to build modeline string.
Takes the progress string as an argument and should return the
final string.")

(defvar-local procress--current-frame nil
  "The currently displayed frame.
The car is the index used for the cdr.")

(defcustom procress-click-hook nil
  "Hook run after clicking modeline string."
  :type 'hook
  :group 'procress
  :local t)


(defun procress-modeline-string ()
  "Return the modeline string."
  (if procress--current-frame
      (propertize
       (funcall
        procress-modeline-function
        (propertize "-"
                    'display (nth (car procress--current-frame)
                                  (symbol-value
                                   (cdr procress--current-frame)))))
       'help-echo procress-modeline-help-string
       'keymap
       `(keymap (mode-line keymap
                           (down-mouse-1
                            . ,(lambda () (interactive)
                                 (run-hooks 'procress-click-hook))))))
    ""))

(defun procress-progress ()
  "Update modeline to indicate progress of a process."
  (unless (and procress--current-frame
               (eq 'procress-animation-frames
                   (cdr procress--current-frame)))
    (setq procress--current-frame
          '(0 . procress-animation-frames)))
  (setcar procress--current-frame
          (% (+ 1 (car procress--current-frame))
             (length
              (symbol-value
               (cdr procress--current-frame)))))
  (force-mode-line-update))

(defun procress-start (&rest _)
  "Update modeline to indicate start of a process."
  (setq procress--current-frame nil)
  (force-mode-line-update))

(defun procress-done (success)
  "Update modeline to indicate termination of a process.
SUCCESS is non-nil if the process is successful."
  (setq procress--current-frame
        (if success
            '(0 . procress-success-frames)
          '(0 . procress-failure-frames)))
  (force-mode-line-update))

;;;;;;;;;;;;;;;;;;;;;;;; SVG specific functions and constants
(defconst procress--svg-wrench
  '((width . 24) (height . 24)
    (path . "M13,2.03V2.05L13,4.05C17.39,4.59 20.5,8.58 19.96,12.97C19.5,16.61\
 16.64,19.5 13,19.93V21.93C18.5,21.38 22.5,16.5 21.95,11C21.5,6.25 17.73,2.5 1\
3,2.03M11,2.06C9.05,2.25 7.19,3 5.67,4.26L7.1,5.74C8.22,4.84 9.57,4.26 11,4.06\
V2.06M4.26,5.67C3,7.19 2.25,9.04 2.05,11H4.05C4.24,9.58 4.8,8.23 5.69,7.1L4.26\
,5.67M2.06,13C2.26,14.96 3.03,16.81 4.27,18.33L5.69,16.9C4.81,15.77 4.24,14.42\
 4.06,13H2.06M7.1,18.37L5.67,19.74C7.18,21 9.04,21.79 11,22V20C9.58,19.82 8.23\
,19.25 7.1,18.37M16.82,15.19L12.71,11.08C13.12,10.04 12.89,8.82 12.03,7.97C11.\
13,7.06 9.78,6.88 8.69,7.38L10.63,9.32L9.28,10.68L7.29,8.73C6.75,9.82 7,11.17 \
7.88,12.08C8.74,12.94 9.96,13.16 11,12.76L15.11,16.86C15.29,17.05 15.56,17.05 \
15.74,16.86L16.78,15.83C17,15.65 17,15.33 16.82,15.19Z"))
  "SVG related data that represents a wrench.
Taken from `Templarian/MaterialDesign', to be used with `procress--svg-create'")

(defconst procress--svg-alert
  '((width . 24) (height . 24)
    (path . "M13 14H11V9H13M13 18H11V16H13M1 21H23L12 2L1 21Z"))
  "SVG related data that represents an alert.
Taken from `Templarian/MaterialDesign', to be used with `procress--svg-create'")

(defconst procress--svg-wrench-check
  '((width . 24) (height . 24)
    (path . "M9 2C10.8 2.6 12 4.3 12 6.2C12 8.2 10.8 9.9 9 10.5V21.5C9 21.8 8.\
8 22 8.5 22H6.5C6.2 22 6 21.8 6 21.4V10.4C4.2 9.8 3 8.1 3 6.2S4.2 2.6 6 2V5.7H\
9V2M20.6 13L22 14.41L15.47 21L12 17.5L13.4 16.09L15.47 18.17L20.6 13"))
  "SVG related data that represents a wrench with a check.
Taken from `Templarian/MaterialDesign', to be used with `procress--svg-create'")

(defun procress-load-default-svg-images ()
  "Set procress images from default SVG ones.
Assumes that Emacs is compiled with SVG support."
  (eval-and-compile
    (require 'svg))
  (setq
   procress-animation-frames
   (mapcar (lambda (id)
             (procress--svg-create
              procress--svg-wrench :rotate id))
           (number-sequence 0 360 10))
   procress-success-frames
   (list (procress--svg-create
          procress--svg-wrench-check :fill "green"))
   procress-failure-frames
   (list (procress--svg-create
          procress--svg-alert :fill "red"))))

(defun procress--svg-create (svg-data &rest args)
  "Create an svg image given SVG-DATA.
The SVG-DATA is an alist containing `width', `height' and `path'.
Creates an `svg-node' with the `path' and passes ARGS.
ARGS may contain `:rotate' to rotate the SVG image relative to
the center. "
  (let* ((width  (cdr (assoc 'width svg-data)))
         (height (cdr (assoc 'height svg-data)))
         (svg (svg-create width height))
         (rotate (plist-get args :rotate)))
    (when rotate
      (let ((transform (plist-get args :transform))
            (rotate-pos (cl-position :rotate args)))
        (when transform
            ;; Remove rotate and value completelyn
            (setq args
                  (cl-delete-if (lambda (_) t)
                                args
                                :start rotate-pos
                                :end (+ 2 rotate-pos)))
          ;; Replace `:rotate' with `:transform'
          (setf (nth rotate-pos args) :transform))
        (setq args (plist-put args :transform
                              (concat transform
                                      (format " rotate(%d %d %d)"
                                              rotate
                                              (/ width 2)
                                              (/ height 2)))))))
    (apply 'svg-node
           svg 'path :d (cdr (assoc 'path svg-data))
           args)
    (svg-image svg :ascent 'center)))
;;;;;;;;;;;;;;;;;;;;;; AUCTeX specific functions/modes
(define-minor-mode tex-procress-mode
  "Show auctex progress."
  :global nil
  :require 'procress
  (eval-and-compile
    (require 'tex))
  (let ((filter-fun '(TeX-command-filter
                      TeX-format-filter
                      TeX-background-filter)))
    (if tex-procress-mode
        (progn
          (advice-add 'TeX-run-command :before 'procress-start)
          (dolist (fun filter-fun)
            (advice-add fun :after 'procress--tex-progress))
          (advice-add 'TeX-command-sentinel :after 'procress--tex-done)
          (setq mode-line-process
                '(:eval (list (procress-modeline-string))))
          (setq procress-modeline-function
                (lambda (x)
                  (concat x (with-current-buffer (TeX-active-buffer)
                              TeX-current-page))))
          (add-hook 'procress-click-hook 'procress--tex-click))
      ;; Don't remove advice, the functions are conditional on the
      ;; local mode
      ;; (advice-remove 'TeX-run-command 'procress-start)
      ;; (dolist (fun filter-fun)
      ;;   (advice-remove fun 'procress--tex-progress))
      ;; (advice-remove 'TeX-command-sentinel 'procress--tex-done)
      (remove-hook 'procress-click-hook 'procress--tex-click)
      (setq mode-line-process nil)
      (setq procress-modeline-function 'identity)))
  (force-mode-line-update))

(defun procress--tex-done (process _)
  "Update modeline to indicate termination of a tex process.
Effective only when `tex-procress-mode' is `t'."
  (when tex-procress-mode
    (with-current-buffer (procress--tex-command-buffer process)
      (procress-done (not (TeX-error-report-has-errors-p))))))

(defun procress--tex-command-buffer (process &rest _)
  "Returns modeline buffer for tex processes"
  (with-current-buffer (process-buffer process)
    TeX-command-buffer))

(defun procress--tex-click ()
  "Modeline click action for tex buffers"
  (TeX-recenter-output-buffer nil))

(defun procress--tex-progress (process &rest _)
  "Update modeline to indicate progress of a tex process.
Effective only when `tex-procress-mode' is `t'."
  (when tex-procress-mode
    (with-current-buffer (procress--tex-command-buffer process)
      (procress-progress))))

(provide 'procress)
