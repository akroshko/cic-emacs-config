;; cic-emacs-site.el --- Configuring common external packages
;;  that I use.
;;
;; Copyright (C) 2015-2019, Andrew Kroshko, all rights reserved.
;;
;; Author: Andrew Kroshko
;; Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
;; Created: Thu, Aug 27, 2015
;; Version: 20190804
;; URL: https://github.com/akroshko/cic-emacs-common
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commentary:
;;
;; This provides some configuration that I use for common external
;; packages.  It is not loaded by default but can be loaded by using
;; (require 'cic-emacs-site).
;;
;; This package does not cover Auctex, that is done in a seperate
;; file.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; site specific settings
(requiring-package (cl-lib))
;; TODO: done as a test, makes some variables available
(requiring-package (find-dired))

;; TODO: prevent dbus loading if already loaded, maybe?
;; TODO: fix this
;; (requiring-package (dbus)
;;   (dbus-get-unique-name :system)
;;   (dbus-get-unique-name :session)
;;   (dbus-init-bus :system)
;;   (dbus-init-bus :session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unloading
;; I don't like doc-view because accidentally hitting the wrong document can run my system out of memory
(when (featurep 'org-docview)
  (unload-feature 'org-docview))
(when (featurep 'doc-view)
  (unload-feature 'doc-view))
;; disable docview
(add-to-list 'auto-mode-alist '("\\.pdf\\'\\|\\.djvu\\'" . fundamental-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; things that don't really require a requiring-package, but I use it anyways
(requiring-package (cc-mode)
  (define-key c-mode-map (kbd "C-h") 'c-electric-delete-forward)
  (define-key c-mode-map (kbd "C-d") 'next-line)
  (define-key java-mode-map (kbd "C-h") 'c-electric-delete-forward)
  (define-key java-mode-map (kbd "C-d") 'next-line)
  ;; TODO: awk mode map
  )

;; TODO: move this
(requiring-package (man)
  (define-key Man-mode-map (kbd ".") 'scroll-up)
  (define-key Man-mode-map (kbd ",") 'scroll-down)
  (define-key Man-mode-map (kbd "b") 'scroll-down))

(setq browse-url-browser-function 'cic:browse-url-conkeror
      browse-url-generic-program "conkeror")
(add-to-list 'command-switch-alist '("diff" . command-line-diff))
(defun command-line-diff (switch)
  "Allow diffs from the command line.
TODO broken, provided a diff cleanup function too!"
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
;; Usage: emacs -diff file1 file2

;; TODO: move automode elsewhere?
;; TODO: see if I can pattern bash* to automode alist
(add-to-list 'auto-mode-alist '("bash_profile_agents" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc_functions"    . sh-mode))
(add-to-list 'auto-mode-alist '("bash_colors"         . sh-mode))
(add-to-list 'auto-mode-alist '("bash_library"        . sh-mode))
(add-to-list 'auto-mode-alist '("bash_logout"         . sh-mode))
(add-to-list 'auto-mode-alist '("inputrc"             . sh-mode))
(add-to-list 'auto-mode-alist '(".inputrc"            . sh-mode))
(add-to-list 'auto-mode-alist '("psqlrc"              . sql-mode))

(requiring-package (sh-script)
  ;; TODO: find another thing to run executable-interpret
  (define-key sh-mode-map (kbd "C-c C-x") nil)
  ;; TOOD: not done yet
  (defun cic:sh-script-fill (&optional justify region)
    (interactive)
    (when (save-excursion
            (beginning-of-line)
            (looking-at "^[[:space:]]*#.*"))
      ;; regions...
      (fill-comment-paragraph justify))
    t)
  (defun cic:sh-script-fill-set-function ()
    (setq-local fill-paragraph-function #'cic:sh-script-fill))
  (add-hook 'sh-mode-hook 'cic:sh-script-fill-set-function)
  ;; my aliases often have
  (add-to-list 'sh-assignment-regexp '(bash . "\\<\\([[:alnum:]_-]+\\)\\(\\[.+\\]\\)?\\+?="))
  ;; XXXX: why did Emacs 25 feel the need to make this annoying mode default
  (remove-hook 'sh-mode-hook 'sh-electric-here-document-mode)
  (add-hook 'sh-mode-hook 'cic:sh-mode-disable-electric-here-document-mode)
  (defun cic:sh-mode-disable-electric-here-document-mode ()
    (sh-electric-here-document-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my custom elisp code and keys
;; easypg
(setq epa-file-select-keys 'silent
      epg-gpg-program "/usr/bin/gpg2")
(epa-file-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotate
;; XXXX: uses my fork from https://github.com/akroshko/annotate.el
(requiring-package (annotate)
  ;; only enable annotate for read-only files
  (add-hook 'find-file-hook 'cic:annotate-enable)
  (defun cic:annotate-enable ()
    (when (and buffer-file-name (string-match "\\.ro\\." buffer-file-name))
      (annotate-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bash-completion
;; (requiring-package (bash-completion)
;;   (bash-completion-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clippy
;; TODO: replace with something less silly
;; (requiring-package (clippy)
;;   ;; TODO: goofy, but put to something else
;;   (global-set-key (kbd "M-.") 'cic:clippy-describe)
;;   (defvar cic:clippy-last-show nil
;;     "")
;;   (defun cic:clippy-describe ()
;;     (interactive)
;;     (if (eq last-command 'cic:clippy-describe)
;;         (if (eq cic:clippy-last-show 'function)
;;             (progn
;;               (call-interactively 'clippy-describe-variable)
;;               (setq cic:clippy-last-show 'variable))
;;           (progn
;;             (call-interactively 'clippy-describe-function)
;;             (setq cic:clippy-last-show 'function)))
;;       (progn
;;         (call-interactively 'clippy-describe-function)
;;         (setq cic:clippy-last-show 'function))))
;;   ;; gives the table heading when at a table
;;   ;; TODO: find definition of words this way too
;;   ;; TODO: add additional things it can describe
;;   (defun clippy-org-describe ()
;;     (interactive)
;;     (when (org-at-table-p)
;;       (let* ((the-string (cic:org-get-column-heading))
;;              (the-string-filled (with-temp-buffer
;;                                   (insert the-string)
;;                                   (goto-char (point-min))
;;                                   (fill-paragraph)
;;                                   (buffer-substring (point-min) (point-max)))))
;;         (clippy-say the-string-filled))))
;;   (define-key org-mode-map (kbd "M-.") 'clippy-org-describe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company mode
(requiring-package (company)
  (global-company-mode)
  (company-quickhelp-mode 1)
  ;; TODO: add something other than F1 for 'company-show-doc-buffer, take out C-h (maybe s-h)
  (define-key company-active-map (kbd "C-d") 'company-select-next)
  (define-key company-active-map (kbd "C-e") 'company-select-previous)
  (define-key company-active-map (kbd "M-,") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-,") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  ;; (define-key company-quickhelp-mode-map (kbd "s-q")  company-quickhelp-manual-begin)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conkeror and javascript

(requiring-package (js)
  (requiring-package (conkeror-minor-mode)
    ;; (autoload 'conkeror-minor-mode "conkeror-minor-mode")
    (defun cic:js-init-conkeror-minor-mode ()
      (when (and buffer-file-name (string-match "conkeror" buffer-file-name))
        (conkeror-minor-mode 1)))
    (add-hook 'js-mode-hook 'cic:js-init-conkeror-minor-mode)
    (add-to-list 'conkeror--font-lock-keywords '("\\(p\\(?:age_mode_deactivate\\)\\)\\s-*(" 1 font-lock-function-name-face)))
  (defun cic:js-mode-disable-electic-indent ()
    ;; TODO: electric-indent-just-newline would be nice
    (electric-indent-local-mode 0))
  (add-hook 'js-mode-hook 'cic:js-mode-disable-electic-indent)
  (add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; dired-sort
;; (requiring-package (dired-sort)
;;   )

(requiring-package (flyspell)
  ;; TODO: change this if I need it
  (setq flyspell-auto-correct-binding nil)
  ;; see https://emacs.stackexchange.com/questions/5415/how-can-i-make-flyspell-ignore-urls/5435
  (defun cic:flyspell-predicate ()
    (not (thing-at-point 'url)))
  (defun cic:flyspell-text-hook ()
    (setq flyspell-generic-check-word-predicate 'cic:flyspell-predicate))
  (add-hook 'org-mode-hook 'cic:flyspell-text-hook)
  ;; TODO: this interferes with my other keys
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (defconst cic:prog-modes
    ;; TODO: expand to more likely modes
    '(c-mode emacs-lisp-mode java-mode js-mode python-mode scheme-mode sh-mode))

  (defun cic:prog-mode-p ()
    "Check if in one of my typical programming modes."
    (member major-mode cic:prog-modes))

  (defun cic:text-mode-p ()
    "Check if this is a text mode."
    (not (cic:prog-mode-p)))

  ;; TODO add to flyspell buffer
  ;; TODO move out of keys and into somewhere else?
  (defun cic:flyspell-word (&optional arg)
    "Spellcheck word and start flyspell-mode.  Prefix disables
flyspell-mode."
    (interactive "P")
    ;; reload word list by killing ispell
    ;; TODO: do not restart if nothing has changed
    ;; (ispell-kill-ispell t)
    ;; detect prog mode first
    ;; TODO: detect if flyspell started
    (cond (arg
           ;; TODO: check if flyspell is started
           (flyspell-mode -1)
           )
          (t
           (cond ;; ((not (eq last-command 'cic:flyspell-here))
            ;;  (ispell-word))
            ((and (not flyspell-mode) (cic:prog-mode-p))
             (shell-command "echo \"personal_ws-1.1 en 0\" > ~/.aspell.en.pws")
             (shell-command (concat "cat " cic:user-wordlist " >> ~/.aspell.en.pws"))
             (cic:flyspell-init-prog))
            ((and (not flyspell-mode) (cic:text-mode-p))
             (shell-command "echo \"personal_ws-1.1 en 0\" > ~/.aspell.en.pws")
             (shell-command (concat "cat " cic:user-wordlist " >> ~/.aspell.en.pws"))
             (cic:flyspell-init-text)))
           (ispell-word)
           ;; TODO: run async?
           (flyspell-buffer))))

  ;; TODO: move to somewhere more appropriate
  (setq flyspell-issue-message-flag nil)

  ;; (put 'text-mode 'flyspell-mode-predicate 'cic:flyspell-predicate)
  ;; (defun flyspell-ignore-http-and-https ()
  ;; "Function used for `flyspell-generic-check-word-predicate' to ignore stuff starting with \"http:\" or \"https:\"."
  ;; (save-excursion
  ;;   (forward-whitespace -1)
  ;;   (when (looking-at " ")
  ;;     (forward-char)
  ;;   (not (looking-at "https:?\\b"))))
  ;; )
  ;; (put 'text-mode 'flyspell-mode-predicate 'flyspell-ignore-http-and-https)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
(unless cic:emacs-minimal
  ;; no flycheck for a minimal mode
  (requiring-package (flycheck)
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (setq flycheck-check-syntax-automatically nil)
    (defun cic:flycheck-buffer-or-list (&optional arg)
      (interactive "P")
      (if arg
          (flycheck-clear)
        (if (eq last-command 'cic:flycheck-buffer-or-list)
            (if (get-buffer-window "*Flycheck errors*")
                (delete-window (get-buffer-window "*Flycheck errors*"))
              (flycheck-list-errors))
          (flycheck-buffer))))
    ;; TODO: reenalbe when it works for my workflow again
    ;; (global-set-key (kbd "C-c C-x") 'cic:flycheck-buffer-or-list)
    ;; I use these for other stuff
    ;; (global-set-key (kbd "s-,") 'flycheck-previous-error)
    ;; (global-set-key (kbd "s-.") 'flycheck-next-error)
    ;; hacks for my own stuff
    ;; (put 'python-flake8    (intern "flycheck-modes")     '(python-mode sage-mode))
    ;; (put 'python-pycompile (intern "flycheck-modes")     '(python-mode sage-mode))
    ;; (requiring-package (flycheck-pyflakes)
    ;;   (put 'python-pyflakes    (intern "flycheck-modes") '(python-mode sage-mode)))
    ;; I still install these, but most of my stuff is not good enough to
    ;; stick to rigid style
    ;; (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    ;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)
    ;; TODO: need to specify good defaults
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; free keys
(requiring-package (free-keys)
  (setq free-keys-modifiers '("" "C" "M" "C-M" "s" "H")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnuplot
(requiring-package (gnuplot)
  (add-to-list 'auto-mode-alist '("\\.gnuplot$" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.gp$"      . gnuplot-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add keys to the image-mode
(requiring-package (image-mode)
  (defun cic:image-goto-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (window-height (window-body-height))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           (image-height  (cdr image-size))
           move-width
           move-height)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-width window-width)
        (setq move-width (round (/ (- image-width window-width) 2)))
        (image-forward-hscroll move-width))
      (unless (<= image-height window-height)
        (setq move-height (round (/ (- image-height window-height) 2)))
        (image-scroll-up move-height))))

  ;; find window size
  ;; (image-display-size (image-get-display-property))
  ;; (image-size)
  ;; get frame size

  (defun cic:image-top-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           move-width)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-width window-width)
        (setq move-width (round (/ (- image-width window-width) 2)))
        (image-forward-hscroll move-width))))

  (defun cic:image-bottom-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (window-height (window-body-height))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           (image-height  (cdr image-size))
           move-width)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-width window-width)
        (setq move-width (round (/ (- image-width window-width) 2)))
        (image-forward-hscroll move-width))
      (unless (<= image-height window-height)
        (image-scroll-up (round image-height)))))

  (defun cic:image-left-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (window-height (window-body-height))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           (image-height  (cdr image-size))
           move-width
           move-height)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-height window-height)
        (setq move-height (round (/ (- image-height window-height) 2)))
        (image-scroll-up move-height))))

  (defun cic:image-right-middle ()
    (interactive)
    (let* ((window-width  (window-body-width))
           (window-height (window-body-height))
           (image-size    (image-display-size (image-get-display-property) nil))
           (image-width   (car image-size))
           (image-height  (cdr image-size))
           move-width
           move-height)
      ;; if image is smaller than window in some dimension, do not move
      (image-bob)
      (unless (<= image-width window-width)
        (image-forward-hscroll (round image-width)))
      (unless (<= image-height window-height)
        (setq move-height (round (/ (- image-height window-height) 2)))
        (image-scroll-up move-height))))

  (defun cic:image-toggle-zoom ()
    (interactive)
    (unless (boundp 'cic:image-last-zoom)
      (setq-local cic:image-last-zoom 'default))
    ;; TODO: somehow save this default zoom
    ;; TODO: indicate zoom somewhere
    )

  ;; TODO: pop back to dired, or go to next on deletion?
  (defun cic:image-delete ()
    (interactive)
    )

  (defconst cic:image-move-lines-columns 8
    "Amount of move in images by lines")

  (defun cic:image-previous-lines ()
    (interactive)
    (image-previous-line cic:image-move-lines-columns))

  (defun cic:image-next-lines ()
    (interactive)
    (image-next-line cic:image-move-lines-columns))

  (defun cic:image-backward-hscroll ()
    (interactive)
    (image-backward-hscroll cic:image-move-lines-columns))

  (defun cic:image-forward-hscroll ()
    (interactive)
    (image-forward-hscroll cic:image-move-lines-columns))

  ;; TODO: need down to be middle lower of buffer
  (define-key image-mode-map (kbd "e")               'image-scroll-up)
  (define-key image-mode-map (kbd "s")               'image-backward-hscroll)
  (define-key image-mode-map (kbd "d")               'image-scroll-down)
  (define-key image-mode-map (kbd "f")               'image-forward-hscroll)
  (define-key image-mode-map (kbd "D")               'cic:dired-preview-image-delete)
  (define-key image-mode-map (kbd "s-c i")           'cic:dired-preview-image-mode)
  (define-key image-mode-map (kbd "<up>")            'cic:image-previous-lines)
  (define-key image-mode-map (kbd "<kp-up>")         'cic:image-previous-lines)
  (define-key image-mode-map (kbd "<down>")          'cic:image-next-lines)
  (define-key image-mode-map (kbd "<kp-down>")       'cic:image-previous-lines)
  (define-key image-mode-map (kbd "<left>")          'cic:image-backward-hscroll)
  (define-key image-mode-map (kbd "<prior>")         'cic:image-backward-hscroll)
  (define-key image-mode-map (kbd "<right>")         'cic:image-forward-hscroll)
  (define-key image-mode-map (kbd "<next>")          'cic:image-forward-hscroll)
  ;; move to extreme points in image
  (define-key image-mode-map (kbd "C-<kp-up>")       'cic:image-top-middle)
  (define-key image-mode-map (kbd "C-<up>")          'cic:image-top-middle)
  (define-key image-mode-map (kbd "C-<kp-down>")     'cic:image-bottom-middle)
  (define-key image-mode-map (kbd "C-<down>")        'cic:image-bottom-middle)
  (define-key image-mode-map (kbd "C-<kp-left>")     'cic:image-left-middle)
  (define-key image-mode-map (kbd "C-<prior>")       'cic:image-left-middle)
  (define-key image-mode-map (kbd "C-<kp-right>")    'cic:image-right-middle)
  (define-key image-mode-map (kbd "C-<next>")        'cic:image-right-middle)
  (define-key image-mode-map (kbd "C-<kp-home>")     'image-bob)
  (define-key image-mode-map (kbd "C-<f17>")         'image-bob)
  (define-key image-mode-map (kbd "C-<kp-prior>")    (lambda (&optional arg) (interactive) (image-bob) (image-eol arg)))
  (define-key image-mode-map (kbd "C-<f19>")         (lambda (&optional arg) (interactive) (image-bob) (image-eol arg)))
  (define-key image-mode-map (kbd "C-<kp-end>")      (lambda (&optional arg) (interactive) (image-eob) (image-bol arg)))
  (define-key image-mode-map (kbd "C-<f13>")         (lambda (&optional arg) (interactive) (image-eob) (image-bol arg)))
  (define-key image-mode-map (kbd "C-<kp-next>")     'image-eob)
  (define-key image-mode-map (kbd "C-<f15>")         'image-eob)
  ;; TODO: kp-begin is 5, toggle fit width/height/etc., maybe make this 0? would love 5 to go to middle
  ;; TODO: figure these out
  (define-key image-mode-map (kbd "C-<kp-begin>")    'cic:image-goto-middle)
  (define-key image-mode-map (kbd "<kp-insert>")     'cic:image-toggle-zoom)
  ;; the C-S- prevents accidental deletion
  (define-key image-mode-map (kbd "C-S-<kp-delete>") 'cic:image-delete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; will add jumplist package here when I decide on one
;; (requiring-package (jumplist)
;;   (global-set-key (kbd "H-i") 'jumplist-previous)
;;   (global-set-key (kbd "H-o") 'jumplist-next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;; TODO: do not use this right now
;; (requiring-package (magit)
;;   (global-set-key (kbd "C-x g") 'magit-status)
;;   ;; TODO: probably not needed
;;   (global-set-key (kbd "C-x l") 'magit-log-buffer-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nxml
(requiring-package (nxml-mode)
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
  (define-key nxml-mode-map (kbd "M-h") 'kill-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; site-specific lisp hooks and configurations
;; paredit and eldoc

(define-key emacs-lisp-mode-map (kbd "s-,") 'cic:find-documentation-transient-window)
(define-key emacs-lisp-mode-map (kbd "s-.") 'cic:find-definition-transient-window)
(add-to-list 'auto-mode-alist '("zile"  . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '(".zile" . emacs-lisp-mode))
(requiring-package (paredit)
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  ;; TODO: what if I eval this??? then what
  (with-current-buffer "*scratch*"
    (enable-paredit-mode))
  (eval-after-load "paredit.el"
    (requiring-package (paredit-menu)))
  ;; paredit-mode keys map
  ;; TODO: add something for paredit-comment-dwim
  (define-key paredit-mode-map (kbd "M-;")   'comment-dwim-line)
  (define-key paredit-mode-map (kbd "C-d")   'next-line)
  (define-key paredit-mode-map (kbd "C-h")   'paredit-forward-delete)
  (define-key paredit-mode-map (kbd "M-h")   'paredit-forward-kill-word)
  (define-key paredit-mode-map (kbd "C-M-s") 'paredit-backward)
  (define-key paredit-mode-map (kbd "C-M-b") nil)
  (define-key paredit-mode-map (kbd "M-d")   'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "M-e")   'paredit-raise-sexp))

(requiring-package (eldoc)
  ;; if not already loaded
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  ;; TODO: what if I eval this??? then what
  (with-current-buffer "*scratch*"
    (eldoc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode
;; http://masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; TODO do I want to do something else for C-s C-r
;; TODO m-n/p is very good
;; TODO ffap
;; TODO good completion for org-mode
;; XXXX: keeping things fast for now
(when cic:ido-enable
  ;; TODO: want option for flex matching all the time
  (setq ido-enable-flex-matching t)
  ;; XXXX: if t many things like describe-function is slow
  (setq ido-everywhere nil)
  (requiring-package (ido)
    (requiring-package (cic-ido-hacks))
    ;; TOOD: possibly reenable without ido-ubiquitous-mode
    ;; (requiring-package (ido-completing-read+)
    ;;   ;; TODO: add any others
    ;;   ;; TODO: change to ido-completing-read+
    ;;   ;; (setq ido-ubiquitous-command-overrides '((disable prefix "org-capture")))
    ;;   (add-to-list 'ido-cr+-function-blacklist "org-capture.*"))
    (set-face-foreground 'ido-only-match "DarkGreen")
    (set-face-attribute  'ido-only-match nil :weight 'bold)
    ;; TODO: below here de-commented
    (ido-mode t)
    ;; TODO: change to ido-completing-read+
    ;; (ido-ubiquitous-mode t)
    (requiring-package (cic-ido-hacks)
      (ido-hacks-mode t))
    (put 'insert-char 'ido 'ignore)
    ;; much faster performance than ido-vertical than, especially for describe-function
    ;; no compatible with ido-hacks because it overrides ido-decorations
    ;; https://www.emacswiki.org/emacs/InteractivelyDoThings#toc24
    (setq ido-decorations (quote ("\n->  " "" "\n    " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]" "\n-> [" "]")))
    (defun ido-disable-line-truncation ()
      (set (make-local-variable 'truncate-lines) nil))
    (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
    (defvar cic:ido-exit-symbol
      nil
      "An exit symbol for ido.")
    ;; TODO: are there keys I should disable and remove
    (defun cic:ido-goback ()
      (interactive)
      (setq cic:ido-exit-symbol 'goback)
      (exit-minibuffer))
    (defun ido-define-keys ()
      (setq cic:ido-exit-symbol nil)
      ;; TODO: ido-fallback command?
      ;; (define-key ido-completion-map (kbd "C-n")       'ido-next-match)
      (define-key ido-completion-map (kbd "C-2")       'cic:ido-goback)
      (define-key ido-completion-map (kbd "H-f")       'ido-next-match)
      ;; (define-key ido-completion-map (kbd "C-p")       'ido-prev-match)
      (define-key ido-completion-map (kbd "H-r")       'ido-prev-match)
      (define-key ido-completion-map (kbd "C-d")       'ido-next-match)
      (define-key ido-completion-map (kbd "C-w")       'ido-next-match)
      (define-key ido-completion-map (kbd "C-e")       'ido-prev-match)
      (define-key ido-completion-map (kbd "C-SPC")     'exit-minibuffer))
    (add-hook 'ido-setup-hook 'ido-define-keys)
    (setq ido-max-prospects 50)
    ;; TODO: might change to C-b
    (global-set-key (kbd "C-w") 'ido-switch-buffer)))

(requiring-package (image+)
  (eval-after-load 'image '(require 'image+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
(requiring-package (json-mode)
  (require 'rx)
  (defconst cic:json-mode-comment-re
    (rx line-start
        (zero-or-more whitespace)
        (group
         (char ?/)
         (char ?/)
         (zero-or-more not-newline)
         eol)))
  (defun cic:json-mode-disable-electric-indent ()
    ;; TODO: electric-indent-just-newline would be nice
    (electric-indent-local-mode 0))
  (add-hook 'json-mode-hook 'cic:json-mode-disable-electric-indent)
  ;; TODO: make sure this does not grow list indefinitely as I reload this file
  ;; TODO: using warning rather than comment because comments are not valid vanilla json syntax
  (add-to-list 'json-font-lock-keywords-1 (list cic:json-mode-comment-re 1 font-lock-warning-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown-mode
(requiring-package (markdown-mode)
  (defun markdown-fill-paragraph--avoid-headings (orig-fun &rest args)
    "Ensures that fill-paragraph (which I do by instinct even
when inappropriate) is not called at markdown headings."
    (if (markdown-heading-at-point)
        t
      (apply orig-fun args)))
  (advice-add 'markdown-fill-paragraph :around #'markdown-fill-paragraph--avoid-headings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-bullets
;; TODO: really slows stuff down
(requiring-package (org-bullets)
  (add-hook 'org-mode-hook 'cic:org-bullets-mode-init)
  (defun cic:org-bullets-mode-init ()
    ;; for my emacs-otlb package, want to put this somewhere else maybe?
    ;; cuts an n=1 test from 55s to 4-7s
    ;; TODO: where else do I want to kill org-bullets?
    (unless (or (derived-mode-p 'org-writing-mode) (and buffer-file-name (string-match "pedestrian-log.*\\.org" buffer-file-name)))
      (org-bullets-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pos-tip
(requiring-package (pos-tip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python and sage
;; add to minor mode and flymake map?
;; http://stackoverflow.com/questions/2571436/emacs-annoying-flymake-dialog-box
;; TODO: put into message box
;; TODO: navigate functions/defun
(requiring-package (python)
  ;; set up variables
  ;; disable shell-completion for now, this seems to mess some stuff up badly
  (setq python-shell-completion-native-enable nil)
  ;; TODO: not sure I need this
  ;; (setq comint-dynamic-complete-functions nil)
  ;; was '(comint-c-a-p-replace-by-expanded-history comint-filename-completion), which seem to cause problems
  ;; set up python keys
  ;; TODO: figure this
  ;; (define-key python-mode-map (kbd "M-[") 'python-indent-shift-left)
  ;; (define-key python-mode-map (kbd "M-]") 'python-indent-shift-right)
  ;; (define-key python-mode-map (kbd "s-x c") 'cic:check-python)
  ;; (defun cic:check-python ()
  ;;   (interactive)
  ;;   (when (buffer-live-p (get-buffer "*cic-python-check*"))
  ;;     (with-current-buffer-erase "*cic-python-check*"
  ;;                                (goto-char (point-min))))
  ;;   (let ((return-code (call-process "python" nil "*cic-python-check*" nil "-m" "py_compile" buffer-file-name)))
  ;;     (if (equal return-code 0)
  ;;         (message "Syntax check passed!!!")
  ;;       ;; TODO: flash if failed...
  ;;       ;; (message "***!!!Syntax check failed!!!***")
  ;;       (message (with-current-buffer "*cic-python-check*" (buffer-substring (point-min) (point-max)))))))

  ;; (setq python-shell-interpreter "ipython")
  ;; (setq python-shell-interpreter-args "--profile=emacs_inferior --simple-prompt --colors=NoColor -i")

  (defun python-detect-interpreter ()
    ;; read first line for shebang
    (condition-case nil
        (save-excursion
          (goto-char (point-min))
          (let ((theline (cic:get-current-line)))
            (cond ((string-match "sage.*python"  theline)
                   (setq-local python-shell-interpreter "sage")
                   (setq-local python-shell-interpreter-args "-python" "-i")
                   ;; (setq-local python-shell-interpreter-args "-ipython --profile=emacs_inferior --simple-prompt --colors=NoColor -i")
                   )
                  ;; XXXX: match sage without Python
                  ;; TODO: probably does not work
                  ((string-match "sage"  theline)
                   (setq-local python-shell-interpreter "sage")
                   (setq-local python-shell-interpreter-args ""))
                  ;; default is just default built-in python
                  (t
                   (setq-local python-shell-interpreter "ipython")
                   (setq-local python-shell-interpreter-args "--profile=emacs_inferior --simple-prompt --colors=NoColor -i")))))
      (error (message "Error setting up inferior process!!!"))))
  ;; add the hook to detect interpretor
  (add-hook 'python-mode-hook 'python-detect-interpreter)
  ;; (add-hook 'python-mode-hook 'anaconda-mode)
  ;; (add-hook 'python-mode-hook 'eldoc-mode)
  )
;; (define-key python-mode-map (kbd "C-c C-v") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow
(requiring-package (rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readline
(requiring-package (readline-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme
(setq scheme-program-name "mit-scheme")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime
(requiring-package (slime)
  (add-hook 'lisp-mode-hook 'cic:init-slime-mode)
  (defun cic:init-slime-mode ()
    (slime-mode t))
  (add-hook 'inferior-lisp-mode-hook 'cic:init-inferior-slime-mode)
  (defun cic:init-inferior-slime-mode ()
    (inferior-slime-mode t))
  ;; Optionally, specify the lisp program you are using. Default is "lisp"
  (setq inferior-lisp-program "sbcl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smartscan
(requiring-package (smartscan)
  (global-smartscan-mode -1)
  (define-key smartscan-map (kbd "M-n") nil)
  (define-key smartscan-map (kbd "M-p") nil)
  (define-key smartscan-map (kbd "M-'") nil)
  (define-key smartscan-map (kbd "C-n") 'cic:smartscan-symbol-or-region-go-forward)
  (define-key smartscan-map (kbd "C-p") 'cic:smartscan-symbol-or-region-go-backward)
  (global-smartscan-mode 1)

  (defun cic:smartscan-symbol-or-region-go-forward ()
    "If region active, go smartscan forward in that regions.
Useful for strings that don't fit nicely as words or symbols."
    (interactive)
    (cond ((region-active-p)
           (let ((region-beg (region-beginning))
                 (region-end (region-end)))
             (set-mark region-beg)
             (goto-char region-end)
             (activate-mark))
           (let ((the-region-string (buffer-substring (region-beginning) (region-end))))
             (setq mark-active nil)
             (smartscan-symbol-goto the-region-string 'forward)
             (save-excursion
               (re-search-backward the-region-string)
               (set-mark (point))
               (goto-char (+ (point) (length the-region-string)))
               (activate-mark))))
          (t
           (setq cic:smartscan-last-region nil)
           (smartscan-symbol-go-forward)))))

  (defun cic:smartscan-symbol-or-region-go-backward ()
    "If region active, go smartscan backwards in that regions.
Useful for strings that don't fit nicely as words or symbols."
    (interactive)
    (cond ((region-active-p)
           (let ((region-beg (region-beginning))
                 (region-end (region-end)))
             (set-mark region-end)
             (goto-char region-beg)
             (activate-mark))
           (let ((the-region-string (buffer-substring (region-beginning) (region-end))))
             (setq mark-active nil)
             (smartscan-symbol-goto the-region-string 'backward)
             (save-excursion
               (re-search-forward the-region-string)
               (set-mark (point))
               (goto-char (+ (point) (length the-region-string)))
               (activate-mark))))
          (t
           (setq cic:smartscan-last-region nil)
           (smartscan-symbol-go-backward))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql
(requiring-package (sql)
  (defun cic:sql-setup ()
    (sql-highlight-postgres-keywords)
    (setq-local tab-width 4))
  ;; TODO: change this to recognize something once I use more sql variations
  (add-hook 'sql-mode-hook 'cic:sql-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spice-mode
(requiring-package (spice-mode)
  ;; TODO: add more as required
  ;; TODO: configure for ngspice
  (add-to-list 'auto-mode-alist '("\\.cir$" . spice-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(requiring-package (vc)
  (setq vc-follow-symlinks t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; w3m
(setq w3m-default-display-inline-images t
      w3m-use-cookies t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vline
(add-hook 'calendar-mode-hook 'cic:calendar-init-highlight)
(defun cic:calendar-init-highlight ()
  (setq cursor-type 'box)
  (hl-line-mode)
  (vline-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some nice packages I like
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(setq package-check-signature nil
      cic:package-list '(company-auctex
                         flymake-cursor
                         ;; image-dired+
                         ;; letcheck
                         ;; py-import-check
                         ;; TODO: get rid of flymake
                         apache-mode
                         apt-sources-list
                         arduino-mode
                         async
                         auctex
                         ;; conflicts with shells such as Python
                         ;; bash-completion
                         bbdb
                         bbdb-ext
                         benchmark-init
                         biblio
                         capture
                         cdlatex
                         centered-cursor-mode
                         clippy
                         company
                         company-arduino
                         company-c-headers
                         company-irony
                         company-math
                         company-quickhelp
                         conkeror-minor-mode
                         crontab-mode
                         csv-mode
                         cython-mode
                         dash
                         diff-hl
                         dired-avfs
                         dired-hacks-utils
                         ;; dired-icon
                         ;; dired-rainbow
                         eimp
                         ;; emms
                         ;; emms-player-mpv
                         epl
                         ess
                         f
                         flycheck
                         flycheck-bashate
                         flycheck-checkbashisms
                         flycheck-cython
                         flycheck-package
                         flycheck-pyflakes
                         flymake-cursor
                         free-keys
                         gh-md
                         ;; ghub
                         git-timemachine
                         gited
                         gnuplot
                         gnuplot-mode
                         ;; TODO: may interfere with comint
                         ;; ido-completing-read+
                         ido-hacks
                         idomenu
                         image+
                         json-mode
                         ;; jumplist
                         latex-extra
                         lua-mode
                         markdown-mode
                         math-symbol-lists
                         matlab-mode
                         memoize
                         mew
                         nhexl-mode
                         org
                         org-bullets
                         package-lint
                         paredit
                         paredit-menu
                         peep-dired
                         pos-tip
                         ;; prolog
                         ;; pythonic
                         rainbow-delimiters
                         readline-complete
                         s
                         sage-shell-mode
                         sicp
                         simple-call-tree
                         simple-httpd
                         slime
                         slime-company
                         smartscan
                         spice-mode
                         ssh-config-mode
                         ssh-tunnels
                         systemd
                         tracwiki-mode
                         w3m
                         wanderlust
                         with-editor
                         xml-rpc))

(defun cic:install-packages ()
  (interactive)
  (let ((failed nil))
    (dolist (package cic:package-list)
      ;; package-built-in-p something else to check?
      (let ((package-found (cic:package-installed-manager-p package)))
        (cond ((not package-found)
               ;; there are sometimes errors, catch them
               (condition-case error-string
                   (package-install package)
                 (error (progn
                          (setq filed t)
                          (message (concat "Failed to install package: " (symbol-name package)))))))
              (t
               (message (concat "The package: " (symbol-name package) " already found as " (pp-to-string package-found)))))))
    (when failed
      (message "Some packages failed to install!"))))
;; error-string

(defun cic:update-packages ()
  (package-list-packages)
  (with-current-buffer "*Packages*"
    (package-menu-mark-upgrades)
    ;; copied from package.el.gz
    (let (install-list)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq cmd (char-after))
          (unless (eq cmd ?\s)
            (setq pkg-desc (tabulated-list-get-id))
            (when (eq cmd ?I)
              (push pkg-desc install-list)))
          (forward-line)))
      (when install-list
        (dolist (package install-list)
          (condition-case error-string
              (progn
                (package-install package)
                (message (concat "Successfully upgraded package: " (symbol-name (elt package 1)))))
            (error (message (concat "Failed to upgrade package: " (symbol-name (elt package 1)) " " error-string)))))))
    (revert-buffer)))

(defun cic:package-installed-manager-p (package)
  ""
  ;; TODO: not completely tested, deals with stuff I fixed manually
  (cdr (assq package package-alist)))

(provide 'cic-emacs-site)
