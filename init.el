;; -*- lexical-binding: t; -*-
;; Add MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; Disable default startup screen
(setq inhibit-startup-message t)

;; Start Emacs in fullscreen
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; Enable tab-bar-mode
(tab-bar-mode 1)
(setq tab-bar-separator "")
(setq tab-bar-tab-hints t)
(setq tab-bar-close-button-show nil)
(setq tab-bar-format '(tab-bar-format-history
		       tab-bar-format-tabs
		       tab-bar-separator
		       tab-bar-format-align-right
		       tab-bar-format-global))

(eval-after-load "tab-bar"
(defun tab-bar-format-align-right ()
  "Align the rest of tab bar items to the right."
  (let* ((rest (cdr (memq 'tab-bar-format-align-right tab-bar-format)))
         (rest (tab-bar-format-list rest))
         (rest (mapconcat (lambda (item) (nth 2 item)) rest ""))
         (hpos (length rest))
         (str (propertize " " 'display `(space :align-to (- right ,hpos 2)))))
    `((align-right menu-item ,str ignore)))))

(global-set-key (kbd "H-t t") 'tab-new)
(global-set-key (kbd "H-t k") 'tab-close)

(defvar my/tab-numbers-alist
  '((0 . "0.")
    (1 . "1.")
    (2 . "2.")
    (3 . "3.")
    (4 . "4.")
    (5 . "5.")
    (6 . "6.")
    (7 . "7.")
    (8 . "8.")
    (9 . "9."))
  "Alist of integers to strings of circled unicode numbers.")

(defun oli/tab-bar-tab-name-format-default (tab i)
  (let ((current-p (eq (car tab) 'current-tab))
        (tab-num (if (and tab-bar-tab-hints (< i 10))
                     (alist-get i my/tab-numbers-alist) "")))
    (propertize
     (concat " "
	     tab-num
	     " "
	     (alist-get 'name tab)
             " ")
     'face (funcall tab-bar-tab-face-function tab))))
(setq tab-bar-tab-name-format-function #'oli/tab-bar-tab-name-format-default)

;; Keybindings to switch tabs using numbers
(global-set-key (kbd "H-1") (lambda () (interactive) (tab-bar-select-tab 1)))
(global-set-key (kbd "H-2") (lambda () (interactive) (tab-bar-select-tab 2)))
(global-set-key (kbd "H-3") (lambda () (interactive) (tab-bar-select-tab 3)))
(global-set-key (kbd "H-4") (lambda () (interactive) (tab-bar-select-tab 4)))
(global-set-key (kbd "H-5") (lambda () (interactive) (tab-bar-select-tab 5)))
(global-set-key (kbd "H-6") (lambda () (interactive) (tab-bar-select-tab 6)))
(global-set-key (kbd "H-7") (lambda () (interactive) (tab-bar-select-tab 7)))
(global-set-key (kbd "H-8") (lambda () (interactive) (tab-bar-select-tab 8)))
(global-set-key (kbd "H-9") (lambda () (interactive) (tab-bar-select-tab 9)))

;; Modeline
(defun oli/simple-mode-line-render (left right)
  "Return a string of `window-width' length.  Containing LEFT, and
RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default
 mode-line-format
 '((:eval
    (oli/simple-mode-line-render
     ;; Left.
     (quote (" "
	     mode-line-mule-info
	     mode-line-client
	     mode-line-modified
	     mode-line-remote
	     "  "
	     (:eval (when (bound-and-true-p meow-mode) (meow-indicator)))
	     "  "
             "L%l"))
     ;; Right.
     (quote (
	    "%b [%m] "))))))

;; Move custom settings to custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable ring bell
(setq ring-bell-function 'ignore)

;; Column beyond which automatic line-wrapping should happen.
(setq fill-column 80)

;; Highlight the current line
(global-hl-line-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Enable line wrapping
(global-visual-line-mode 1)

;; Show matching parentheses
(show-paren-mode 1)

;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
(save-place-mode 1)

;; Enable minibuffer history
(savehist-mode 1)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; File backup settings
(setq load-prefer-newer t)
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Save clipboard text into kill ring before replacing it
(setq save-interprogram-paste-before-kill t)

;; Make apropos command search more extensively
(setq apropos-do-all t)

;; Restore some of the disabled commands
(put 'dired-find-alternate-file 'disabled nil)

;; Disable s-* bindings
(global-unset-key (kbd "s-z"))
(global-unset-key (kbd "s-x"))
(global-unset-key (kbd "s-c"))
(global-unset-key (kbd "s-v"))
(global-unset-key (kbd "s-q"))

;; Disable suspend Emacs binding
(global-unset-key (kbd "C-z"))

;; Replace zap-to-char with zap-up-to-char
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Disable arrow keys
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

;; Remap Control to Command and Meta to Option on macOS keyboard
(setq ns-alternate-modifier 'meta)            ;; map Alt/Option to be Meta
(setq ns-command-modifier 'control)           ;; map Command to be Control
(setq ns-right-alternate-modifier 'meta)      ;; map Right Alt/Option to be Meta
(setq ns-right-command-modifier 'hyper)       ;; map Right Control to be Hyper
(setq ns-control-modifier 'super)             ;; map Control to be Super

;; Switch bindings between default isearch and isearch with regexp
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Easier resize bindings
(global-set-key (kbd "s-C-l") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-h") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-k") 'shrink-window)
(global-set-key (kbd "s-C-j") 'enlarge-window)

;; Easier binding to switch windows
(global-set-key (kbd "H-o") 'other-window)

;; Easier delete window
(global-set-key (kbd "H-0") 'delete-window)

;; Easier split bindings
(global-set-key (kbd "H-[") 'split-window-right)
(global-set-key (kbd "H-]") 'split-window-below)

;; Font
(add-to-list 'default-frame-alist
             '(font . "Comic Code 12"))

;; Theme
(load-theme 'modus-operandi t)
(global-set-key (kbd "<f5>") 'modus-themes-toggle)

;; Display time and date in the modeline
(use-package time
  :commands world-clock
  :config
  (setq display-time-format "%d-%m-%Y %H:%M")
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

;; Display battery level in the modeline
(display-battery-mode 1)

;; auto-package-update
;; This package provides functionality for automatically updating your Emacs
;; packages periodically.
(use-package auto-package-update
  :defer nil
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Diminish
;; This package implements hiding or abbreviation of the mode line displays
;; (lighters) of minor-modes.
(use-package diminish
  :ensure t)

;; Pulsar
;; This is a small package that temporarily highlights the current line after a
;; given function is invoked.
(use-package pulsar
  :ensure t
  :init
  (pulsar-global-mode 1))

;; eldoc.el
;; Add eldoc-mode to diminish
(use-package eldoc
  :diminish eldoc-mode)

;; simple.el
;; Add visual-line-mode to diminish
(use-package simple
  :diminish visual-line-mode)

;; Helpful
;; This package is an alternative to the built-in Emacs help that provides much
;; more contextual information
(use-package helpful
  :ensure t
  :bind
  ("C-h v" . 'helpful-variable)
  ("C-h f" . 'helpful-callable)
  ("C-h k" . 'helpful-key)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding for this
  ;; in lisp modes.
  ("C-c C-d" . 'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  ("C-h F" . 'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  ("C-h C" . 'helpful-command))

;; Olivetti
;; A simple Emacs minor mode for a nice writing environment.
(use-package olivetti
  :ensure t
  :diminish olivetti-mode
  :config
  (global-set-key (kbd "<f9>") 'olivetti-mode))

;; Elfeed
;; Elfeed is an extensible web feed reader for Emacs, supporting both Atom and RSS.
(use-package elfeed
  :ensure t
  :bind ("H-x e" . 'elfeed)
  :config
  (load "~/.emacs.d/feeds.el"))

;; Vertico
;; Vertico provides a performant and minimalistic vertical completion UI based
;; on the default completion system.
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

;; Orderless
;; This package provides an orderless completion style that divides the pattern
;; into space-separated components, and matches candidates that match all of the
;; components in any order.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult
;; Consult provides practical commands based on the Emacs completion function
;; completing-read.
(use-package consult
  :ensure t
  :config
  (setq switch-to-buffer #'consult-buffer)
  :bind
  ;; ("C-x b" . 'consult-buffer)   ;; replace default switch-to-buffer with consult-buffer
  ("C-s"   . 'consult-line)     ;; replace default isearch with consult-line
  ("H-c b" . 'consult-bookmark)
  ("H-c a" . 'consult-apropos))

;; Embark
;; Embark makes it easy to choose a command to run based on what is near point,
;; both during a minibuffer.  completion session and in normal buffers.
(use-package embark
  :ensure t
  :config
  (setq prefix-help-command #'embark-prefix-help-command)) ;; uses Embark when pressing C-h during a keychord

;; Marginalia
;; This package provides marginalia-mode which adds
;; marginalia to the minibuffer completions.
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; Meow
;; Meow aims to blend modal editing into Emacs with minimal interference with
;; its original key-bindings, avoiding most of the hassle introduced by
;; key-binding conflicts.
(use-package meow
  :ensure t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  :config
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1))

;; TODO Find better defaults
;; TODO Fix bug of mode-name not showing for Elisp mode
;; TODO Put meow-indicator in bold
;; TODO Configure consult to replace some of the default keybings (e.g consult-go-to-line)
;; TODO Cycle between windows clockwise (ask on SE)
;; TODO Port configuration to org-mode
;; TODO Append new tabs after all other tabs, not after the currently focused tab
