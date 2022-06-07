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

;; Enable tab-bar-mode
(tab-bar-mode 1)
(setq tab-bar-separator "")
(setq tab-bar-close-button-show nil)
(setq tab-bar-format '(tab-bar-format-history
		       tab-bar-format-tabs
		       tab-bar-separator
		       tab-bar-format-align-right
		       tab-bar-format-global))
(global-set-key (kbd "H-t t") 'tab-new)
(global-set-key (kbd "H-t <tab>") 'tab-next)
(global-set-key (kbd "H-t k") 'tab-close)

(defvar my/tab-numbers-alist
  '((0 . "0.")
    (1 . "1.")
    (2 . "2.")
    (3 . "⒊")
    (4 . "4.")
    (5 . "5.")
    (6 . "6.")
    (7 . "7.")
    (8 . "8.")
    (9 . "9."))
  "Alist of integers to strings of circled unicode numbers.")

(defun my/tab-bar-tab-name-format-default (tab i)
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
(setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format-default)

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
(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		mode-line-mule-info
		mode-line-client
		mode-line-modified
		mode-line-remote
		mode-line-frame-identification
		mode-line-buffer-identification
		"   "
		mode-line-position
		(vc-mode vc-mode)
		"  "
		mode-line-modes
		;; mode-line-misc-info
		mode-line-end-spaces))

;; Move custom settings to custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable ring bell
(setq ring-bell-function 'ignore)

;; Highlight the current line
(global-hl-line-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Enable line wrapping
(global-visual-line-mode 1)

;; Show matching parentheses
(show-paren-mode 1)

;; When you visit a file, point goes to the last place where it was
;; when you previously visited the same file.
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

;; Hyper bindings
;; (global-set-key (kbd "H-3") 'split-window-right)
;; (global-set-key (kbd "H-2") 'split-window-below)
(global-set-key (kbd "H-x s") 'eshell)

;; Font
(add-to-list 'default-frame-alist
             '(font . "Iosevka SS12 14"))

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
;; This package provides functionality for automatically updating your Emacs packages periodically.
(use-package auto-package-update
  :defer nil
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Diminish
;; This package implements hiding or abbreviation of the mode line
;; displays (lighters) of minor-modes.
(use-package diminish
  :ensure t)

;; Eldoc
;; Add eldoc-mode to diminish
(use-package eldoc
  :diminish eldoc-mode)

;; Helpful
;; This package is an alternative to the built-in Emacs help that provides much more
;; contextual information
(use-package helpful
  :ensure t
  :bind
  ("C-h v" . 'helpful-variable)
  ("C-h f" . 'helpful-callable)
  ("C-h k" . 'helpful-key)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
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

;; Elfeed
;; Elfeed is an extensible web feed reader for Emacs, supporting both Atom and RSS.
(use-package elfeed
  :ensure t
  :bind ("H-x e" . 'elfeed)
  :config
  (load "~/.emacs.d/feeds.el"))

;; Vertico
;; Vertico provides a performant and minimalistic vertical completion UI
;; based on the default completion system.
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

;; Orderless
;; This package provides an orderless completion style that divides the pattern into space-separated
;; components, and matches candidates that match all of the components in any order.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult
;; Consult provides practical commands based on the Emacs completion function completing-read.
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
;; Embark makes it easy to choose a command to run based on what is near point, both during a minibuffer.
;; completion session and in normal buffers.
(use-package embark
  :ensure t
  :config
  (setq prefix-help-command #'embark-prefix-help-command)) ;; uses Embark when pressing C-h during a keychord

;; Marginalia
;; This package provides marginalia-mode which adds marginalia to the minibuffer completions.
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; TODO Configure consult to replace some of the default keybings (e.g consult-go-to-line)
;; TODO Configure zap-up-to-char

;;; tab-bar ;;;
;; TODO Add padding on the right corner so that the time is not on the edge of the screen
;; TODO Open a few tabs by default
;; TODO Append new tabs after all other tabs, not after the currently focused tab
