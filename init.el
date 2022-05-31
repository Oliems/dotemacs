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

;; Move custom settings to custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Enable ido-mode
(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

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
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Easier resize bindings
(global-set-key (kbd "s-C-l") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-h") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-k") 'shrink-window)
(global-set-key (kbd "s-C-j") 'enlarge-window)

;; Hyper bindings
(global-set-key (kbd "H-3") 'split-window-right)
(global-set-key (kbd "H-2") 'split-window-below)
(global-set-key (kbd "H-x s") 'eshell)

;; Font
(add-to-list 'default-frame-alist
             '(font . "Fira Code 14"))

;; Theme
;; (load-theme 'modus-vivendi t)
(load-theme 'modus-operandi t)

;; Display time and date in the modeline
(use-package time
  :commands world-clock
  :config
  (setq display-time-format "%H:%M  %d-%m-%Y")
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

;; Eldoc
;; Add eldoc-mode to diminish
(use-package eldoc
  :diminish eldoc-mode)

;; Whichkey
;; This package displays available keybindings in popup
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

;; Diminish
;; This package implements hiding or abbreviation of the mode line
;; displays (lighters) of minor-modes.
(use-package diminish
  :ensure t)

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

;; Swiper
;; Swiper is an alternative to isearch that uses Ivy to show an overview of all matches.
(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper)) ;; replace default isearch with swiper

;; Avy
;; This package allows jumping to visible text using a char-based decision tree
(use-package avy
  :ensure t
  :bind ("M-z" . avy-goto-char)) ;; replace zap-to-char whith avy-goto-char

;; Elfeed
;; Elfeed is an extensible web feed reader for Emacs, supporting both Atom and RSS.
(use-package elfeed
  :ensure t)

;; Dashboard
;; An extensible emacs startup screen showing you what’s most important.
(use-package dashboard
  :ensure t
  :defer nil
  :preface
  (defun update-config ()
    "Update to the latest version."
    (interactive)
    (let ((dir (expand-file-name user-emacs-directory)))
      (if (file-exists-p dir)
          (progn
            (message "Updating...")
            (cd dir)
            (shell-command "git pull")
            (message "Update finished. Switch to the messages buffer to see changes and then restart Emacs"))
        (message "\"%s\" doesn't exist." dir))))

  (defun create-scratch-buffer ()
    "Create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)))
  (setq dashboard-banner-logo-title "Emacs: the extensible customizable self-documenting display editor")
  (setq dashboard-startup-banner "~/.emacs.d/lambda.png")
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "%d packages loaded in %s"
                                    (length package-activated-list) (emacs-init-time)))
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(
          ((,nil
	    "Open scratch buffer"
	    "Switch to the scratch buffer"
	    (lambda (&rest _) (create-scratch-buffer))
	    'default)
	   (nil
	    "Open init.el"
	    "Open configuration file"
	    (lambda (&rest _) (find-file "~/.emacs.d/init.el"))
	    'default)
	   (nil
	    "Update init.el"
	    "Pull changes from Github"
	    (lambda (&rest _) (update-config))
	    'default)))))
