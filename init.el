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
'(ns-alternate-modifier 'meta)            ;; map Alt/Option to be Meta
'(ns-command-modifier 'control)           ;; map Command to be Control
'(ns-right-alternate-modifier 'meta)      ;; map Right Alt/Option to be Meta
'(ns-right-command-modifier 'control)     ;; map Right Control to be Control
'(ns-control-modifier 'super)             ;; map Control to be Super

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

;; Whichkey
;; The whichkey package prints possible completions for keychords
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

;; Dired
;; use dired-find-alternate-file which is bound to a in dired-mode to opend folders without creating a new buffer
(put 'dired-find-alternate-file 'disabled nil)

;; TODO Move custom-set-variables out of init.el
;; TODO Install packages automatically
;; TODO bind hyper key
;; http://xahlee.info/emacs/emacs/emacs_hyper_super_keys.html
;; TODO Configure dashboard
