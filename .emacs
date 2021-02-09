;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Disable menu bar
(menu-bar-mode -1)

;; Disable tool bar
(tool-bar-mode -1)

;; Disable scroll bar

;; Enable show-paren-mode
(show-paren-mode 1)

(toggle-scroll-bar -1)

;; Fix keyboard behavior on Mac
(pcase system-type
  ('darwin (setq mac-option-modifier nil
		 mac-command-modifier 'meta
		 x-select-enable-clipboard t)
	   (global-set-key (kbd "M-v") 'clipboard-yank)
	   (global-set-key (kbd "M-c") 'clipboard-kill-ring-save)))

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Configure package manger 
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Font settings
(pcase system-type
  ('gnu/linux (set-face-attribute 'default nil :font "Fira Code Retina")
	      (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina")
	      (set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 200 :weight 'regular))
  ('darwin (set-face-attribute 'default nil :font "Fira Code")
	   (set-face-attribute 'fixed-pitch nil :font "Fira Code")
	   (set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 200 :weight 'regular)))

;; Set tab indent to 4 spaces
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq tab-width 4)
(setq tab-stop-list (my-generate-tab-stops))

;; Rainbow delimters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Autocomplete
(use-package auto-complete
:ensure t
:init
(progn
  (ac-config-default)
  (global-auto-complete-mode t)
  ))

;; Ivy and Counsel
(use-package ivy
  :ensure t)
(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (ivy-rich-mode 1))
(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; web-mode
(use-package web-mode
  :ensure t
  :hook web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode)))

;; neotree
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

;; Which key?
(use-package which-key
        :ensure t
        :config
        (which-key-mode))

;; Enable evil mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; Configure org-mode
(defun kk/org-mode-config ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org-mode font setup
(defun kk/org-font-config ()
  ;; Set fixed-pitch where needed
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :ensure t
  :hook (org-mode . kk/org-mode-config)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  ;;  (with-eval-after-load' org-faces
  ;;			 (kk/org-mode-config)))
  (add-hook 'org-mode-hook (lambda () (kk/org-font-config))))


;; Make org-bullets nice
(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Load theme-related stuff
(use-package color-theme-sanityinc-tomorrow
  :ensure t)

;; Enable telephone-line
(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-lhsi
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode 1))

;; Add markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; elpher
(use-package elpher
  :ensure t
  :commands elpher)

;; rainbow-mode
(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
