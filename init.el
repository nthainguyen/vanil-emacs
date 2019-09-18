(setq-default gc-cons-threshold 100000000)
(let ((file-name-handler-alist nil)) "init.el")
(defun my--tangle-byte-compile-org ()
 "Tangles emacs.org and byte compiles ~/.emacs.d/"
   (interactive)
   (when (equal (buffer-name)
                (concat "init.org"))
     (org-babel-tangle)
     (byte-recompile-directory (expand-file-name user-emacs-directory) 0)))

(defun my--tangle-org ()
 "Tangles emacs.org and byte compiles ~/.emacs.d/"
   (interactive)
   (when (equal (buffer-name)
                (concat "init.org"))
     (org-babel-tangle)))
(add-hook 'after-save-hook #'my--tangle-org)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                 (not (gnutls-available-p))))
    (proto (if no-ssl "http" "https")))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("org" . (concat proto "://orgmode.org/elpa/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

(use-package gcmh
:ensure t)
(gcmh-mode)
(setq garbage-collection-messages t)
  (use-package kaolin-themes
  :config
  (load-theme 'kaolin-ocean t))
  (use-package spaceline
    :ensure t
    :config
    (require 'spaceline-config)
    (spaceline-spacemacs-theme))  
  (use-package spaceline
    :ensure t
    :config
    (require 'spaceline-config)
    (spaceline-spacemacs-theme))
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-fontify-natively t)
  (setq make-backup-files nil) ; stop creating backup~ files
  (setq auto-save-default nil) ; stop creating #autosave# files
  (setq create-lockfiles nil)  ; stop creating .# files
  (global-auto-revert-mode t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (transient-mark-mode 1)
  (delete-selection-mode 1)
  (global-unset-key (kbd "s-p"))
  (global-hl-line-mode 0)
  (add-hook 'org-capture-mode-hook #'visual-line-mode)
  (global-display-line-numbers-mode)
  (setq
   cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows
   inhibit-splash-screen t
   default-frame-alist '((font . "Source Code Pro-11"))
   display-line-numbers-type 'relative
   echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffe
   initial-scratch-message nil       ; Empty scratch buffer
   initial-major-mode 'org-mode      ; org mode by default
   sentence-end-double-space nil     ; Sentences should end in one space, come on!
   confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
  )
  (fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
  (setq scroll-margin 10
     scroll-step 1
     next-line-add-newlines nil
     scroll-conservatively 10000
     scroll-preserve-screen-position 1)
  (setq mouse-wheel-follow-mouse 't)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq custom-safe-themes t)

(use-package winum :ensure t
  :defer t
:config
(setq winum-auto-setup-mode-line nil)
(winum-mode t))

  (use-package general
    :ensure t
    :config
    (general-evil-setup t))

  (use-package which-key :ensure t
    :config
    (which-key-mode)
    (which-key-setup-minibuffer))

(setq inhibit-compacting-font-caches t)
(setq url-proxy-services '((("no_proxy"
      . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
	  ("http"     . "access614.cws.sco.cisco.com:8080")
        ("https"    . "access614.cws.sco.cisco.com:8080"))))

  (use-package ace-window :ensure t)
  (use-package ivy
    :ensure t
    :diminish ivy-mode
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq enable-recursive-minibuffers t)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t      . ivy--regex-fuzzy))))
  (use-package counsel
    :ensure t
    :diminish counsel-mode
    :config
    (counsel-mode 1))
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode))
  (use-package smex :ensure t)
  (use-package flx :ensure t)
  (use-package avy :ensure t)

  (use-package yasnippet :ensure t
      :config
    (use-package yasnippet-snippets
      :ensure t)
    (setq yas-snippet-dirs
      '("~/.emacs.d/snippets")))

(general-create-definer my-leader-def :prefix "SPC")
(general-create-definer my-local-leader-def :prefix ",")
(general-define-key
 :keymaps 'global
 :states '(emacs insert normal motion)
 "C-f" 'swiper
 "C-s" 'save-buffer
 "C-w" 'delete-other-windows)
(my-leader-def 'normal 
 "SPC" 'counsel-M-x
 "d"   'counsel-bookmark
 "1"   'winum-select-window-1
 "2"   'winum-select-window-2
 "3"   'winum-select-window-3
 "4"   'winum-select-window-4
 "5"   'winum-select-window-5
 "6"   'winum-select-window-6
 "7"   'winum-select-window-7
 "8"   'winum-select-window-8
 "y"   'counsel-yank-pop
 "p"   'projectile-command-map
 "d"   'deadgrep
 "m"   'magit-status
 "TAB" '(switch-to-next-buffer :which-key "prev buffer")
 "f"   '(:ignore t :which-key "files")
 "ff"  'counsel-find-file
 "fr"  'counsel-recentf
 "a"   '(:ignore t :which-key "Applications")
 "ad"  '(:ignore t :which-key "zetteldeft")
 "add" 'deft
 "adf" 'counsel-find-file
 "adn" 'zd-new-file
 "adN" 'zd-new-file-and-link
 "adt" 'zd-avy-tag-search
 "adf" 'zd-follow-link
 "adF" 'zd-get-thing-at-point
 "adr" 'zd-file-rename
 "ao"  '(:ignore t :which-key "Org mode")
 "aon" '(org-add-note :wk "Create Note")
 "aoc" '(org-capture :which-key "Capture")
 ;; Buffer
 "b"   '(:ignore t :which-key "Buffer")
 "bb"  '(ivy-switch-buffer :which-key "Change buffer")  ; change buffer, chose using ivy
 "bs"  '(save-buffer :which-key "Save buffer")
 "bS"  '(save-some-buffers :which-key "save all buffer")
 "be"  '(eval-buffer :wk "evaluate buffer")
 "q"   '(:ignore t :which-key "quick open file")
 "qi"  '((lambda() (interactive)(find-file "~/.emacs.d/init.el")) :which-key "init")
 "qo"  '((lambda() (interactive)(find-file "~/.emacs.d/init.org")) :which-key "init")
)

    (use-package deft
      :defer t
      :commands (deft)
      :general
      (my-local-leader-def 'normal deft-mode-map
        "f" 'counsel-find-file
	"n" 'zd-new-file
	"N" 'zd-new-file-and-link
	"t" 'zd-avy-tag-search
	"f" 'zd-follow-link
	"F" 'zd-get-thing-at-point
	"r" 'zd-file-rename)
      :init (setq deft-directory "~/Dropbox/Archives"
                    deft-text-mode 'org-mode
                    deft-extensions '("org")
                    deft-recursive t
                    deft-use-filename-as-title nil))
   (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  (setq deft-org-mode-title-prefix t)
  (use-package zetteldeft
    :load-path "~/.emacs.d/zetteldeft/")
  (use-package helm-org-rifle
    :ensure t)
  (defun hai/helm-org-rifle-archives ()
    "Rifle through Archives folder"
    (interactive)
    (helm-org-rifle-directories "~/Dropbox/Archives"))
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (use-package org-noter
	:defer t
    :ensure t)
  (pdf-tools-install)
  (use-package helm-ag
	:defer t
    :ensure t)

(use-package evil
  :ensure t
  :config
  (general-evil-setup t)
  (evil-mode t))



  (use-package deadgrep
    :ensure t
    :defer t)

  (use-package magit
    :ensure t
    :defer t)

  (use-package smartparens
    :ensure t
	:defer t
    :config
    (add-hook 'lisp-mode-hook #'smartparens-mode)
    (add-hook 'python-mode-hook #'smartparens-mode)
    (add-hook 'org-mode-hook #'smartparens-mode))
  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
  conses, where NAME is the function name that will be created and
  STRING is a single-character string that marks the opening character.

    (def-pairs ((paren . \"(\")
		(bracket . \"[\"))

  defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
  respectively."
    `(progn
       ,@(loop for (key . val) in pairs
	       collect
	       `(defun ,(read (concat
			       "wrap-with-"
			       (prin1-to-string key)
			       "s"))
		    (&optional arg)
		  (interactive "p")
		  (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren . "(")
	      (bracket . "[")
	      (brace . "{")
	      (single-quote . "'")
	      (double-quote . "\"")
	      (back-quote . "`")))
(general-define-key
 :keymap 'smartparens-mode-map
 "C-c ("   'wrap-with-parens
 "C-c ["   'wrap-with-brackets
 "C-c {"   'wrap-with-braces
 "C-c '"   'wrap-with-single-quotes
 "C-c \""  'wrap-with-double-quotes
 "C-c _"   'wrap-with-underscores
 "C-c `"   'wrap-with-back-quotes)

(use-package diminish
 :ensure t)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))
(load-user-file "orgfile.el")

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package lsp-mode
  :hook (python-mode . lsp)
  :commands lsp)
(setq lsp-auto-configure nil)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(gcmh-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package company
  :ensure t
  :hook 'python-mode
  :config
(setq company-minimum-prefix-length 1
      company-idle-delay 0.1
      company-tooltip-limit 14
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-dabbrev-code-other-buffers t
      company-tooltip-align-annotations t
      company-require-match 'never
      company-lsp-cache-candidates 'auto))

(set-face-attribute 'org-block-begin-line nil :slant
 'normal :background nil)

(use-package evil-matchit
  :ensure t)
(use-package rainbow-delimiters :ensure t)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(defun hai/deft-new-file-named (slug string)
  "Create a new file named SLUG.
SLUG is the short file name, without a path or a file extension."
  (interactive "New filename (without extension): ")
  (let ((file (deft-absolute-filename slug)))
    (if (file-exists-p file)
        (message "Aborting, file already exists: %s" file)
      (deft-auto-populate-title-maybe file)
      (deft-cache-update-file file)
      (deft-refresh-filter)
      (write-region string nil file)
      )))


(defun hai/zd-new-file (str &optional empty)
  "Create a new deft file.
Filename is `zd-id-format' appended by STR.
No file extension needed.

The title is inserted in `org-mode' format (unless EMPTY is true)
and the file name (without extension) is added to the kill ring.
When `evil' is loaded, enter instert state."
(interactive "P")
(save-excursion
  (let*  ((zdstr (org-get-heading))
         (zdId (zd-generate-id))
         (zdName (concat zdId " " zdstr)))
  (org-copy-subtree)
  (append-to-file (concat "#+TITLE: " (zd-lift-file-title (deft-absolute-filename zdName)) "\n") nil (deft-absolute-filename zdName))
  (append-to-file org-subtree-clip nil (deft-absolute-filename zdName))
  )))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)
