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

  (use-package nimbus-theme)
  (nimbus-theme)
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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

(use-package winum :ensure t
:config
(winum-mode t))

  (use-package general
    :ensure t)

  (use-package which-key :ensure t
    :config
    (which-key-mode)
    (which-key-setup-minibuffer))

(setq url-proxy-services
      '(("http"     . "access614.cws.sco.cisco.com:8080")
        ("https"    . "access614.cws.sco.cisco.com:8080")))
(setq inhibit-compacting-font-caches t)

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
    :init
    (yas-global-mode 1)
    :config
    (use-package yasnippet-snippets
      :ensure t)
    (setq yas-snippet-dirs
      '("~/.emacs.d/snippets")))

     (use-package pyvenv :ensure t)
     (setq exec-path (append exec-path '("C:\\Users\\VNHANGU19\\AppData\\Local\\Programs\\Python\\Python37-32"))) 
     (defun spacemacs//pyvenv-mode-set-local-virtualenv ()
       "Set pyvenv virtualenv from \".venv\" by looking in parent directories."
       (interactive)
       (let ((root-path (locate-dominating-file default-directory
						"venv")))
	 (when root-path
	   (let* ((file-path (expand-file-name "venv" root-path))
		  (virtualenv
		   (with-temp-buffer
		     (insert-file-contents-literally file-path)
		     (buffer-substring-no-properties (line-beginning-position)
						     (line-end-position)))))
		 (pyvenv-workon virtualenv)))))

    (use-package deft
      :bind ("C-x d" . deft)
      :commands (deft)
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
    :load-path "~/.emacs.d/zetteldeft/"
    :after deft)
  (use-package helm-org-rifle
    :ensure t)
  (defun hai/helm-org-rifle-archives ()
    "Rifle through Archives folder"
    (interactive)
    (helm-org-rifle-directories "~/Dropbox/Archives"))
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (use-package org-noter
    :ensure t)
  (pdf-tools-install)
  (use-package helm-ag
    :ensure t)

  (use-package dap-mode
    :ensure t
    )
  (require 'dap-python)
  (setq dap-python-executable "urxvt -hold -e python")
  (add-hook 'python-mode-hook #'dap-ui-mode)

  (defun air-pop-to-org-agenda (split)
    "Visit the org agenda, in the current window or a SPLIT."
    (interactive "P")
    (org-agenda)
    (when (not split)
      (delete-other-windows)))

  (use-package org
    :init
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    :config
  (setq org-use-speed-commands t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-directory "~/Nextcloud/gtd")
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-protocol)
  (require 'org-protocol)
  (setq org-default-notes-file "~/Nextcloud/gtd/inbox.org")
  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "CAPTURE" (frame-parameter nil 'name))
	(delete-frame)))
  (add-hook 'org-mode-hook 'visual-line-mode)
  :general
  (:keymaps 'org-agenda-mode-map
   :states  '(normal emacs)
   :prefix ","
   "RET"    'org-agenda-switch-to
   "ESC"    'org-agenda-kill
   "q"      'org-agenda-quit
   "m"      'org-agenda-month-view
   "r"      'org-agenda-refile
   "f"      'org-agenda-filter-by-tag)

  (:keymaps 'org-mode-map
   :states  '(normal emacs)
   :prefix ","
      "c" '(org-capture :which-key "Capture")
      "r" '(:ignore t :which-key "refile templates")
      "rg"'((lambda() (interactive)(my/refile "gtd.org" "GTD")) :which-key "GTD")
      "R" '(org-refile :which-key "refile without template")
      ","  "C-c C-c"
      "a" '(org-archive-subtree-default :which-key "Archive this")
      ;;"A" '(org-agenda :which-key "Agenda")
      "l" "C-c C->"
      "h" "C-c C-<"
      "T" 'org-todo
      "t" 'org-set-tags
      "p" '(:ignore t :wk "link")
      "pl"'(org-store-link :wk "create link and save")
      "pi"'(org-insert-link :wk "insert link")
      "po"'(org-open-at-point :wk "open link")
      "A" 'air-pop-to-org-agenda
      "n" '(:ignore t :wk "Narrow")
      "nb"'org-narrow-to-block
      "nt"'org-narrow-to-subtree
      "ni" 'narrow-or-widen-dwim
      "nw" 'widen
      "ne"'org-narrow-to-element
  ))

  (general-define-key
   :keymap 'globals
   "C-f" 'swiper
   "C-s" 'save-buffer
   "C-r" nil
   "C-c p" 'projectile-command-map
   "C-x C-f" 'counsel-find-file
   "C-c d" 'deadgrep
   "C-x m" 'magit-status)
  (general-define-key
   :keymap 'globals
   "C-c z n" 'zd-new-file
   "C-c z N" 'zd-new-file-and-link
   "C-c z t" 'zd-avy-tag-search
   "C-c z f" 'zd-follow-link
   "C-c z F" 'zd-get-thing-at-point
   "C-c z r" 'zd-file-rename)

  (use-package deadgrep
    :ensure t
    :defer t)

  (use-package magit
    :ensure t
    :defer t)

  (use-package smartparens
    :ensure t
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

(use-package hyperbole
  :ensure t)
(setq org-refile-use-outline-path 'file)
