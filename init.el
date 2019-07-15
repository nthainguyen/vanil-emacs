(eval-when-compile
  (require 'use-package))
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

(setq org-refile-use-outline-path 'file)
(setq ivy-display-style 'fancy)
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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

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
      :defer t
      :config
    (use-package yasnippet-snippets
      :ensure t)
    (setq yas-snippet-dirs
      '("~/.emacs.d/snippets")))

    (use-package deft
      :bind ("C-x d" . deft)
	  :defer t
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

(general-nmap "RET" (general-key "C-c C-c"))
(general-imap "n"
              (general-key-dispatch 'self-insert-command
		:timeout 0.25
              "e" 'evil-normal-state))
(general-def :states '(normal motion) "SPC" nil)
(general-define-key
 :keymaps 'global
 :states '(emacs insert normal motion)
 "C-f" 'swiper
 "C-s" 'save-buffer
 "C-w" 'delete-other-windows
 "C-x C-f" 'counsel-find-file
 "C-c z n" 'zd-new-file
 "C-c z N" 'zd-new-file-and-link
 "C-c z t" 'zd-avy-tag-search
 "C-c z f" 'zd-follow-link
 "C-c z F" 'zd-get-thing-at-point
 "C-c z r" 'zd-file-rename)
(general-define-key
 :keymaps 'global
 :states '(normal motion visual)
 :prefix "SPC"
 ""    nil
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
 "az"  '(:ignore t :which-key "zetteldeft")
 "af" 'counsel-find-file
 "an" 'zd-new-file
 "aN" 'zd-new-file-and-link
 "at" 'zd-avy-tag-search
 "af" 'zd-follow-link
 "aF" 'zd-get-thing-at-point
 "ar" 'zd-file-rename
 "ao"  '(:ignore t :which-key "Org mode")
 "aon" '(org-add-note :wk "Create Note")
 "aoc" '(counsel-org-capture :which-key "Capture")
 "aoa" 'air-pop-to-org-agenda
 "w"   '(hydra-frame-window/body :wk "Windows")
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

(use-package lsp-mode
  :hook (python-mode . lsp)
  :commands lsp)
(setq lsp-auto-configure nil)
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred
;; optionally
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
(with-eval-after-load 'org
  (defvar-local rasmus/org-at-src-begin -1
    "Variable that holds whether last position was a ")

  (defvar rasmus/ob-header-symbol ?☰
    "Symbol used for babel headers")

  (defun rasmus/org-prettify-src--update ()
    (let ((case-fold-search t)
          (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
          found)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (goto-char (match-end 0))
          (let ((args (org-trim
                       (buffer-substring-no-properties (point)
                                                       (line-end-position)))))
            (when (org-string-nw-p args)
              (let ((new-cell (cons args rasmus/ob-header-symbol)))
                (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
                (cl-pushnew new-cell found :test #'equal)))))
        (setq prettify-symbols-alist
              (cl-set-difference prettify-symbols-alist
                                 (cl-set-difference
                                  (cl-remove-if-not
                                   (lambda (elm)
                                     (eq (cdr elm) rasmus/ob-header-symbol))
                                   prettify-symbols-alist)
                                  found :test #'equal)))
        ;; Clean up old font-lock-keywords.
        (font-lock-remove-keywords nil prettify-symbols--keywords)
        (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (while (re-search-forward re nil t)
          (font-lock-flush (line-beginning-position) (line-end-position))))))

  (defun rasmus/org-prettify-src ()
    "Hide src options via `prettify-symbols-mode'.

  `prettify-symbols-mode' is used because it has uncollpasing. It's
  may not be efficient."
    (let* ((case-fold-search t)
           (at-src-block (save-excursion
                           (beginning-of-line)
                           (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
      ;; Test if we moved out of a block.
      (when (or (and rasmus/org-at-src-begin
                     (not at-src-block))
                ;; File was just opened.
                (eq rasmus/org-at-src-begin -1))
        (rasmus/org-prettify-src--update))
      ;; Remove composition if at line; doesn't work properly.
      ;; (when at-src-block
      ;;   (with-silent-modifications
      ;;     (remove-text-properties (match-end 0)
      ;;                             (1+ (line-end-position))
      ;;                             '(composition))))
      (setq rasmus/org-at-src-begin at-src-block)))

  (defun rasmus/org-prettify-symbols ()
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
          (cl-reduce 'append
                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                             `(("#+begin_src" . ?✎) ;; ➤ 🖝 ➟ ➤ ✎
                               ("#+end_src"   . ?✎) ;; ⏹
                               ("#+header:" . ,rasmus/ob-header-symbol)
                               ("#+begin_quote" . ?»)
                               ("#+end_quote" . ?«)))))
    (turn-on-prettify-symbols-mode)
    (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))
  (add-hook 'org-mode-hook #'rasmus/org-prettify-symbols))
