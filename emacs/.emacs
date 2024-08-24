;;(setq inhibit-startup-message t) ;remove starting screen

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Path Variables for better maintainance
(setq local-ditaa-path "C:\\Programms\\ditaa\\ditaa0_9.jar")
(setq local-ditaa-eps-path  "C:\\Programms\\ditaa\\DitaaEps.jar")
(setq local-langtool-path "D:/Programme/LanguageTool-6.3/languagetool-commandline.jar")
(setq local-reveal-js-path "file:///D:/Programme/reveal.js")

;; Abbreviations
(setq-default abbrev-mode t)
(define-abbrev global-abbrev-table "$SetupPath$" "C:/Users/Simon/AppData/Roaming/.config/emacs/setup.org")

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(set-default-coding-systems 'utf-8) ; Default coding system

;;Improve Scroling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;Set frame transparency and maximize windows by default.
(set-frame-parameter (selected-frame) 'alpha '(99 . 99))
(add-to-list 'default-frame-alist '(alpha . (99 . 99)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)

;; Don't create backupfiles
(setq make-backup-files nil)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Messure Starup Time.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;;CMake mode
(setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
(use-package cmake-mode)
;; Dart-mode
(use-package dart-mode)

;;eglot
(use-package eglot
  :ensure t)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

 ;;dart-mode
(use-package dart-mode
  :ensure t)
;;flutter
(use-package flutter
  :ensure t
  :after dart-mode)
;;dart eglot cpnfig
(add-to-list 'eglot-server-programs '(dart-mode . ("dart" "language-server")))
(add-hook 'dart-mode-hook 'eglot-ensure)

;; Other Hooks for Programming
(use-package company
  :ensure t
  :hook
  (c-mode . company-mode)
  (c++-mode . company-mode)
  (cmake-mode . company-mode)
  (dart-mode . company-mode))


;; Flymake-languagetool API access
(use-package flymake-languagetool
  :ensure t
  :hook ((text-mode       . flymake-languagetool-load)
         (latex-mode      . flymake-languagetool-load)
         (org-mode        . flymake-languagetool-load)
         (markdown-mode   . flymake-languagetool-load))
  :init
  ;; LanguageTools API Remote Server Configuration
  (setq flymake-languagetool-server-jar nil)
  (setq flymake-languagetool-url "https://api.languagetool.org"))

;;Language Tool
(use-package langtool
  :ensure t
  :init
  (setq langtool-language-tool-jar local-langtool-path)
  (setq langtool-default-language "en-GB"))

;; Auctex
(use-package auctex
  :ensure t
  :config
  (setq TeX-PDF-mode t)
  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.
  :hook
  (LaTeX-mode . turn-on-prettify-symbols-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-math-mode . company-mode))

;; Org-mode latex Exportation
(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-listings 't)
(setq org-latex-title-command "\\maketitle \\newpage")
(setq org-latex-toc-command "\\tableofcontents \\newpage")

;; Magit
;;(require 'package)
;;(add-to-list 'package-archives
;;             '("melpa" . "https://melpa.org/packages/") t)

;; Custom Keysets
(global-set-key (kbd "C-c a") #'org-agenda)

;; Recent files
(use-package recentf
  :init
  (setq recentf-max-saved-items 200
	recentf-max-menu-items 15)
  (recentf-mode +1)
  :hook
  (after-init . recentf-open-files))
(global-set-key (kbd "C-c r") #'recentf-open-files)
;;(initial-buffer-choice 'recentf-open-files)

;; Insert Text Snippet for Org-mode Latex Export
(defun my/latex-org-export ()
  (interactive)
  (insert "#+TITLE:
#+SUBTITLE: Summer Semester 2024
#+AUTHOR: Simon Engel
#+SETUPFILE: $SetupPath$")
  (backward-char))

(defun my/source-block ()
  (interactive)
  (insert " #+begin_src c++

#+end_src") (backward-char))

(defun my/revealjs-export ()
  (interactive)
  (insert "#+OPTIONS: num:nil  timestamp:nil toc:nil
#+REVEAL_TRANS: None
#+REVEAL_THEME: dracula
# #+REVEAL_MARGIN: 0.3
#+REVEAL_MIN_SCALE: -0.5
# #+REVEAL_EXTRA_CSS: ./presentation.css
#+Title: 
#+Author: Simon Engel")
  (backward-char))

(global-set-key (kbd "M-# M-1") #'my/latex-org-export)
(global-set-key (kbd "M-# M-2") #'my/source-block)
(global-set-key (kbd "M-# M-3") #'my/revealjs-export)

(use-package org-re-reveal
  :ensure t
  :init
  (setq org-re-reveal-root local-reveal-js-path) )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(kanagawa))
 '(custom-safe-themes
   '("380763a0ed87b880b905c604bf0c2925b767b43ffe42fb70048f33ffd2349ceb" default))
 '(org-agenda-files '("d:/Uni/CTS/CTS5/CG/CG.org"))
 '(package-selected-packages
   '(flutter dart-mode ty flymake-languagetool magit kanagawa-theme org-re-reveal use-package ox-reveal langtool auctex company cmake-mode kaolin-themes command-log-mode))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
