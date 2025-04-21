;; (setq inhibit-startup-message t) ;remove starting screen

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; Path Variables for better maintainance
(setq local-ditaa-path "C:\\Programms\\ditaa\\ditaa0_9.jar")
(setq local-ditaa-eps-path  "C:\\Programms\\ditaa\\DitaaEps.jar")
(setq local-langtool-path "C:/Programms/LanguageTool-6.3/languagetool-commandline.jar")
(setq local-reveal-js-path "file:///C:/Programms/reveal.js")
(setq local-org-roam-db-path "G:/Dokumente/MemoryPalace/")

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
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
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

 ;; Hideshow to collapse code blocks
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c <right>") 'hs-show-block)
(global-set-key (kbd "C-c <left>") 'hs-hide-block)

;;CMake mode
(setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
(use-package cmake-mode
  :ensure t)

;;eglot
(use-package eglot
  :ensure t
  :hook
  (c-mode      . eglot-ensure)
  (c++-mode    . eglot-ensure)
  (csharp-mode . eglot-ensure)
  :custom
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '(csharp-mode . ("OmniSharp.exe" "-lsp"))))

;;dart-mode
;; (use-package dart-mode
;;   :ensure t)
;; ;;flutter
;; (use-package flutter
;;   :ensure t
;;   :after dart-mode)
;; ;;dart eglot cpnfig
;; (add-to-list 'eglot-server-programs '(dart-mode . ("dart" "language-server")))
;; (add-hook 'dart-mode-hook 'eglot-ensure)

;;Yaml-mode
(use-package yaml-mode
  :ensure t)

;; Markdown-mode
(use-package markdown-mode
  :ensure t)

(use-package move-text
  :ensure t)
(move-text-default-bindings)

;; Other Hooks for Programming
(use-package company
  :ensure t
  :hook
  (c-mode . company-mode)
  (c++-mode . company-mode)
  (csharp-mode . company-mode)
  (cmake-mode . company-mode)
  (dart-mode . company-mode))

;; Flymake-languagetool API access
(use-package flymake-languagetool
  :ensure t
  :hook ((text-mode       . flymake-languagetool-load)
         (LaTeX-mode      . flymake-languagetool-load)
         (org-mode        . flymake-languagetool-load)
         (markdown-mode   . flymake-languagetool-load))
  :custom
  ;; LanguageTools API Remote Server Configuration
  (flymake-languagetool-server-jar nil)
  (flymake-languagetool-url "https://api.languagetool.org"))

;;Language Tool
(use-package langtool
  :ensure t
  :cutom
  (langtool-language-tool-jar local-langtool-path)
  (langtool-default-language "en-GB"))


;; Auctex
(use-package auctex
  :ensure t
  :custom
  (TeX-PDF-mode t)
  (TeX-parse-self t) ; Enable parse on load.
  (TeX-auto-save t) ; Enable parse on save.
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


;; Org-mode Color Highlights
(setq org-emphasis-alist
  '(("*" (bold :foreground "red" ))
    ("/" italic)
    ("_" underline)
    ("=" (:background "maroon" :foreground "white"))
    ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
    ("+" (:strike-through t))))

(global-set-key (kbd "C-c a") #'org-agenda)

 ;; Org-roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-v2-ack t)
  (org-roam-completion-everywhere t)
  (org-roam-directory local-org-roam-db-path)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("v" "vocabulary" plain "\n* Translation \n\n** English \n\n** German
\n\n* Location(Loci)\n\n* Additional Comments"
      :target (file+head "%<vocab>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("g" "grammar" plain "\n* Explanation \n\n* Usage \n\n* Loci \n\n* Additional Comments"
      :target (file+head "%<grammar>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

;; Org-roam-ui
(use-package org-roam-ui
  :ensure t)

;; Magit
;; (use-package magit
;;   :ensure t)

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

#+end_src")
  (backward-char))

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

(defun my/revealjs-htmlblock ()
  (interactive)
  (insert " #+BEGIN_EXPORT html
<div>
</div>
#+END_EXPORT ")
  (backward-char))

(global-set-key (kbd "M-# M-1") #'my/latex-org-export)
(global-set-key (kbd "M-# M-2") #'my/source-block)
(global-set-key (kbd "M-# M-3") #'my/revealjs-export)
(global-set-key (kbd "M-# M-4") #'my/revealjs-htmlblock)

(use-package org-re-reveal
  :ensure t
  :custom
  (org-re-reveal-root local-reveal-js-path))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(kanagawa))
 '(custom-safe-themes
   '("7e0dec7d9f3821acba56ab19083ed059a94753ed79ee89b0d8167b727ed6cb81" default))
 '(org-agenda-files '("d:/Uni/CTS/CTS5/CG/CG.org"))
 '(package-selected-packages
   '(move-text markdown-mode org-roam-ui org-roam flymake-languagetool yaml-mode org-re-reveal kanagawa-theme langtool auctex company cmake-mode command-log-mode))
 '(safe-local-variable-values
   '((org-roam-directory . "G:/Dokumente/MemoryPalace/")
     (org-roam-db-location . "./org-roam.db")
     (org-roam-directory . ".")
     (org-roam-db-location . "G:/Dokumente/MemoryPalace/org-roam.db")
     (org-roam-directory . "G:/Dokumente/MemoryPalace")))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((use-package) (use-package) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
