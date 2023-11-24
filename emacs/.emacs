;;(setq inhibit-startup-message t) ;remove starting screen

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

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
(require 'cmake-mode)

;;eglot
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Other Hooks for Programming
(add-hook 'c-mode-hook #'company-mode)
(add-hook 'c++-mode-hook #'company-mode)
(add-hook 'cmake-mode-hook #'company-mode)

;;Language Tool
(setq langtool-language-tool-jar "D:/Programme/LanguageTool-6.3/languagetool-commandline.jar")
(require 'langtool)
(require 'langtool-popup)
(setq langtool-default-language "en-GB")

;; Auctex
(use-package auctex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-prettify-symbols-mode))

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

;;DOOM Modeline
;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1))

;; Custom Keysets
(global-set-key (kbd "C-c a") #'org-agenda)

;; Insert Text Snippet for Org-mode Latex Export
(defun my/latex-org-export ()
  (interactive)
  (insert "#+TITLE:
#+SUBTITLE: Winter Semester 2023/24
#+AUTHOR: Simon Engel
#+SETUPFILE: D:/Programme/.config/emacs/setup.org")
  (backward-char))

(global-set-key (kbd "M-#") #'my/latex-org-export)

;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)
(add-hook 'after-init-hook 'recentf-open-files)
;;(initial-buffer-choice 'recentf-open-files)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(kaolin-shiva))
 '(custom-safe-themes
   '("be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "3de5c795291a145452aeb961b1151e63ef1cb9565e3cdbd10521582b5fd02e9a" default))
 '(org-agenda-files '("d:/Uni/CTS/CTS5/CG/CG.org"))
 '(package-selected-packages
   '(langtool-popup langtool auctex company cmake-mode kaolin-themes doom-themes command-log-mode))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
