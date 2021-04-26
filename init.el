;;; init.el --- my own emacs init.el file specifically for use in docker containers

;;; Commentary:
;;
;;; Initialize:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(auctex sphinx-doc company-box visual-fill-column org org-bullets sr-speedbar elpy flycheck blacken which-key try use-package pandoc-mode pandoc markdown-mode))
 '(show-paren-mode t)
 '(text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

(load-theme 'dracula t)

;; ---------------
;; --- my personal settings
;; ---------------

;; --- satter lite kortbindningar --------------------------
;; ---------------
(global-set-key "\M-1" 'other-window)
(global-set-key "\C-cq" 'query-replace)
(global-set-key "\C-ca" 'mark-whole-buffer)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-cw" 'ispell-word)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cl" 'count-lines-region)
(global-set-key "\C-cd" 'delete-trailing-whitespace)
(global-set-key "\C-cp" 'org-toggle-inline-images)


;; --- tyst *scratch* buffer och inget pling ---------------
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)

;; --- oh! the blank spaces! -------------------------------
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; --- melpa packages settings -----------------------------
;; ----------------
(require 'package)
;; (setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; --- the packages

;; --- simple stuff ----------------------------------------
(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))


;; --- python help stuff -----------------------------------
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
  :custom
  (python-shell-interpreter "python3"))


(add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)))


;; --- markdown and pandoc ---------------------------------
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(add-hook 'markdown-mode-hook 'pandoc-mode)

(use-package pandoc
  :ensure t)
(use-package pandoc-mode
  :ensure t)


;; --- the fantastic speedbar ------------------------------
(use-package sr-speedbar
  :ensure t)
;;(require 'sr-speedbar)
(global-set-key "\C-cs" 'sr-speedbar-toggle)
(setq speedbar-use-images nil)
(setq sr-speedbar-refresh-turn-on t)


;; ----------------
;; Org Mode Configuration ----------------------------------
;; ----------------

(defun ns/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1)
  )

;; (defun efs/org-font-setup ()
;;   ;; Replace list hyphen with dot
;;   (font-lock-add-keywords 'org-mode
;;                           '(("^ *\\([-]\\) "
;;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    ;; (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
    )

  ;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . ns/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-todo-keywords
  '((sequence "TODO" "DOING" "DONE")))
  (setq org-todo-keyword-faces
 '(("TODO" . "red3") ("DOING" . "magenta3") ("DONE" . "green3")))
   ;; (efs/org-font-setup)
)

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(defun ns/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package visual-fill-column
  :ensure t
  :hook ((org-mode . ns/org-mode-visual-fill)
        (markdown-mode . ns/org-mode-visual-fill))
  )


;; --- END org-mode

;; --- company stuff ---------------------------------------
(use-package company
  :after elpy-mode
  :hook (elpy-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map elpy-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )


;; ;; --- some colors for company mode when dark
;; (require 'color)

;; (custom-theme-set-variables
;;  ;; company-mode
;;  ;;
;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  '(company-tooltip                  ((t (:inherit nil :background "grey23" :foreground "white")))))


 ;; (let ((bg (face-attribute 'default :background)))
 ;;    (custom-set-faces
 ;;     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
 ;;     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
 ;;     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
 ;;     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 ;;     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; company mode global
(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode t)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)
;; company quick-help
;; (company-quickhelp-mode 1)
;; (setq company-quickhelp-delay 0)

;; ---

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; ---
;; ---
;; ---
