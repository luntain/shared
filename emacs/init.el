(require 'package)

(setq package-archives
      '(;  ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa"     . "http://melpa.org/packages/")
        ; ("marmalade" . "http://marmalade-repo.org/packages/")
        ("gnu"       . "http://elpa.gnu.org/packages/")
        ))


(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package hindent
  :init ; init - to execute before the package is loaded
  (setq hindent-extra-args '("--line-length" "120"))
  :ensure t)

(use-package projectile
  :ensure t)

(use-package hydra :ensure t)

(use-package exec-path-from-shell :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(add-to-list 'completion-ignored-extensions ".exe")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-dispatch-always t)
 '(completion-pcm-word-delimiters "-./:| ")
 '(custom-safe-themes
   (quote
    ("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain) t)
 '(electric-pair-mode t)
 '(evil-shift-width 2)
 '(evil-symbol-word-search t)
 '(fiplr-ignored-globs
   (quote
    ((directories
      (".git" ".svn" ".hg" ".bzr" ".stack-work" ".gitmodules"))
     (files
      (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip")))))
 '(flycheck-disabled-checkers (quote (haskell-ghc)))
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-doc interactive-haskell-mode flycheck-mode)))
 '(haskell-notify-p t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(hyai-basic-offset 2)
 '(indent-tabs-mode nil)
 '(lsp-haskell-process-args-hie nil)
 '(package-selected-packages
   (quote
    (lsp-haskell lsp-ui lsp-mode smartparens hydra dumb-jump projectile hyai haskell-mode smex simp ido-vertical-mode ido-completing-read+ flx-ido evil-surround company-ghc company-flx coffee-mode ace-jump-mode)))
 '(projectile-enable-caching nil)
 '(reb-re-syntax (quote rx))
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 500)
 '(save-place-file "~/.emacs.d/emacs-places")
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(smex-save-file "~/.emacs.d/smex-items")
 '(split-height-threshold nil)
 '(tab-always-indent nil)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(visible-bell t))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;(ido-ubiquitous-mode t) -- usepackage?
(use-package ido-completing-read+
 :ensure t
 :init
  (setq ido-cr+-max-items 50000)
  (setq ido-default-buffer-method (quote selected-window))
  (setq ido-default-file-method (quote selected-window))
  (setq ido-enable-flex-matching t)
  (setq ido-ubiquitous-max-items 50000)
  (setq ido-use-virtual-buffers t)
  (setq ido-show-dot-for-dired nil)
  (setq ido-create-new-buffer 'always)
  (setq ido-use-filename-at-point nil)
  (setq ido-use-virtual-buffers t)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (ido-mode 'both)
 :config
  (ido-ubiquitous-mode 1)
)

(use-package smex :ensure t)

(use-package ido-vertical-mode :ensure t
 :config
 (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
)

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t) ; evil-want-C-u-scroll needs to be set before 'evil is loaded

  ; The following snippet will make Evil treat an Emacs symbol as a word.
  ; This has the advantage that it changes depending on the language:
  ; `foo-bar` is one symbol in lisp-mode but two symbols in c-mode
  ; I am surprised that it works for move back a word or deleting a word
  :config
  (require 'evil-surround)
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (define-key evil-normal-state-map "s" 'evil-surround-edit)
  (define-key evil-normal-state-map ",e" 'ido-find-file)
  (define-key evil-normal-state-map ",f" 'projectile-find-file)
  (define-key evil-normal-state-map ",b" 'ido-switch-buffer)
  (define-key evil-normal-state-map ",c" 'comment-dwim)
  (define-key evil-normal-state-map (kbd "C-l") 'delete-other-windows)
  (define-key evil-normal-state-map (kbd "gt") 'evil-goto-definition)

  (define-key evil-insert-state-map (kbd "C-l") 'evil-forward-char)
  (define-key evil-insert-state-map "\r" 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-o") 'evil-ret)
  (define-key evil-insert-state-map (kbd "C-k") 'smex)

  :ensure t)

;(use-package evil-easymotion
   ;:ensure t
   ;:config (evilem-default-keybindings "SPC"))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(evil-mode 1)

(use-package dumb-jump
  :ensure t
  :config
  (defhydra dumb-jump-hydra (:color red :columns 3)
      "Dumb Jump"
      ("j" dumb-jump-go "Go")
      ("o" dumb-jump-go-other-window "Other window")
      ("e" dumb-jump-go-prefer-external "Go external")
      ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
      ("i" dumb-jump-go-prompt "Prompt")
      ("l" dumb-jump-quick-look "Quick look")
      ("b" dumb-jump-back "Back"))
   (define-key evil-normal-state-map ",d" 'dumb-jump-hydra/body)
  )

(use-package ace-window
  :ensure t
  :config
  (define-key evil-normal-state-map ",a" 'ace-window)
  )

(use-package smartparens
  :ensure t
  :config
  (defhydra hydra-smartparens (:hint nil)
    "
  Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
  ------------------------------------------------------------------------------------------------------------------------
  [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
  [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
  [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
  [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
    ;; Moving
    ("a" sp-beginning-of-sexp)
    ("e" sp-end-of-sexp)
    ("f" sp-forward-sexp)
    ("b" sp-backward-sexp)
    ("n" sp-down-sexp)
    ("N" sp-backward-down-sexp)
    ("p" sp-up-sexp)
    ("P" sp-backward-up-sexp)

    ;; Slurping & barfing
    ("h" sp-backward-slurp-sexp)
    ("H" sp-backward-barf-sexp)
    ("l" sp-forward-slurp-sexp)
    ("L" sp-forward-barf-sexp)

    ;; Wrapping
    ("R" sp-rewrap-sexp)
    ("u" sp-unwrap-sexp)
    ("U" sp-backward-unwrap-sexp)
    ("(" sp-wrap-round)
    ("{" sp-wrap-curly)
    ("[" sp-wrap-square)

    ;; Sexp juggling
    ("S" sp-split-sexp)
    ("s" sp-splice-sexp)
    ("r" sp-raise-sexp)
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ("A" sp-absorb-sexp)
    ("E" sp-emit-sexp)
    ("o" sp-convolute-sexp)

    ;; Destructive editing
    ("c" sp-change-inner :exit t)
    ("C" sp-change-enclosing :exit t)
    ("k" sp-kill-sexp)
    ("K" sp-backward-kill-sexp)
    ("w" sp-copy-sexp)

    ("q" nil)
    ("g" nil))
   (define-key evil-normal-state-map ",s" 'hydra-smartparens/body)
)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-k") 'smex)

; (setq key-chord-two-key-delay 0.05)
;(require 'key-chord)
;(key-chord-mode 1)
;(key-chord-define-global "fj" 'smex)
;(key-chord-define-global "dk" 'evil-normal-state)
;(key-chord-define-global "df" 'delete-other-windows)
;(key-chord-define-global "jk" (lambda ()
;                                (interactive)
;                                (switch-to-buffer (other-buffer))))

(global-set-key (kbd "C-j") (lambda ()
                               (interactive)
                                (switch-to-buffer (other-buffer))))

(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-0") 'text-scale-adjust)

; remove this hook because it slows down opening files
(remove-hook 'find-file-hook 'vc-find-file-hook)

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))

(define-key evil-insert-state-map [tab] 'indent-or-expand)

(defun my-esc (prompt)
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
   ;; Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ;; Note: As long as I return [escape] in normal-state, I don't need this.
   ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))

(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "C-f") 'universal-argument)

;; The weird thing about this mapping is that it makes ESC to exit the minibuffer immediately
;; but c-[ needs to be pressed twice for the same effect. So in summary, it takes down the
;; number of ESC presses from 3 to 1, but c-[ from 3 to 2.
(define-key key-translation-map (kbd "C-[") 'my-esc)
(define-key key-translation-map (kbd "C-]") 'my-esc)

;; I don't know what the following is for:
;; Works around the fact that Evil uses read-event directly when in operator state, which
;; doesn't use the key-translation-map.
;;(define-key evil-operator-state-map (kbd "C-[") 'keyboard-quit)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#222" :foreground "white smoke" :height 140 :family "PragmataPro" :foundry "outline" :slant normal :weight normal :width normal))))
 '(flyspell-duplicate ((t (:underline t))))
 '(flyspell-incorrect ((t (:underline t))))
 '(help-argument-name ((t (:background "gray25"))))
 '(mouse ((t (:background "purple"))))
 '(whitespace-trailing ((t (:background "pink" :weight bold))) t))


(show-paren-mode 1)

;; don't create those ~ postfixed files
(setq make-backup-files nil)
;; don't crate #files#
(setq auto-save-default nil)

(when (file-exists-p  "~/shared/emacs/pragmatapro-ligatures.el")
    (load "~/shared/emacs/pragmatapro-ligatures.el")
    (add-to-list 'default-frame-alist '(font . "PragmataPro Mono-14")))

(use-package flycheck :ensure t)

;;(require 'company-flx)
;;(company-flx-mode +1)

(use-package flx-ido :ensure t)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;(add-to-list 'safe-local-variable-values
             ;'(haskell-hoogle-command . "stack exec -- hoogle --database=/Users/luntain/p/ta/.stack-work/hoogle"))

(defun web-hoogle (query)
  "Do a Hoogle search for QUERY."
  (interactive
   (list (read-string "Hoogle query: ")))
  (browse-url (format  "http://haskell.org/hoogle/?q=%s" (url-hexify-string query)))
  )

;; haskell yet another indentation
; (use-package hyai :ensure t
; :config (add-hook 'haskell-mode-hook #'hyai-mode))

(use-package company-cabal
  :ensure t
  :config
    (add-to-list 'company-backends 'company-cabal))

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; (package-install 'purescript-mode)
;;(use-package psc-ide
;;  :ensure t)
;(setq psc-ide-use-npm-bin t)
;(add-hook 'purescript-mode-hook
  ;(lambda ()
    ;(psc-ide-mode)
    ;(company-mode)
    ;(flycheck-mode)
    ;(turn-on-purescript-indentation)))

(setq company-idle-delay 2)

(company-tng-configure-default)

; display full file path in the window's title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

; disable the light gray background for the vue-js mode
;(add-hook 'mmm-mode-hook
          ;(lambda ()
            ;(set-face-background 'mmm-default-submode-face nil)))

;(require 'flycheck-vale)
;(flycheck-vale-setup)

; Emacs throws a popup when opening a file that is a symlink
; to a file in a repo. This setting prevents that question and
; tells it not to follow the symlink.
(setq vc-follow-symlinks nil)


(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "ghcide")
 (setq lsp-haskell-process-wrapper-function (lambda (argv) (append '("nice") argv)))
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;(setq lsp-log-io t)
 ;; (define-key evil-normal-state-map "gd" 'intero-goto-definition)
 (define-key evil-normal-state-map "gn" 'flycheck-next-error)
 (define-key evil-normal-state-map "gp" 'flycheck-previous-error))
