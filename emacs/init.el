(require 'package)

(setq package-archives
      '(;  ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa"     . "http://melpa.org/packages/")
        ; ("marmalade" . "http://marmalade-repo.org/packages/")
        ;("gnu"       . "http://elpa.gnu.org/packages/")
        ))

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq
   inhibit-startup-screen t
   initial-scratch-message nil
   sentence-end-double-space nil
   ;; Prompts should go in the minibuffer, not in a GUI.
   use-dialog-box nil ;
   )

(column-number-mode)

(use-package hindent
  :init ; init - to execute before the package is loaded
  (setq hindent-extra-args '("--line-length" "120"))
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (put 'projectile-project-name 'safe-local-variable #'stringp)
  ; this goes with naming the project inside .dir-locals.el:
  ; ((nil . ((projectile-project-name . "GFS"))))
  (setq projectile-completion-system 'ivy)
  )
(projectile-mode)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
)

(add-to-list 'completion-ignored-extensions ".exe")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-pcm-word-delimiters "-./:| ")
 '(custom-file "~/shared/emacs/init.el")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
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
 '(grep-find-ignored-directories (quote (".git" ".stack-work")))
 '(haskell-mode-hook (quote (flycheck-mode)))
 '(hyai-basic-offset 2)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-use-virtual-buffers t)
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (exec-path-from-shell psc-ide purescript-mode vterm ivy-rich company yasnippet magit which-key expand-region ormolu reformatter lsp-haskell lsp-ui lsp-mode flx-ido flycheck ace-jump-mode smartparens ace-window evil-surround evil-collection evil yaml-mode counsel-projectile ivy-hydra counsel ivy hydra dumb-jump hyai haskell-mode smex simp f ac-capf)))
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
(setq require-final-newline t)

(use-package yaml-mode :ensure t)

(use-package undo-fu :ensure t)

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t) ; evil-want-C-u-scroll needs to be set before 'evil is loaded
  (setq evil-undo-system 'undo-fu) ; can be set to the built-in 'undo-redo in Emacs 28, also: remove undo-fu use-package line

  ; The following snippet will make Evil treat an Emacs symbol as a word.
  ; This has the advantage that it changes depending on the language:
  ; `foo-bar` is one symbol in lisp-mode but two symbols in c-mode
  ; I am surprised that it works for move back a word or deleting a word
  :config
  (require 'evil-surround)
  (defalias #'forward-evil-word #'forward-evil-symbol)


  (define-key evil-normal-state-map "s" 'evil-surround-edit)
  (define-key evil-normal-state-map ",e" 'counsel-find-file)
  (define-key evil-normal-state-map ",f" 'counsel-projectile-find-file) ;'projectile-find-file)
  (define-key evil-normal-state-map ",p" 'counsel-projectile-switch-project)
  (define-key evil-normal-state-map ",b" 'ivy-switch-buffer) ; counsel-swithc-buffer preloads the buffer as you type its name, this is however not so great if loading a buffer costs starting some backgroun processes
  (define-key evil-normal-state-map ",c" 'comment-dwim)
  (define-key evil-normal-state-map (kbd "C-l") 'delete-other-windows)
  (define-key evil-normal-state-map (kbd "gt") 'evil-goto-definition)

  (define-key evil-insert-state-map (kbd "C-l") 'evil-forward-char)
  (define-key evil-insert-state-map "\r" 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-o") 'evil-ret)

  :ensure t)

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

(use-package hydra :ensure t
 :config
 (defhydra hydra-window (:color red
                        :hint nil)
  "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("v" split-window-right)
  ("x" split-window-below)
  ;("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("f" new-frame :exit t)
  ("s" ace-swap-window)
  ("da" ace-delete-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil)
  ;("i" ace-maximize-window "ace-one" :color blue)
  ;("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump))
  (define-key evil-normal-state-map ",w" 'hydra-window/body)
)


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
  (define-key evil-normal-state-map (kbd "M-o") 'ace-window)
  (define-key evil-normal-state-map ",a" 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
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

(use-package ace-jump-mode :ensure t :config
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
  (define-key evil-normal-state-map (kbd "C-SPC") 'ace-jump-mode))

(use-package avy :ensure t :config
  (define-key evil-normal-state-map (kbd "S-SPC") 'avy-goto-char-2)
 )

(define-key evil-normal-state-map (kbd "C-f") 'universal-argument)

(define-prefix-command 'profiler-map)
(global-set-key (kbd "C-p") 'profiler-map)
(define-key evil-normal-state-map (kbd "C-p") 'profiler-map)
(define-key profiler-map (kbd "s") 'profiler-start)
(define-key profiler-map (kbd "S") 'profiler-stop)
(define-key profiler-map (kbd "r") 'profiler-report)
(define-key profiler-map (kbd "R") 'profiler-reset)

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
(setq create-lockfiles nil)

(when (file-exists-p  "~/shared/emacs/pragmatapro-ligatures.el")
    (load "~/shared/emacs/pragmatapro-ligatures.el")
    (add-to-list 'default-frame-alist '(font . "PragmataPro Mono-14")))

(use-package flycheck :ensure t
  :config
  ; so that flycheck overrides eldoc
  (setq eldoc-idle-delay 0.1
        flycheck-display-errors-delay 0.2)
  )


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

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(use-package purescript-mode :ensure t)
(use-package psc-ide
  :ensure t
  :config
  (setq psc-ide-use-npm-bin t)
  (add-hook 'purescript-mode-hook
    (lambda ()
      (psc-ide-mode)
      (company-mode)
      (flycheck-mode)
      (turn-on-purescript-indentation)))
)

(use-package company
  :diminish
  :bind (("C-." . #'company-complete))
  :hook (prog-mode . company-mode)
  :init
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-idle-delay 1 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  :config
  (company-tng-configure-default)

  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
			  `(lambda () (interactive) (company-complete-number ,x))))
	    (number-sequence 0 9))))

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

; 100mb gc threshold, recommended by lsp-mode authors
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-prefer-capf t)

(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp
  :init
  (setq lsp-keymap-prefix "s-l") ;; doubt if it works
  (setq lsp-use-native-json t)
  (setq lsp-print-performance nil)
  (setq lsp-log-io nil)
  (setq lsp-diagnostics-modeline-scope :project)
  (setq lsp-file-watch-threshold 5000)
)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  ;(setq lsp-idle-delay 1)
  (setq lsp-ui-doc-position (quote bottom))
  :config
  (setq company-minimum-prefix-length 1)
  (eldoc-mode -1) ;; superfluous
)
(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 (setq lsp-haskell-process-wrapper-function (lambda (argv) (append '("nice") argv)))
 (setq lsp-haskell-process-args-hie nil)
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;(setq lsp-log-io t)
 ;; (define-key evil-normal-state-map "gd" 'intero-goto-definition)
 (define-key evil-normal-state-map "gn" 'flycheck-next-error)
 (define-key evil-normal-state-map "gp" 'flycheck-previous-error))

(use-package reformatter :ensure t)
(use-package ormolu
  :ensure t
 ; :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
   ("C-c r" . ormolu-format-buffer)))

; no menu bar
(menu-bar-mode -1)
; no scroll bar
(toggle-scroll-bar -1)

(use-package expand-region
  :ensure t
  :bind (("C-c n" . er/expand-region)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom (which-key-idle-delay 2))

(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package yasnippet
  :ensure t
  :defer 3 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :config (yas-global-mode)
  :custom (yas-prompt-functions '(yas-completing-prompt)))

(use-package smex :ensure t
  :init
  (setq smex-save-file "~/.emacs.d/smex-items"))

(use-package ivy :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  ; ivy--regex-plus is Ivy's default completion method:
  ; space is turned into .*
  ; ! causes inversion for following patterns
  ; c-o opens a hydra menu
  ; counsel
  ;   describe
  ;     function, var
  ;   find-library
  ;   ivy-push-view: c-c v, c-c V for pop
  )
(use-package counsel :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-k") 'counsel-M-x)
  (define-key evil-insert-state-map (kbd "C-k") 'counsel-M-x)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-k") 'counsel-M-x)
)
(use-package ivy-hydra :ensure t)
(ivy-mode)
(counsel-mode)
(use-package counsel-projectile :ensure t
  :init
  (setq counsel-projectile-remove-current-project t)
  (setq counsel-projectile-remove-current-buffer t)
  )

(use-package ivy-rich :ensure t
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )
