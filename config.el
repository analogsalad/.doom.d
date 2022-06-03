;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "analogsalad"
      user-mail-address "hey@analogsalad.com")

;; ## Editor Settings ##
(setq display-line-numbers-type t)
(setq-default fill-column 120)
(setq fancy-splash-image "~/Pictures/Brainstorm.jpg")

(setq highlight-indent-guides-method 'column)
;; Start emacs fullscreen:
;(add-hook 'after-init-hook 'toggle-frame-fullscreen)

;; Set whitespace style:
(setq-default whitespace-style '(face tabs tab-mark spaces space-mark trailing))
;(global-whitespace-mode +1)

;; ## Theme Settings ##
(setq doom-theme 'doom-xcode)

;; ## Font Settings ##
;; TODO: Find out where doom-variable-pitch-font and doom-serif-font is used.
(setq-default line-spacing 0.20)
(setq-default doom-font (font-spec :family "JetBrains Mono Medium" :size 24)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 24)
      doom-big-font (font-spec :family "JetBrains Mono Bold" :size 24)
      doom-unicode-font (font-spec :family "IBM Plex Mono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

;; ## Org-Mode Settings ##
;; Completed items get timestamped:
(setq org-log-done 'time)
;; Multiline emphasis is allowed(up-to 5 lines):
(setq org-emphasis-regexp-components
  '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 5))

;; ## crystal-mode settings##
;; Register crystalline as a language backend:
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(crystal-mode . "crystal"))

  (lsp-register-client
  (make-lsp-client :new-connection (lsp-stdio-connection '("crystalline"))
                   :activation-fn (lsp-activate-on "crystal")
                   :completion-in-comments? nil
                   :priority 1
                   :server-id 'crystalline)))

; TODO: Remove the following snippets after finding a solution.
; Issue #1: The icons don't load unless you manually run doom/reload
; Issue #2: The icons are sized much bigger than the already defined icons
;(treemacs-define-custom-icon (all-the-icons-alltheicon "html5" :height 1.00) "ecr")
;(treemacs-define-custom-icon (format " %s "(all-the-icons-alltheicon "html5")) "ecr")
;(treemacs-define-custom-icon (format " %s "(all-the-icons-fileicon "crystal")) "cr")

;; ## go-mode settings ##
;; Use goimports instead of go-fmt
(setq gofmt-command "goimports")

;; flycheck-golangci-lint
;; Set Lint Rules
;; (setq flycheck-golangci-lint-enable-all t)

;; Making flycheck work with LSP
;; See: https://github.com/weijiangan/flycheck-golangci-lint/issues/8
;;(defvar-Local flycheck-local-checkers nil)
;;  (defun +flycheck-checker-get(fn checker property)
;;    (or (alist-get property (alist-get checker flycheck-local-checkers))
;;        (funcall fn checker property)))
;;  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

;;(add-hook 'go-mode-hook (lambda()
;;                            (flycheck-golangci-lint-setup)
;;                            (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))

;; ## web-mode settings ##
;; Register extensions as web-mode targets:
(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ecr\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; Use the erb engine in ecr files:
(setq web-mode-engines-alist
      '(("erb" . "\\.ecr\\'")))

;; ## js2-mode settings ##
;; Enable eslint integration:
(setq-hook! 'js2-mode-hook flycheck-checker 'javascript-eslint)

;; ## lsp-mode settings ##
;; lsp tries to render links on treemacs, destroying its functionality.
;; as a work around we disable this offending setting.
(setq lsp-enable-links nil)

;; lsp performance tuning:
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Slow lsp down
;;(setq lsp-idle-delay 0.800)
(after! lsp-mode
 (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'")
 (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.parcel-cache\\'")
 (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.git\\'")
 (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\build\\'")
 (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\lib\\'")
 (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\node_modules\\'"))


;;## lisp-mode settings ##
;; Make sly open vertically instead of horizontally
(after! sly
        (set-popup-rule! "^\\*sly-mrepl" :ignore t))


;; ## treemacs settings ##
;; Make treemacs display only the current project:
(add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively)
;; Make treemacs display colorful icons:
(setq doom-themes-treemacs-theme "doom-colors")

;; ## Key Binds ##
(setq evil-escape-key-sequence "jj")

(map! :leader
      :desc "toggle whitespace mode"
      "t w" 'whitespace-mode)

(map! :leader
      :desc "toggle documentation"
      "t k" 'lsp-ui-doc-glance)

(map! :leader
      :desc "comment line"
      "l c" 'comment-line)

(map! :after go-mode
    :map go-mode-map
    :leader
    :desc "gofmt"
    "m f" 'gofmt)

(map! :after crystal-mode
      :map crystal-mode-map
      :leader
      :desc "crystal format"
      "m f" 'crystal-tool-format)

(map! :after go-mode
    :map go-mode-map
    :leader
    :desc "godoc at point"
    "m k" 'godoc-at-point)

(map! :after rjsx-mode
      :map rjsx-mode-map
      :leader
      :desc "lsp format"
      "m f" 'lsp-format-buffer)

(map! :after web-mode
      :map web-mode-map
      :leader
      :desc "editorconfig format"
      "m f" 'editorconfig-format-buffer)

(map! :after scss-mode
      :map scss-mode-map
      :leader
      :desc "editorconfig format"
      "m f" 'editorconfig-format-buffer)

(map! :leader
      :desc "reload/doom"
      "x" 'doom/reload)

(map! :leader
      :desc "edit code block in org-mode"
      "o c" 'org-edit-src-code)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;(setq org-directory "~/org/")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
