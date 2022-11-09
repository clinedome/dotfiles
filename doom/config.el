;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Peter Cline"
      user-mail-address "cline.peter@gmail.com")
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq doom-localleader-key ",")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 15)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 14))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; previous was doom-challenger-deep
(setq doom-theme 'doom-dark+)



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:*" :size 0.5 :vslot -4 :select t :quit nil :ttl 0 :side 'right)
)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Project/")
(setq org-log-done 'time)

(defun wrap-around-and-insert (&optional arg)
  (interactive)
  (sp-wrap-round)
  (evil-insert 1))

(after! smartparens
  (smartparens-global-mode)
  (map! :leader
        (:prefix ("k" . "parens")
         :desc "jump to end" "$" #'sp-end-of-sexp
         :desc "jump to beginning" "^" #'sp-beginning-of-sexp
         :desc "raise" "r" #'sp-raise-sexp
         :desc "forward barf" "b" #'sp-forward-barf-sexp
         :desc "forward slurp" "s" #'sp-forward-slurp-sexp
         :desc "wrap" "w" #'wrap-around-and-insert)))

(map! :leader :desc "Exec" "SPC" #'execute-extended-command)

(map! :leader (:prefix "s" :desc "find references" "r" #'lsp-find-references))

(after! org
  (setq org-roam-directory "/home/petercline/org"))

(map! :leader (:prefix ("r" . "org-roam")
                 :desc "toggle buffer" "h" #'org-roam-buffer-toggle
                 :desc "find" "f" #'org-roam-node-find
                 :desc "capture" "c" #'org-roam-capture
                 :desc "capture today" "t" #'org-roam-dailies-capture-today
                 :desc "capture manana" "m" #'org-roam-dailies-capture-tomorrow))

;;CLOJURE
;;
(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
  like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords mode
                          `(("(\\(fn\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "λ")
                                       nil)))
                            ("(\\(partial\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "Ƥ")
                                       nil)))
                            ("(\\(comp\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∘")
                                       nil)))
                            ("\\(#\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))
                            ("\\(#\\){"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∈")
                                       nil))))))

(after! clojure-mode
  (progn
    (clojure/fancify-symbols 'clojure-mode)
    (require 'flycheck-clj-kondo)
    (require 'popwin)
    (popwin-mode 1)
    (setq popwin:debug nil)
    (global-set-key (kbd "C-z") popwin:keymap)
    (push '("*cider-test-report*" :stick t) popwin:special-display-config)
    (push "*cider-error*" popwin:special-display-config)))

(after! cider
(set-popup-rules!
   '(("^\\*cider-repl" :ignore t)))
  (progn
    (clojure/fancify-symbols 'cider-repl-mode)
    (clojure/fancify-symbols 'cider-clojure-interaction-mode))
  (map! :map cider-repl-mode-map "C-k" #'cider-repl-previous-input "C-j" #'cider-repl-next-input)
  (map! :map cider-repl-mode-map :localleader (:prefix "r" :desc "last clojure buffer" "b" #'cider-switch-to-last-clojure-buffer))
  (map! :map clojure-mode-map :localleader (:prefix "e"
                                            :desc "eval func" "f" #'cider-eval-defun-at-point
                                            :desc "eval list" "(" #'cider-eval-list-at-point
                                            :desc "eval defun to comment" ";" #'cider-eval-defun-to-comment)))

(defun cider-repl-new-line-prompt (namespace)
  "Return a prompt string that mentions NAMESPACE."
  (format "%s\n:) " namespace))
(setq cider-repl-prompt-function 'cider-repl-new-line-prompt)
(setq popup-mode t)
(setq cider-auto-jump-to-error t)
(setq cider-show-error-buffer t)
; (setq pop-up-windows t)
(setq clojure-toplevel-inside-comment-form t)
(setq +popup-mode t)

(setq zprint-bin-path "~/bin/zprint")
(load "~/Projects/zprint.el/zprint.el" )
(add-hook 'clojure-mode-hook 'zprint-mode)

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(global-set-key (kbd "s-r") 'cider-eval-buffer)
(global-set-key (kbd "s-p") 'cider-eval-defun-at-point)
(global-set-key (kbd "s-l") 'sp-forward-slurp-sexp)
(global-set-key (kbd "s-'") 'sp-raise-sexp)
(global-set-key (kbd "s-;") 'wrap-around-and-insert)
(global-set-key (kbd "s-t") 'cider-test-run-test)
(global-set-key (kbd "s-T") 'cider-test-run-ns-tests)
(global-set-key (kbd "s-o") 'cider-find-ns)
(global-set-key (kbd "s-O") 'find-file)
(global-set-key (kbd "s-<left>") 'previous-buffer)
(global-set-key (kbd "s-<right>") 'next-buffer)
(global-set-key (kbd "<C-r>") 'cider-repl-history-search-backward)
(global-set-key (kbd "s-<up>") 'cider-repl-previous-input)
(global-set-key (kbd "s-<down>") 'cider-repl-next-input)
(global-set-key (kbd "<C-tab>") 'consult-buffer)

(global-set-key (kbd "s-d") '+lookup/definition)
(global-set-key (kbd "s-u") 'cider-xref-fn-refs)

; maybe solves nothing
; (global-set-key (kbd "s-i") 'toggle-window-dedicated)

; nah
; (global-set-key (kbd "<M-tab>")  'consult-buffer)
; (global-set-key (kbd "<M-tab>") '+vertico/switch-workspace-buffer)