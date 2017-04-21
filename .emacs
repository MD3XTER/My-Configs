;--------packages-config--------

; list the packages you want
(setq package-list '(monokai-theme
         drag-stuff
         undo-tree
         multi-term
         org
         neotree
         rainbow-delimiters
         ido-vertical-mode
	 exec-path-from-shell
         bm
         magit
	 web-mode
	 js2-mode
	 php-mode
         jedi
         flycheck
         epc
         elpy
         auto-complete
         projectile
         csv-mode
         switch-window
         smartparens
         desktop+
         paradox
         electric-spacing
         multiple-cursors))

; list the repositories containing them
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
;--------prebuild-functions--------

; start image bug
(defun use-fancy-splash-screens-p ()
  "Return t if fancy splash screens should be used."
  (when (and (display-graphic-p)
	     (or (and (display-color-p)
		      (image-type-available-p 'xpm))
		 (image-type-available-p 'pbm)))
    (let ((frame (fancy-splash-frame)))
      (when frame
	(let* ((img (create-image (fancy-splash-image-file)))
	       (image-height (and img (cdr (image-size img nil frame))))
	       ;; We test frame-height so that, if the frame is split
	       ;; by displaying a warning, that doesn't cause the normal
	       ;; splash screen to be used.
	       (frame-height (1- (frame-height frame))))
	  ;; The original value added to the `image-height' for the test was 19; however,
	  ;; that causes the test to fail on X11 by about 1.5 -- so use 17 instead.
	  (> frame-height (+ image-height 17)))))))

; maximize buffer
(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
    (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

; copy line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

; cut line
(defun quick-cut-line ()
  "Cut the whole line that point is on.  Consecutive calls to this command append each line to the kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-cut-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end)))
    (delete-region beg end))
  (beginning-of-line 1)
  (setq this-command 'quick-cut-line))

; comment/uncomment line
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

; select entire line
(defun select-entire-line ()
	"select entire line"
	(interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

; dublicate line
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

; line-number-mode
(defun show-lines ()
  (linum-mode 1))
;--------python-config--------

; set python default indentation tabs
(add-hook 'python-mode-hook
    (lambda ()
	    (setq-default indent-tabs-mode t)
	    (setq-default tab-width 4)
	    (setq-default py-indent-tabs-mode t)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
; enable line-number-mode
(add-hook 'python-mode-hook 'show-lines)

; enable electric spacing mode
(add-hook 'python-mode-hook 'electric-spacing-mode)

; enable flychek
(add-hook 'python-mode-hook 'flycheck-mode)

;; enable Jedi setup on mode start
(add-hook 'python-mode-hook 'jedi:setup)
;; Don't let jedi-tooltip show up automatically
;;(setq jedi:get-in-function-call-delay 10000000)
;; Start jedi-completion at method dot
(setq jedi:complete-on-dot t)
;; Use custom jedi-keybinds
(add-hook 'python-mode-hook 'jedi-config:setup-keys)

; jedi
(defun jedi-config:setup-keys ()
(local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (local-set-key (kbd "M-?") 'jedi:show-doc)
  (local-set-key (kbd "M-/") 'jedi:get-in-function-call))

;--------c-config--------

; enable line-number-mode
(add-hook 'c-mode-hook 'show-lines)

;--------web-config--------

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
)
;--------global-config--------

; fix linum and auto-combplete bug
(add-hook 'linum-before-update-hook
          '(lambda ()
             (when auto-complete-mode
               (if (ac-menu-live-p)
                   (setq linum-suppress-updates t)
                 (setq linum-suppress-updates nil)))))

; move tmp file to another dir
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

; fullscreen on start
(set-frame-parameter nil 'fullscreen 'fullboth)
(toggle-frame-fullscreen)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

; set font-size
(set-face-attribute 'default nil :font "PragmataPro Mono-13")
(set-face-attribute 'default nil :height 130)

; set exec-path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; set default theme
(load-theme 'monokai t)

; enable undo tree
(global-undo-tree-mode 1)

; set multi-term options
(setq multi-term-program "/usr/local/bin/fish")

; replace marked text
(delete-selection-mode 1)

; move lines M-UP/DOWN
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

; enable org-mode
(setq org-log-done t)

; enable neotree
(setq neo-smart-open t)

; enable rainbow-delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; enable ido-mode
(ido-mode 1)
(ido-vertical-mode 1)

; enable elpy
(elpy-enable)

; enable smart-parens
(smartparens-global-mode 1)

; enable projectile
(projectile-global-mode)

; disable sound
(setq ring-bell-function 'ignore)

; auto refresh file
(global-auto-revert-mode)

;; auto-complete
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)
;; hook up to autocomplete
(add-to-list 'ac-sources 'ac-source-jedi-direct)
;--------key-bindings-config--------

; fix fn-arrows
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(global-set-key (kbd "<prior>") 'beginning-of-buffer)
(global-set-key (kbd "<next>") 'end-of-buffer)

;duplicate line C-c d
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

; new line
(global-set-key (kbd "C-o") (lambda () (interactive)(beginning-of-line)(open-line 1)))
(global-set-key (kbd "M-o") (lambda () (interactive)(end-of-line)(newline)))

; kill to the left
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)) )

; maximize window
(global-set-key "\C-x\ \`" 'toggle-maximize-buffer)

; copy line
(global-set-key "\C-c\M-w" 'copy-line)

; delete parenthesis
(global-set-key "\C-c\M-\\" 'sp-splice-sexp)

; cut line
(global-set-key "\C-c\C-w" 'quick-cut-line)

; comment/uncomment line
(global-set-key "\C-c\ \/" 'toggle-comment-on-line)

; select line
(global-set-key "\C-x\ l" 'select-entire-line)

; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

; neotree
(global-set-key [f8] 'neotree-toggle)

; multiple-cursors
(global-set-key (kbd "C-d") 'mc/mark-next-like-this-word)

; boook-mark
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

; switch-window
(global-set-key (kbd "C-x o") 'switch-window)
;--------key-bindings-config--------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors electric-spacing paradox desktop+ smartparens switch-window csv-mode projectile elpy flycheck jedi magit bm ido-vertical-mode rainbow-delimiters neotree multi-term undo-tree drag-stuff monokai-theme)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
