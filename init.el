(require 'package)
(dolist (source '(("elpa" . "http://elpa.gnu.org/packages/")
                  ("tromey" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

(setq-default indent-tabs-mode nil)
(global-auto-revert-mode t)
(setq inhibit-startup-message t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json.marko\\'" . json-mode))
(add-to-list 'auto-mode-alist
  '("\\.sql.marko\\'" . (lambda ()
  (sql-mode)
  (sql-highlight-ms-keywords))))

(require 'yasnippet)
(yas-global-mode 1)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (highlight-parentheses-mode t)))
(add-hook 'js2-mode-hook (lambda () (electric-pair-mode t)))
(add-hook 'js2-mode-hook (lambda () (show-paren-mode t)))
(add-hook 'js2-mode-hook (lambda () (auto-highlight-symbol-mode t)))
(add-hook 'js2-mode-hook (lambda () (flycheck-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(setq show-paren-delay 0)

;; show the matching paren when it is offscreen
(defadvice show-paren-function
      (after show-matching-paren-offscreen activate)
      "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
      (interactive)
      (let* ((cb (char-before (point)))
             (matching-text (and cb
                                 (char-equal (char-syntax cb) ?\) )
                                 (blink-matching-open))))
        (when matching-text (message matching-text))))

(desktop-save-mode 0)
(setq desktop-path '("~/.emacs.d/tmp/"))
(setq desktop-dirname "~/.emacs.d/tmp/")
(setq desktop-base-file-name "~/.emacs.d/tmp/emacs-desktop")
(setq history-length 250)
    (add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
            "\\)$"))
   (add-to-list 'desktop-modes-not-to-save 'dired-mode)
   (add-to-list 'desktop-modes-not-to-save 'Info-mode)
   (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
   (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)
(defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (desktop-save desktop-dirname))
(add-hook 'after-save-hook 'my-desktop-save)
(add-hook 'auto-save-hook 'my-desktop-save)
(add-hook 'after-init-hook 'desktop-read)
(setq linum-format "%d ")

;; flycheck
(flycheck-def-config-file-var
    flycheck-jscs
    javascript-jscs
    ".jscs"
  :safe #'stringp)

(flycheck-define-checker javascript-jscs
  "A jscs code style checker."
  :command ("jscs" "--reporter" "checkstyle"
            (config-file "--config" flycheck-jscs) source)
  :error-parser flycheck-parse-checkstyle
  :modes (js-mode js2-mode js3-mode)
  :next-checkers (javascript-jshint))

(provide 'flycheck-jscs)

(add-hook 'flycheck-mode-hook
          (lambda()
            ;; Re-add this when it works correctly
            (require 'flycheck-jscs)
            (add-to-list 'flycheck-checkers 'javascript-jscs)))
;; flycheck end
;; backing up
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.
(setq vc-make-backup-files t)

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/tmp/backup")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/tmp/backup")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)
;; backing up


(global-linum-mode t)
(global-hl-line-mode t)
(menu-bar-mode nil)
(nyan-mode t)
(tool-bar-mode nil)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(projectile-global-mode)
(setq projectile-completion-system 'grizzl)
(setq projectile-require-project-root nil)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-x C-f") 'projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x C-d") 'projectile-find-dir)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'projectile-switch-to-buffer)

;; key remap minor mode
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
;; key remap minor mode end
;; paste and format
(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(js2-mode plain-tex-mode sql-mode html-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))
;; paste and format


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (tango-dark)))
 '(global-highlight-changes-mode t)
 '(js2-allow-member-expr-as-function-name t)
 '(js2-basic-offset 4)
 '(js2-build-imenu-callbacks nil)
 '(js2-include-node-externs t)
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.5)
 '(nyan-bar-length 8)
 '(nyan-wavy-trail t)
 '(set-face-background (quote highlight-changes-delete) t)
 '(set-face-foreground (quote highlight-changes-delete))
 '(set-face-underline (quote highlight-changes)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(highlight-changes ((t nil)))
 '(highlight-changes-delete ((t (:foreground "red1"))))
 '(hl-line ((t (:inherit highlight :background "#596569" :foreground "white")))))
