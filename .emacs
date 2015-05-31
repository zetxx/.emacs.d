(require 'package)
(dolist (source '(("elpa" . "http://elpa.gnu.org/packages/")
                  ("tromey" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

(setq inhibit-startup-message t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(require 'yasnippet)
(yas-global-mode 1)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (highlight-parentheses-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(electric-pair-mode 1)
(show-paren-mode 1)
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
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (tango-dark)))
 '(global-highlight-changes-mode nil)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(indicate-empty-lines t)
 '(js2-allow-member-expr-as-function-name t)
 '(js2-basic-offset 4)
 '(js2-build-imenu-callbacks nil)
 '(js2-include-node-externs t)
 '(menu-bar-mode nil)
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.5)
 '(nyan-bar-length 8)
 '(nyan-mode t)
 '(nyan-wavy-trail t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit highlight :background "#596569" :foreground "white")))))
