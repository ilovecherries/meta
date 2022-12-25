;;; init.el --- Emacs Configuration
;;; Author: Tabletop (tabletop@horsefucker.org)
;;; Commentary
;; fuck you
;;; Code

(setq gc-cons-threshold 2000000)
(setq read-process-output-max (* 1024 1024))

(defconst tbt-config/config-location
  (expand-file-name (concat user-emacs-directory "config.org"))
  "The location on the filesystem where the configuration is saved.")

(defconst tbt-config/secret-config-location
  (expand-file-name (concat user-emacs-directory "secret.org"))
  "The location on the filesystem where the secret configuraiton is saved.")

(defun tbt-config/load-config (filename)
  "Load the org file from FILENAME using babel."
  (let ((file-name-handler-alist nil))
    (when (file-readable-p filename)
      (org-babel-load-file filename))))

(tbt-config/load-config tbt-config/secret-config-location)
(tbt-config/load-config tbt-config/config-location)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	'("903e3eff08dfb8769ddc974ba7832490c65905e46e81286dfc05d189042a8885" "47518129952b12c737de7b04cc4f64f5b1d04568bfdea5931f0a09fbdae6bf5a" "afeb7b07dbc1a4cfadb24f3ef6c8cf5e63051bf76411779f03a0fe3aadc07768" default))
 '(help-at-pt-display-when-idle '(flymake-diagnostic) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.1)
 '(package-selected-packages
	'(denote sly hide-mode-line origami vertico-posframe all-the-icons-completion svelte-mode unicode-fonts json-mode prism goggles evil-goggles org-src-context valign lambda-line javascript javascript-mode js-mode pyvenv sublimity magit-todos highlight-indent-guides skewer-mode docker-tramp smart-tabs-mode app-launcher exwm doom-modeline tree-sitter lsp-bridge smilebasic moody vundo corfu-doc-terminal corfu-terminal popon pug-mode plantuml-mode org-yt moe-theme selectrum vue-mode visual-fill-column undo-tree shell-pop rainbow-mode rainbow-delimiters page-break-lines ob-mermaid ob-ipython nim-mode lsp-python-ms lsp-java impatient-mode hover dumb-jump beacon ag))
 '(safe-local-variable-values
	'((indent-tabs-mode . 1)
	  (js2-basic-offset . 3)
	  (js2-strict-missing-semi-warning)
	  (lsp-clangd-binary-path . /usr/bin/clangd))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(fixed-pitch-serif ((t (:family cherry/font-name)))))
