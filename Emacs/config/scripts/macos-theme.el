;;; macos-theme -- Provides helper functions for toggling macOS themes.
;;; Author: Cherry (cherry@epochal.quest)
;;; Date: September 4th, 2022
;;; Commentary:
;;; This is just to help with setting an interval timer for macOS theme changes.
;;; Such as, for example, changing the state of the Modus theme.
;;; Code:

(require 'cl-lib)

(defun macos-theme ()
  "Status of the current macOS theme: `dark' or `light'."
  (if (equal (string-trim-right (shell-command-to-string "defaults read -g AppleInterfaceStyle"))
				 "Dark")
		'dark
	 'light))

(defvar macos-theme--timer nil
  "The current timer for checking the current macOS theme state.")

(defvar macos-theme--timer-state nil
  "Used to check if the currently loaded theme is either `light' or `dark'.")

(defun macos-theme-stop-theme-cycle ()
  "Stop the macOS automatic theme changing timer."
  (when (timerp macos-theme--timer)
	 (cancel-timer macos-theme--timer)
	 (setq macos-theme--timer nil)
	 (setq macos-theme--timer-state nil)))

(defun macos-theme-set-theme-cycle (light-theme dark-theme)
  "On macOS theme change, it will switch to either the LIGHT-THEME or DARK-THEME."
  (interactive)
  (macos-theme-stop-theme-cycle)
  (cl-flet ((theme-switch
				 (light-theme dark-theme)
				 (let ((theme (macos-theme)))
					(unless (equal theme macos-theme--timer-state)
					  (setq macos-theme--timer-state theme)
					  (load-theme (if (equal theme 'light)
											light-theme
										 dark-theme)
									  t)))))
	 (setq macos-theme--timer
			 (run-at-time "1 sec"
							  1
							  #'theme-switch
							  light-theme
							  dark-theme))))

(provide 'macos-theme)

;;; macos-theme.el ends here
