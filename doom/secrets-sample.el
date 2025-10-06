;;; secrets-sample.el --- Sample secrets configuration -*- lexical-binding: t; -*-

;; This is a sample secrets file. To use it:
;; 1. Copy this file to "secrets.el"
;; 2. Replace the placeholder values with your actual API keys
;; 3. Set secure permissions: chmod 600 secrets.el
;; 4. The secrets.el file is automatically ignored by git for security

;; IMPORTANT: Never commit secrets.el to version control!
;; The makefile will copy secrets.el to ~/.config/doom/ if it exists.
;; Emacs will refuse to load secrets.el unless it has 600 permissions.

;; Anthropic Claude API Key
;; Get your key from: https://console.anthropic.com/
(setq anthropic-api-key "your-anthropic-api-key-here")

;; OpenAI API Key (optional)
;; Get your key from: https://platform.openai.com/api-keys
;; (setq openai-api-key "your-openai-api-key-here")

;; Other API keys can be added here as needed

(provide 'secrets)
;;; secrets-sample.el ends here