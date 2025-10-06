;;; my-gptel-config.el --- GPTel LLM configuration and writing critique system

;;; Commentary:
;;; GPTel configuration with OpenAI and Anthropic backends, plus
;;; interactive writing critique functionality for analyzing text
;;; with different specialized prompts.

;;; Code:

;;; LLM Configuration with gptel
(use-package! gptel
  :config
  ;; Set default model parameters
  (setq gptel-default-mode 'org-mode)

  ;; Define available backends as variables
  (defvar my/gptel-backends '()
    "List of available gptel backends.")

  ;; OpenAI Configuration
  (when (boundp 'openai-api-key)
    (let ((openai-backend (gptel-make-openai "OpenAI"
                            :key openai-api-key
                            :stream t
                            :models '("gpt-4o" "gpt-4o-mini" "gpt-4-turbo" "gpt-3.5-turbo"))))
      (add-to-list 'my/gptel-backends `("OpenAI" . ,openai-backend))))

  ;; Anthropic Claude Configuration
  (when (boundp 'anthropic-api-key)
    (let ((claude-backend (gptel-make-anthropic "Claude"
                            :key anthropic-api-key
                            :stream t
                            :models '("claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022" "claude-3-opus-20240229"))))
      (add-to-list 'my/gptel-backends `("Claude" . ,claude-backend))))

  ;; Local Ollama Configuration
  (when (executable-find "ollama")
    (let ((ollama-backend (gptel-make-ollama "Ollama"
                            :host "localhost:11434"
                            :stream t
                            :models '("mistral" "llama3.2" "codellama" "phi3"))))
      (add-to-list 'my/gptel-backends `("Ollama" . ,ollama-backend))))

  ;; Set default backend (prefer Ollama > Claude > OpenAI)
  (cond
   ((assoc "Ollama" my/gptel-backends)
    (setq gptel-backend (cdr (assoc "Ollama" my/gptel-backends)))
    (setq gptel-model 'llama3.2))
   ((and (boundp 'anthropic-api-key) (assoc "Claude" my/gptel-backends))
    (setq gptel-backend (cdr (assoc "Claude" my/gptel-backends)))
    (setq gptel-model 'claude-3-5-sonnet-20241022))
   ((and (boundp 'openai-api-key) (assoc "OpenAI" my/gptel-backends))
    (setq gptel-backend (cdr (assoc "OpenAI" my/gptel-backends)))
    (setq gptel-model 'gpt-4o))
   (t
    (message "No gptel backends available. Please configure API keys or install Ollama.")))

  ;; Custom system prompts/directives
  (setq gptel-directives
        '((default . "You are a helpful assistant.")
          (programming . "You are an expert programmer. Provide clear, concise code examples and explanations. Focus on best practices and readable code.")
          (writing . "You are a skilled writing assistant. Help improve clarity, style, and structure while maintaining the author's voice.")
          (emacs . "You are an Emacs expert. Provide practical elisp solutions and configuration advice for Doom Emacs users.")
          (research . "You are a research assistant. Provide accurate, well-sourced information and help analyze complex topics systematically.")))

  ;; Set default directive
  (setq gptel-default-directive "programming")

  ;; Backend selection functions
  (defun my/gptel-list-backends ()
    "List available gptel backends."
    (interactive)
    (if my/gptel-backends
        (let ((current-backend-name (or (and gptel-backend
                                            (car (rassoc gptel-backend my/gptel-backends)))
                                       "None")))
          (message "Available backends: %s | Current: %s"
                   (mapconcat 'car my/gptel-backends ", ")
                   current-backend-name))
      (message "No gptel backends available")))

  (defun my/gptel-switch-backend ()
    "Interactively switch between available gptel backends."
    (interactive)
    (if (not my/gptel-backends)
        (user-error "No gptel backends available")
      (let* ((backend-names (mapcar 'car my/gptel-backends))
             (choice (completing-read "Select backend: " backend-names nil t))
             (selected-backend (cdr (assoc choice my/gptel-backends))))
        (setq gptel-backend selected-backend)
        ;; Set appropriate default model for each backend
        (cond
         ((string= choice "Claude")
          (setq gptel-model 'claude-3-5-sonnet-20241022))
         ((string= choice "OpenAI")
          (setq gptel-model 'gpt-4o))
         ((string= choice "Ollama")
          (setq gptel-model 'llama3.2)))
        (message "Switched to %s backend with model %s" choice gptel-model))))

  ;; Key bindings
  (map! :leader
        (:prefix ("o" . "open")
         :desc "gptel chat" "g" #'gptel)
        :leader
        :desc "gptel send" "G" #'gptel-send
        :leader
        :desc "gptel menu" "M-g" #'gptel-menu
        :leader
        (:prefix ("t" . "toggle")
         :desc "gptel backend" "b" #'my/gptel-switch-backend
         :desc "list backends" "B" #'my/gptel-list-backends))

;;; Writing Critique System
;; Defer loading until gptel is available and ensure robustness

(after! gptel

  ;; Helper function for critique operations
  (defun my/gptel-critique-with-prompt (prompt-text)
    "Send selected text to gptel with a specific critique prompt in a new buffer."
    (unless (fboundp 'gptel-send)
      (user-error "GPTel is not available. Please ensure gptel is properly configured"))
    (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
           (critique-type (car (split-string prompt-text " " t)))
           (buffer-name (format "*Writing Critique: %s*" critique-type))
           (critique-buffer (get-buffer-create buffer-name)))
      (with-current-buffer critique-buffer
        (erase-buffer)
        (org-mode)
        (when (fboundp 'gptel-mode)
          (gptel-mode))
        (insert prompt-text "\n\n" selected-text)
        (goto-char (point-max)))
      (pop-to-buffer critique-buffer)
      (gptel-send)))

  ;; Individual critique functions
  (defun my/gptel-critique-general ()
    "Provide comprehensive writing feedback on selected text."
    (interactive)
    (my/gptel-critique-with-prompt "Please provide comprehensive writing feedback on the following text, covering: clarity, coherence, grammar, style, structure, and word choice. Include specific examples and actionable suggestions.\n\nText to critique:"))

  (defun my/gptel-critique-style ()
    "Analyze writing style of selected text."
    (interactive)
    (my/gptel-critique-with-prompt "Please analyze the writing style of the following text, focusing on: tone consistency, voice, sentence variety, word choice, rhythm, and overall readability. Suggest specific stylistic improvements.\n\nText to critique:"))

  (defun my/gptel-critique-clarity ()
    "Analyze clarity and coherence of selected text."
    (interactive)
    (my/gptel-critique-with-prompt "Please analyze the clarity and coherence of the following text: Are ideas clearly expressed? Is the logic easy to follow? Are there confusing passages? Suggest specific ways to improve clarity.\n\nText to critique:"))

  (defun my/gptel-critique-flow ()
    "Evaluate structure and flow of selected text."
    (interactive)
    (my/gptel-critique-with-prompt "Please evaluate the structure and flow of the following text: Are paragraphs well-organized? Do ideas transition smoothly? Is the overall structure logical? Suggest improvements for better organization and flow.\n\nText to critique:"))

  (defun my/gptel-critique-academic ()
    "Review selected text for academic writing standards."
    (interactive)
    (my/gptel-critique-with-prompt "Please review the following text for academic writing standards: Check formal tone, argument structure, evidence support, citation style, objectivity, and scholarly conventions. Suggest improvements for academic rigor.\n\nText to critique:"))

  (defun my/gptel-critique-business ()
    "Analyze selected text for business communication effectiveness."
    (interactive)
    (my/gptel-critique-with-prompt "Please analyze the following text for business communication effectiveness: Check professional tone, conciseness, action-oriented language, audience appropriateness, and clarity of purpose. Suggest improvements for effective business writing.\n\nText to critique:"))

  (defun my/gptel-critique-technical ()
    "Review selected text for technical writing clarity."
    (interactive)
    (my/gptel-critique-with-prompt "Please review the following text for technical writing clarity: Check precision, logical organization, appropriate terminology, step-by-step clarity, and accessibility to target audience. Suggest improvements for technical communication.\n\nText to critique:"))

  (defun my/gptel-writing-critique-menu ()
    "Interactive menu for different writing critiques."
    (interactive)
    (unless (use-region-p)
      (user-error "Please select a region of text to critique"))
    (let ((choice (read-multiple-choice
                   "Select critique focus:"
                   '((?g "general" "Comprehensive writing critique")
                     (?s "style" "Focus on style and tone")
                     (?c "clarity" "Focus on clarity and coherence")
                     (?f "flow" "Focus on structure and flow")
                     (?a "academic" "Academic writing standards")
                     (?b "business" "Business communication")
                     (?t "technical" "Technical writing clarity")))))
      (pcase (car choice)
        (?g (my/gptel-critique-general))
        (?s (my/gptel-critique-style))
        (?c (my/gptel-critique-clarity))
        (?f (my/gptel-critique-flow))
        (?a (my/gptel-critique-academic))
        (?b (my/gptel-critique-business))
        (?t (my/gptel-critique-technical)))))

  ;; Writing critique keybindings - only set if gptel is available
  (map! :leader
        (:prefix ("w" . "write")
         :desc "critique menu" "c" #'my/gptel-writing-critique-menu
         :desc "critique general" "g" #'my/gptel-critique-general
         :desc "critique style" "s" #'my/gptel-critique-style
         :desc "critique clarity" "C" #'my/gptel-critique-clarity)))

) ; Close (after! gptel) block

(provide 'my-gptel-config)

;;; my-gptel-config.el ends here
