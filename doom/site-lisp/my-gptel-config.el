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

  ;; OpenAI Configuration (requires API key)
  ;; Uncomment and set your API key in secrets.el to use OpenAI
  (setq gptel-api-key openai-api-key)
  (setq gptel-model "gpt-5")

  ;; Anthropic Claude Configuration (uses API key from secrets.el)
  (when (boundp 'anthropic-api-key)
    (setq gptel-backend (gptel-make-anthropic "Claude"
                          :stream t
                          :key anthropic-api-key))
    (setq gptel-model 'claude-3-5-sonnet-20241022))

  ;; Custom system prompts/directives
  (setq gptel-directives
        '((default . "You are a helpful assistant.")
          (programming . "You are an expert programmer. Provide clear, concise code examples and explanations. Focus on best practices and readable code.")
          (writing . "You are a skilled writing assistant. Help improve clarity, style, and structure while maintaining the author's voice.")
          (emacs . "You are an Emacs expert. Provide practical elisp solutions and configuration advice for Doom Emacs users.")
          (research . "You are a research assistant. Provide accurate, well-sourced information and help analyze complex topics systematically.")))

  ;; Set default directive
  (setq gptel-default-directive "programming")

  ;; Local model configuration (Ollama)
  ;; Uncomment to use local Ollama models
  ;; (setq gptel-backend (gptel-make-ollama "Ollama"
  ;;                       :host "localhost:11434"
  ;;                       :stream t
  ;;                       :models '("llama2" "codellama" "mistral")))
  ;; (setq gptel-model "llama2")

  ;; Key bindings
  (map! :leader
        (:prefix ("o" . "open")
         :desc "gptel chat" "g" #'gptel)
        :leader
        :desc "gptel send" "G" #'gptel-send
        :leader
        :desc "gptel menu" "M-g" #'gptel-menu))

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

(provide 'my-gptel-config)

;;; my-gptel-config.el ends here