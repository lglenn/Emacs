;; Machine-specific config for gptel mode

;; OpenAi Models
(when (boundp 'openai-api-key)
  (setq my/openai-models '("gpt-4o"
                           "gpt-4o-mini"
                           "gpt-4-turbo"
                           "gpt-3.5-turbo")))

;; Anthropic Models
(when (boundp 'anthropic-api-key)
  (setq my/anthropic-models '("claude-opus-4-6"
                              "claude-sonnet-4-6"
                              "claude-haiku-4-5-20251001"
                              "claude-3-5-sonnet-20241022"
                              "claude-3-5-haiku-20241022"
                              "claude-3-opus-20240229")))

;; Local Ollama Models
(when (executable-find "ollama")
  (setq my/ollama-models '("mistral"
                           "llama3.1"
                           "llama3.2"
                           "qwen2.5-coder")))
