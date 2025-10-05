# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal Doom Emacs configuration repository. Doom Emacs is a framework for Emacs that provides sensible defaults and a modular approach to configuration.

## Key Commands

### Setup and Sync
- `make all` - Complete setup including sync, vale styles, and daemon configuration
- `make sync` - Install/sync Doom Emacs configuration files and run doom sync
- `make files` - Copy configuration files to target directories
- `$(HOME)/.config/emacs/bin/doom sync` - Sync Doom configuration (run after config changes)

### Vale (Prose Linting)
- `make vale` - Install Vale (if not present), configuration and style guides
- `make install-vale` - Check and install Vale via Homebrew if not present
- `make vale-clean` - Remove Vale configuration

### Daemon Management
- `make daemon` - Install Emacs daemon launchd configuration
- `make restart` - Restart the Emacs daemon service

### Running Emacs
- `make run` - Start Emacs
- `emacs` - Direct Emacs launch

## Architecture

### Configuration Structure
```
doom/
├── init.el           # Doom modules configuration
├── config.el         # Personal configuration and customizations
├── packages.el       # Package declarations and customizations
└── site-lisp/        # Custom Lisp files
    ├── my-org-config.el      # Org-mode customizations
    ├── my-roam-config.el     # Org-roam configuration
    └── capture-templates/    # Org capture templates
```

### Key Configuration Files
- **init.el**: Defines which Doom modules are enabled (completion, UI, editor, tools, languages)
- **config.el**: Personal settings, keybindings, theme, and package configurations
- **packages.el**: External package declarations and customizations
- **local-org-config.el**: Local org configuration (created from sample if not exists)

### Build System
The Makefile orchestrates the entire setup:
- Copies configuration files from `doom/` to `~/.config/doom/`
- Manages Vale prose linting setup
- Handles Emacs daemon configuration via launchd
- Provides org capture template installation

### Key Features Enabled
- **Language Support**: Java, Python, Ruby, Scala, F#, Emacs Lisp, LaTeX, Markdown, YAML
- **Tools**: LSP, Magit (Git), Docker, Make, Org-mode with Roam
- **UI**: Doom theme, Vertico completion, workspaces, ligatures
- **Editor**: Evil (Vim bindings), snippets, file templates
- **Writing**: Vale integration for prose linting, spell checking with Hunspell

### Dependencies
- Doom Emacs installed at `~/.config/emacs`
- Vale for prose linting (auto-installed via Homebrew if missing)
- LaTeX installation for org-mode formula preview
- Source Code Pro and Source Serif 4 fonts
- Homebrew (recommended for Vale auto-installation)

## Local Configuration
The system uses `local-org-config.el` for machine-specific org-mode settings. This file is created from `local-org-config.sample.el` if it doesn't exist.