# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a **Doom Emacs starter configuration** - a personalized Emacs setup built on top of the Doom Emacs framework. It's designed to work across different environments including Linux desktops and Termux (Android).

## Architecture

### Configuration Structure
- **init.el**: Main module declaration file that defines which Doom modules are enabled
- **config.el**: Primary configuration file for customizations and package settings
- **packages.el**: Package declarations (installations and disables)
- **+user-info.el**: User information configuration
- **+gptel.el**: AI/LLM integration settings
- **+functions.el**: Custom functions and utilities
- **+office.el**: Office/productivity related configurations
- **per-machine.el**: Machine-specific configurations (not committed to git)
- **user-keys.el**: Custom key bindings

### Profile System
This configuration uses Doom's profile system with a "starter" profile:
- Profile name: `starter`
- DOOMDIR: `~/repos/gh/dotdoom-starter`
- Designed for both GUI and terminal (TUI) usage, with terminal as default

## Common Development Tasks

### Starting Emacs
```bash
# Terminal mode (default)
./start-emacs.sh

# GUI mode
./start-emacs.sh --gui

# Open specific file
./start-emacs.sh file.txt
```

### Syncing Configuration
After modifying init.el, packages.el, or installing new packages:
```bash
# Full profile sync
./sync-profile.sh

# Or manually:
doom sync --profile starter
```

### Package Management
```bash
# Install/update packages after modifying packages.el
doom sync --profile starter

# Update all packages
doom upgrade --profile starter

# Check for issues
doom doctor --profile starter
```

### Working with Configuration Files

When modifying elisp configuration:
1. Changes to **init.el** require `doom sync` to take effect
2. Changes to **config.el** can be reloaded with `M-x doom/reload` or restart Emacs
3. Changes to **packages.el** require `doom sync` and Emacs restart

### Key Modules and Features

#### Enabled Key Modules
- **Completion**: corfu with orderless, vertico
- **Editor**: evil mode (vim keybindings), file templates, snippets
- **Tools**: magit (git), direnv, docker, tree-sitter, llm integration
- **Languages**: emacs-lisp, python, nix, javascript, web, yaml, zig, janet
- **Org-mode**: Extended with hugo, pandoc, journal, contacts support

#### Disabled Packages
Notable disabled packages (see packages.el):
- lsp-mode (prefers eglot when needed)
- flycheck related packages (uses flymake)
- solaire-mode, evil-snipe, evil-goggles

#### Custom Packages
- **denote**: Note-taking system with extensive configuration
- **claude-code**: AI coding assistant integration
- **tempel**: Template system
- **aggressive-indent**: Auto-indentation
- **clipetty**: OSC52 clipboard support for terminals

### Environment Detection

The configuration auto-detects and adapts to:
- **Termux** environment (Android): Sets IS-TERMUX flag and adjusts paths
- **Machine-specific settings**: Loads per-machine.el if exists
- **Server mode**: Auto-starts server in terminal mode with name "starter"

### Testing Changes

For testing configuration changes:
```bash
# Byte-compile check (finds syntax errors)
doom compile --profile starter

# Full diagnostic
doom doctor --profile starter

# Clean build (if issues persist)
doom clean --profile starter && doom sync --profile starter
```

### Working with Denote Notes
The configuration extensively uses denote for note-taking. Key integrations:
- denote-silo for managing separate note collections
- denote-org for org-mode integration
- File naming follows denote conventions: `timestamp--title__tags.extension`

## Important Conventions

1. **Profile Awareness**: Always use `--profile starter` when running doom commands
2. **Terminal First**: Configuration optimized for terminal usage; GUI is secondary
3. **Evil Mode**: Vim keybindings are enabled everywhere - consider this when adding features
4. **Tree-sitter**: Preferred over traditional syntax highlighting where available
5. **No Auto-commit**: Never auto-commit changes; user must explicitly request
6. **Custom Theme**: Uses forked doom-themes from junghan0611/doom-themes branch "ko"