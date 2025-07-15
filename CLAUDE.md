# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a sophisticated Emacs configuration built from scratch, optimized for hardware description language development (Verilog/VHDL) with comprehensive modern tooling. The configuration uses **Elpaca** as the package manager and **use-package** for declarative package configuration.

## Key Architecture Components

### Package Management
- **Elpaca v0.11**: Modern package manager replacing package.el
- **use-package**: Declarative package configuration with `:ensure t` by default
- **Performance optimizations**: GC tuning, native compilation cache setup

### Core Framework
- **Evil mode**: Vim-style modal editing with extensive collection integrations
- **General.el**: Sophisticated key binding system with `SPC` leader key
- **Which-key**: Real-time key binding hints with 0.4s delay
- **Vertico stack**: Modern completion (Vertico, Orderless, Marginalia, Embark, Consult, Corfu)

### Development Environment
- **LSP**: Language Server Protocol with lsp-mode, lsp-ui, and lsp-treemacs
- **Magit**: Git interface with custom full-window display
- **Projectile**: Project navigation and management
- **Treemacs**: File tree with multiple integrations
- **Org-roam**: Zettelkasten-style note-taking system

## Key Binding Patterns

The configuration follows a consistent prefix-based system with `SPC` as the leader key:

```
SPC a   - Applications (org, agenda, etc.)
SPC b   - Buffer operations
SPC c   - Comments
SPC e   - Errors and diagnostics
SPC f   - File operations
SPC g   - Git/version control
SPC j   - Jump/navigation
SPC l   - Layouts/perspectives
SPC o   - Org mode
SPC p   - Project operations
SPC s   - Search and symbols
SPC t   - Toggles
SPC w   - Window management
SPC x   - Text manipulation
SPC 1-9 - Window selection
```

## Language-Specific Configurations

### Verilog/SystemVerilog
- **verilog-mode**: Native mode with extensive customization
- **svlangserver**: LSP server with Verilator/Verible integration
- **Custom linting**: Verilator-based syntax checking
- **Completion**: Cape-powered completion with custom setup

### VHDL
- **vhdl-mode**: Native mode with GHDL compiler support
- **VHDL-LS**: Rust-based language server
- **Flycheck integration**: GHDL-based error checking

### Python
- **lsp-pyright**: Type checking and completion support

## Common Development Tasks

### Package Management
- **Add new package**: Add `(use-package package-name)` block after `(elpaca-wait)`
- **Refresh packages**: `M-x elpaca-rebuild-all` or use `SPC a k` (elpaca helper)
- **Package debugging**: Check `*elpaca-log*` buffer for installation issues

### Git Workflows
- **Magit status**: `SPC g s` opens full-frame magit status
- **Git gutter**: Visual diff indicators in fringe
- **Diff highlighting**: Automatic VC backend selection

### Project Management
- **Switch projects**: `SPC p p` (projectile-switch-project)
- **Find files**: `SPC p f` (projectile-find-file)
- **Project search**: `SPC s p` (consult-ripgrep in project)
- **Perspectives**: `SPC l` for workspace management

### LSP Operations
- **Find definition**: `g d` (evil-goto-definition)
- **Find references**: `g r` (lsp-find-references)
- **Show documentation**: `K` (lsp-describe-thing-at-point)
- **Format code**: `SPC = =` (lsp-format-buffer)

### Org Mode
- **Org agenda**: `SPC a o a`
- **Org capture**: `SPC a o c`
- **Org roam**: Zettelkasten-style note linking
- **Templates**: Located in `org-roam-templates/` and `org-templates/`

## Configuration Files

### Main Files
- **init.el**: Main configuration file (~3000+ lines)
- **early-init.el**: Early initialization settings
- **custom.el**: Emacs customization settings

### Directory Structure
- **elpaca/**: Package manager files and builds
- **snippets/**: Yasnippet templates (especially for Verilog)
- **org-roam-templates/**: Org-roam capture templates
- **org-templates/**: Standard org capture templates
- **var/**: Variable data (bookmarks, recent files, etc.)

## Custom Functions

Key custom functions defined in init.el:
- `efs--toggle-maximize-buffer`: Window maximization with state management
- `efs--magit-status`: Full-frame magit with window restoration
- `efs--cape-capf-setup-verilog`: Verilog completion configuration
- `efs--org-mode-setup`: Org mode initialization
- `efs--set-vc-visualize`: Automatic VC backend selection

## Development Environment Setup

### Terminal Integration
- **Vterm**: Terminal emulator with `SPC '` toggle
- **Project terminals**: Terminal sessions scoped to current project

### Window Management
- **Winum**: Window numbering (1-9) for quick navigation
- **Perspective-mode**: Workspace isolation
- **Window splits**: `SPC w -` (below), `SPC w /` (right)

### Code Navigation
- **Symbol overlay**: `SPC s h` for symbol highlighting
- **Treemacs**: File tree with `SPC f t`
- **Consult**: Enhanced buffer/file navigation

## Performance Considerations

- **Garbage collection**: Tuned for startup and idle performance
- **Native compilation**: Enabled with cache configuration
- **Lazy loading**: Extensive use of `:defer t` and `:commands`
- **LSP optimizations**: Configured for large projects

## Troubleshooting

### Common Issues
- **Package installation**: Check `*elpaca-log*` buffer
- **LSP problems**: Restart with `M-x lsp-restart-workspace`
- **Key bindings**: Use `SPC a b` for `general-describe-keybindings`
- **Performance**: Monitor GC with `M-x emacs-init-time`

### Debugging Commands
- `M-x elpaca-status`: Check package installation status
- `M-x lsp-doctor`: Diagnose LSP configuration
- `M-x consult-flymake`: Check syntax errors
- `M-x magit-status`: Git repository status

## File Locations

- **Configuration root**: `/home/cbs/.config/emacs-from-scratch/`
- **Package cache**: `elpaca/cache/`
- **Built packages**: `elpaca/builds/`
- **Org files**: Various subdirectories with templates and roam files
- **Project data**: `var/` directory for persistent data

This configuration prioritizes HDL development workflow while providing a comprehensive modern Emacs experience suitable for various programming and note-taking tasks.