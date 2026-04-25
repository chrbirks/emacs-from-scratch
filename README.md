
# Emacs Configuration

This repository contains an Emacs configuration file, influenced by the Spacemacs project. It is optimized for hardware description language development (Verilog/VHDL) but works as a general modern Emacs setup.

While this configuration is inspired by Spacemacs, it doesn't adopt the Spacemacs principle of layers. Instead, it focuses on easily comprehensible and ready-to-use configurations split across topical files under `lisp/`.

## Use-package

The configuration makes heavy use of [use-package](https://github.com/jwiegley/use-package) for handling package configurations and [elpaca](https://github.com/progfolio/elpaca) for package management.

## Package Integrations

The configuration includes the following packages:

- [Vertico](https://github.com/minad/vertico): Provides simple vertical completion UI.
- [Corfu](https://github.com/minad/corfu): Offers similar functionality to Vertico but in a pop-up menu at point.
- [Consult](https://github.com/minad/consult): Provides a set of additional commands based on the Emacs completion function.
- [Embark](https://github.com/oantolin/embark): Provides a set of actions on completion candidates like killing, inserting, locating and more.

- [Evil](https://github.com/emacs-evil/evil): Vim's modal editing for Emacs.
- [Which-key](https://github.com/justbur/emacs-which-key): Displays available keybindings in popup.
- [Projectile](https://github.com/bbatsov/projectile): Provides project management features, allowing you to switch between projects efficiently.
- [Persp-mode](https://github.com/Bad-ptr/persp-mode.el): Provides workspace management capabilities, allowing isolation of working contexts.
- [Spacemacs-theme](https://github.com/nashamri/spacemacs-theme): A theme package developed for Spacemacs.
- [Spaceline](https://github.com/TheBB/spaceline): A powerline theme developed for Spacemacs.
- [Vundo](https://github.com/casouri/vundo): Tree visualizer for the built-in undo system (replaces `undo-tree`).
- [GCMH](https://gitlab.com/koral/gcmh): Adaptive garbage-collection thresholds.
- [Apheleia](https://github.com/radian-software/apheleia): Asynchronous on-demand code reformatting.
- [compile-multi](https://github.com/mohkale/compile-multi): Per-project picker for `M-x compile` targets, declared in each project's `.dir-locals.el`.
- [envrc](https://github.com/purcell/envrc): Per-buffer environment loaded from `direnv` (`.envrc`).

## Workflow

### Code formatting (explicit only — never on save)

Apheleia is configured for explicit invocation only; saving a file does not trigger any formatter.

| Key       | Action                                                                  |
| --------- | ----------------------------------------------------------------------- |
| `SPC = =` | Format current buffer. Uses Apheleia if a formatter is configured for the major mode (Verilog → `verible-verilog-format`, VHDL → `vhdl_lang --format`); falls back to `lsp-format-buffer` otherwise. |

To add a formatter for another language, push an entry onto `apheleia-formatters` and a `(major-mode . formatter-name)` pair onto `apheleia-mode-alist` in `lisp/efs-hdl.el` (or wherever fits).

External tools used:
- `verible-verilog-format` — install via the [Verible](https://github.com/chipsalliance/verible) toolchain.
- `vhdl_lang` — provided by [rust_hdl](https://github.com/VHDL-LS/rust_hdl) (the same binary that backs the `vhdl_ls` LSP server).

### Undo visualization

| Key       | Action                                          |
| --------- | ----------------------------------------------- |
| `SPC a u` | Open `vundo` to visualize the undo tree.        |

Evil's undo system is set to `'undo-redo` (Emacs 28+ built-in). Vundo provides the tree picker; no `.~undo-tree~` files are written.

### Project compile picker

| Key       | Action                                          |
| --------- | ----------------------------------------------- |
| `SPC p c` | `compile-multi` — pick a target to run.         |
| `SPC p P` | `projectile-commander` (moved from `SPC p c`).  |

Targets are declared per-project in `.dir-locals.el`. Example for a typical FPGA project:

```elisp
;; .dir-locals.el
((nil . ((compile-multi-config
          . (("verilator-lint"
              . "verilator_bin -sv --lint-only -Wall ./rtl/top.sv")
             ("verible-lint"
              . "verible-verilog-lint --rules_config .verible-rules ./rtl/top.sv")
             ("vivado-synth"
              . "make synth")
             ("ghdl-elab"
              . "ghdl -a --std=08 ./src/*.vhd && ghdl -e top"))))))
```

`consult-compile-multi-mode` is enabled, so target picking goes through Vertico/Consult.

### Per-project tool environments (direnv)

`envrc-global-mode` is enabled. When you visit a buffer inside a directory that has an `.envrc`, the environment defined there (e.g. `PATH` for a specific Quartus or Vivado release, `LM_LICENSE_FILE`, a Python venv) becomes buffer-local. Switching buffers between projects switches environments.

Requires the `direnv` binary on your `$PATH`. Run `direnv allow` in each project the first time.

### HDL compilation jump-to-line

`M-x compile`, `next-error`, and `previous-error` understand output from:
- Verilator (`%Error-...:` and `%Warning-...:`)
- Verible-lint (`file:line:col: ...`)
- GHDL (`file:line:col:[severity]:`)
- Vivado (`ERROR: [tag] file:line` and `WARNING: ...`)
- Quartus (`Error: file(line)`)

Regexes live in `lisp/efs-hdl.el` and are added to `compilation-error-regexp-alist` after `compile` loads.

## Installation

Replace your existing `.emacs` or `init.el` file with the contents of this repository (it includes the `lisp/` directory). Back up your existing configuration before doing so.

## Using with Chemacs2

[Chemacs2](https://github.com/plexus/chemacs2) is an Emacs profile switcher. It allows switching between different Emacs configurations.

To use this Emacs configuration with Chemacs2, install Chemacs2 as per the instructions in the [Chemacs2 README](https://github.com/plexus/chemacs2#installation).

Once Chemacs2 is installed, add a new profile for this configuration. Open your Chemacs2 configuration file (usually `~/.emacs-profiles.el`) and add a new entry:

```elisp
(("my-config" . ((user-emacs-directory . "~/path/to/init.el")
                 (server-name . "my-server")
                 (custom-file . "~/path/to/custom.el"))))
```

Replace `"my-config"` with the name for this profile, `"~/path/to/init.el"` with the path to the directory containing this `init.el` file, `"my-server"` with the name for your Emacs server, and `"custom.el"` with your custom file name.

Start Emacs with this configuration by running `emacs --with-profile my-config` in your terminal.

## Directory Structure

```
.
├── init.el                # Slim entry point — load-path + (require 'efs-*) chain
├── early-init.el          # Early initialization settings
├── custom.el              # Emacs customization variables
├── lisp/
│   ├── efs-bootstrap.el   # Elpaca + use-package + diminish/delight
│   ├── efs-evil.el        # general/leader, evil + companions, vundo
│   ├── efs-ui.el          # globals, GC (gcmh), encodings, fonts, theme, modeline
│   ├── efs-completion.el  # vertico/orderless/marginalia/embark/consult/corfu/cape/helpful
│   ├── efs-window.el      # maximized-mode, which-key, vterm, symbol-overlay, transient, treemacs
│   ├── efs-vc.el          # magit, git-gutter, diff-hl
│   ├── efs-projects.el    # projectile, rg, compile-multi, envrc
│   ├── efs-lsp.el         # lsp-mode + family, flycheck
│   ├── efs-org.el         # org, org-roam, org-modern, org-projectile, org-download
│   ├── efs-treesit.el     # tree-sitter grammars, treesit-fold, treesit-auto
│   ├── efs-hdl.el         # verilog-mode, vhdl-mode, vhdl-ts-mode, apheleia, compile regexes
│   └── efs-misc.el        # tramp, persp-mode, ws-butler, hl-todo, yasnippet, etc.
├── elpaca/                # Package manager cache and builds
├── snippets/              # Yasnippet templates (especially Verilog)
├── org-roam-templates/    # Org-roam capture templates
├── org-templates/         # Standard org capture templates
└── var/                   # Variable data (bookmarks, recent files)
```
