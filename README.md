
# Emacs Configuration

This repository contains an Emacs configuration file, influenced by the Spacemacs project. The `init.el` file contains several packages and customizations.

While this configuration is inspired by Spacemacs, it doesn't adopt the Spacemacs principle of layers. Instead, it focuses on easily comprehensible and ready-to-use configurations.

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

## Installation

Replace your existing `.emacs` or `init.el` file with the `init.el` file in this repository. Back up your existing configuration before doing so.

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

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.
