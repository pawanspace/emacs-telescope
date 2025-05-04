# Emacs Telescope

A fuzzy finder with preview capabilities for Emacs, inspired by [telescope.nvim](https://github.com/nvim-telescope/telescope.nvim) for Neovim.

## Features

- Fuzzy finding for files, buffers, and grep results
- Live preview of selected items
- Project-aware searching
- Customizable UI

## Installation

### Manual Installation

1. Clone this repository:
   ```
   git clone https://github.com/yourusername/emacs-telescope.git ~/.emacs.d/site-lisp/emacs-telescope
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-telescope")
   (require 'emacs-telescope)
   ```

### With use-package

```elisp
(use-package emacs-telescope
  :straight (emacs-telescope :type git :host github :repo "yourusername/emacs-telescope")
  :bind (("C-c f f" . emacs-telescope-find-files)
         ("C-c f b" . emacs-telescope-buffers)
         ("C-c f g" . emacs-telescope-grep)))
```

## Usage

- `M-x emacs-telescope-find-files` - Find files in the current project
- `M-x emacs-telescope-buffers` - Find and switch to open buffers
- `M-x emacs-telescope-grep` - Search for text in project files

### Key Bindings (within Telescope)

- `C-n` - Move to next item
- `C-p` - Move to previous item
- `RET` - Select current item
- `C-g` - Quit telescope

## Customization

```elisp
;; Customize the UI
(setq emacs-telescope-height 25)
(setq emacs-telescope-width 100)
(setq emacs-telescope-preview-delay 0.1)
```

## Requirements

- Emacs 27.1 or later
- popup.el
- dash.el

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Inspired by [telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)
