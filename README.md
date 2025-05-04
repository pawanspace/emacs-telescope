# Emacs Telescope

A fuzzy finder with preview capabilities for Emacs, inspired by [telescope.nvim](https://github.com/nvim-telescope/telescope.nvim) for Neovim.

## Features

- Fuzzy finding for files, buffers, and grep results
- Live preview of selected items
- Project-aware searching
- Customizable UI
- Modular architecture for easy extension

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
## Project Structure

The project is organized into several modules with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────────────┐
│                      emacs-telescope.el                         │
│                                                                 │
│  - Core functionality                                           │
│  - Main entry points                                            │
│  - UI management                                                │
│  - Result filtering and selection                               │
└───────────────────┬─────────────────┬───────────────────────────┘
                    │                 │
                    ▼                 ▼
┌─────────────────────────┐  ┌─────────────────────────────────────┐
│ emacs-telescope-grep.el │  │           src/ directory            │
│                         │  │                                     │
│ - Specialized grep      │  │  ┌───────────────────────────────┐  │
│   functionality         │  │  │  emacs-telescope-ui.el        │  │
│ - Project-wide text     │  │  │  - Window layout creation     │  │
│   search                │  │  │  - Buffer setup and styling   │  │
└─────────────────────────┘  │  │  - Display updating           │  │
                             │  └───────────────────────────────┘  │
                             │                                     │
                             │  ┌───────────────────────────────┐  │
                             │  │  emacs-telescope-actions.el   │  │
                             │  │  - File opening               │  │
                             │  │  - Buffer switching           │  │
                             │  │  - Grep result navigation     │  │
                             │  │  - Command execution          │  │
                             │  └───────────────────────────────┘  │
                             │                                     │
                             │  ┌───────────────────────────────┐  │
                             │  │  emacs-telescope-sources.el   │  │
                             │  │  - Project files              │  │
                             │  │  - Open buffers               │  │
                             │  │  - Recent files               │  │
                             │  │  - Grep results               │  │
                             │  │  - Git files and status       │  │
                             │  └───────────────────────────────┘  │
                             └─────────────────────────────────────┘
```

### Component Relationships

1. **Core Module (`emacs-telescope.el`)**
   - Acts as the main entry point for the package
   - Provides the primary user-facing commands
   - Coordinates between UI, actions, and sources
   - Handles user input and selection

2. **Grep Module (`emacs-telescope-grep.el`)**
   - Specialized implementation for text searching
   - Integrates with the core module through function declarations
   - Provides comprehensive file type support for searching

3. **UI Module (`emacs-telescope-ui.el`)**
   - Manages the three-window layout (input, results, preview)
   - Handles buffer setup and styling
   - Updates the display as selections change
   - Provides customization options for appearance

4. **Actions Module (`emacs-telescope-actions.el`)**
   - Defines what happens when items are selected
   - Includes actions for different types of items (files, buffers, grep results)
   - Provides specialized navigation for grep results

5. **Sources Module (`emacs-telescope-sources.el`)**
   - Provides data for the telescope to search
   - Implements filtering for file exclusions
   - Supports various data sources (files, buffers, commands)
   - Integrates with project.el for project-aware searching

### Data Flow

1. User invokes a telescope command (find-files, buffers, grep)
2. Core module creates the UI using the UI module
3. Core module gets data from the appropriate source
4. User filters results by typing in the input buffer
5. Core module updates the selection and preview
6. When user selects an item, the appropriate action is executed
