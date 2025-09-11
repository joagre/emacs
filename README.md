# Emacs Configuration

A collection of Emacs configurations, customizations, and useful hacks for improved productivity.

## Structure

```
.
├── init.el              # Main configuration entry point
├── config/              # Organized configuration modules
│   ├── basic-settings.el # UI improvements and better defaults
│   └── keybindings.el   # Custom key bindings
├── lisp/               # Custom functions and utilities
│   ├── utils.el        # General utility functions
│   └── text-hacks.el   # Text manipulation functions
└── README.md           # This file
```

## Installation

1. Clone this repository to your desired location:
   ```bash
   git clone https://github.com/joagre/emacs.git ~/.emacs.d
   ```

2. Or symlink the `init.el` file to your Emacs directory:
   ```bash
   ln -s /path/to/this/repo/init.el ~/.emacs.d/init.el
   ```

## Features

### Basic Settings (`config/basic-settings.el`)
- Clean UI (no toolbar, scrollbar, menu bar)
- Better defaults for scrolling and editing
- Line numbers and parentheses matching
- Electric modes for improved editing

### Key Bindings (`config/keybindings.el`)
- Enhanced window navigation
- Quick file operations
- Buffer management shortcuts
- Text editing enhancements

### Utility Functions (`lisp/utils.el`)
- `duplicate-line` - Duplicate current line
- `kill-other-buffers` - Close all other buffers
- `insert-date` / `insert-timestamp` - Insert current date/time
- `rename-file-and-buffer` - Rename file and buffer
- `sudo-edit` - Edit files as root

### Text Manipulation (`lisp/text-hacks.el`)
- `smart-beginning-of-line` - Intelligent line navigation
- `join-next-line` - Join lines efficiently
- `unfill-paragraph` - Remove line breaks from paragraph
- `count-words-region-or-buffer` - Word counting
- `toggle-letter-case` - Cycle through case styles

## Usage

After installation, start Emacs and all configurations will be loaded automatically. The configuration is modular, so you can easily disable or modify specific parts by editing the relevant files in the `config/` and `lisp/` directories.

## Customization

Feel free to modify any of the configuration files to suit your preferences. The structure is designed to be easily extensible - add new `.el` files to the `config/` or `lisp/` directories and they will be loaded automatically.
