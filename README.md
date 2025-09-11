# ChatGPT Extensions for Emacs

A comprehensive ChatGPT integration package for Emacs that allows you to interact with OpenAI's ChatGPT directly from within your favorite editor.

## Features

- 🤖 **Direct ChatGPT Integration**: Chat with ChatGPT in a dedicated Emacs buffer
- 🔧 **Code Analysis Tools**: Explain, review, document, and refactor code
- 📝 **Text Processing**: Improve writing, translate text, and summarize content
- 💾 **Conversation Management**: Save, load, and export conversation histories
- ⚙️ **Configurable Settings**: Customize API key, model, temperature, and more
- 🎯 **Template System**: Pre-built prompts for common development tasks
- ⌨️ **Key Bindings**: Convenient shortcuts for all functions

## Installation

### Prerequisites

- Emacs 25.1 or later
- OpenAI API key (get one from [OpenAI Platform](https://platform.openai.com/api-keys))

### Setup

1. **Download the package files** to your Emacs configuration directory:
   - `chat-gpt.el` - Main functionality
   - `chat-gpt-utils.el` - Additional utilities
   - `chat-gpt-pkg.el` - Package definition

2. **Load the package** in your Emacs configuration:

```elisp
;; Add to your .emacs or init.el
(add-to-list 'load-path "/path/to/chat-gpt-extensions")
(require 'chat-gpt)
(require 'chat-gpt-utils)

;; Enable global key bindings
(chat-gpt-enable)
(chat-gpt-utils-enable)
```

3. **Configure your API key** (choose one method):

```elisp
;; Method 1: Set in configuration file
(setq chat-gpt-api-key "your-api-key-here")

;; Method 2: Set interactively
M-x chat-gpt-set-api-key
```

## Configuration

### Basic Settings

```elisp
;; API Configuration
(setq chat-gpt-api-key "your-openai-api-key")
(setq chat-gpt-model "gpt-4")                    ; Default: "gpt-3.5-turbo"
(setq chat-gpt-temperature 0.7)                 ; Default: 0.7
(setq chat-gpt-max-tokens 2048)                 ; Default: 2048
(setq chat-gpt-system-message "You are a helpful coding assistant.")
```

### Available Models

- `gpt-3.5-turbo` - Fast and cost-effective
- `gpt-4` - More capable, slower, higher cost
- `gpt-4-turbo-preview` - Latest GPT-4 model
- `gpt-4o` - Optimized GPT-4 variant

## Usage

### Basic Chat

1. **Start a conversation**: `M-x chat-gpt-send-message` or `C-c g m`
2. **Send selected text**: `C-c g r` (send region)
3. **Send entire buffer**: `C-c g b` (send buffer)
4. **Clear conversation**: `C-c g c`

### Code Analysis Tools

| Function | Key Binding | Description |
|----------|-------------|-------------|
| `chat-gpt-explain-code` | `C-c g e` | Explain selected code |
| `chat-gpt-review-code` | `C-c g v` | Review code and suggest improvements |
| `chat-gpt-document-code` | `C-c g d` | Add documentation to code |
| `chat-gpt-refactor-code` | `C-c g f` | Get refactoring suggestions |
| `chat-gpt-debug-code` | `C-c g x` | Get debugging help |
| `chat-gpt-write-tests` | `C-c g t` | Generate unit tests |

### Text Processing

| Function | Key Binding | Description |
|----------|-------------|-------------|
| `chat-gpt-improve-writing` | `C-c g i` | Improve writing style |
| `chat-gpt-translate-text` | `C-c g l` | Translate text to another language |
| `chat-gpt-summarize-buffer` | `C-c g s` | Summarize buffer content |

### Template System

Use predefined templates for common tasks:

```elisp
M-x chat-gpt-use-template
```

Available templates:
- **explain** - Explain code functionality
- **review** - Review code quality
- **document** - Add documentation
- **refactor** - Suggest refactoring
- **debug** - Help with debugging
- **optimize** - Performance optimization
- **translate** - Code translation
- **test** - Generate unit tests

### Conversation Management

| Function | Key Binding | Description |
|----------|-------------|-------------|
| `chat-gpt-save-conversation` | `C-c g S` | Save current conversation |
| `chat-gpt-load-conversation` | `C-c g L` | Load saved conversation |
| `chat-gpt-export-conversation` | `C-c g E` | Export conversation to file |
| `chat-gpt-list-conversations` | - | List all saved conversations |

## Example Workflows

### Code Review Workflow

1. Select code you want reviewed
2. Press `C-c g v` (chat-gpt-review-code)
3. Review ChatGPT's suggestions in the chat buffer
4. Apply improvements to your code

### Documentation Workflow

1. Select a function or code block
2. Press `C-c g d` (chat-gpt-document-code)
3. ChatGPT will generate documentation comments
4. Copy and paste the documentation into your code

### Debugging Workflow

1. Select problematic code
2. Press `C-c g x` (chat-gpt-debug-code)
3. Describe the issue in the chat
4. Follow ChatGPT's debugging suggestions

## Customization

### Adding Custom Templates

```elisp
(add-to-list 'chat-gpt-prompt-templates
             '("security" . "Please analyze this code for security vulnerabilities:\n\n%s"))
```

### Custom Key Bindings

```elisp
;; Add your own key bindings
(global-set-key (kbd "C-c a i") 'chat-gpt-send-message)
(global-set-key (kbd "C-c a r") 'chat-gpt-send-region)
```

### Advanced Configuration

```elisp
;; Customize system message for specific use cases
(setq chat-gpt-system-message 
      "You are an expert software engineer specializing in Emacs Lisp.")

;; Adjust response length
(setq chat-gpt-max-tokens 4096)

;; Make responses more creative
(setq chat-gpt-temperature 1.0)
```

## Troubleshooting

### Common Issues

1. **"API key not configured" error**
   - Set your API key: `M-x chat-gpt-set-api-key`
   - Or add to config: `(setq chat-gpt-api-key "your-key")`

2. **Network/API errors**
   - Check your internet connection
   - Verify your API key is valid
   - Check OpenAI service status

3. **Empty responses**
   - Try reducing `chat-gpt-max-tokens`
   - Check if you've hit API rate limits
   - Verify your OpenAI account has credit

### Debug Mode

Enable debug information:

```elisp
(setq url-debug t)  ; Enable URL debugging
```

## Security Notes

- **API Key Storage**: Store your API key securely, consider using environment variables
- **Data Privacy**: Be aware that your text is sent to OpenAI's servers
- **Rate Limits**: OpenAI has rate limits based on your account tier

## Contributing

This is an open-source project. Contributions are welcome!

### Development Setup

1. Clone the repository
2. Load the files in Emacs
3. Test your changes
4. Submit a pull request

## License

This project is open source. Please check the repository for license details.

## Support

For issues and questions:
- Open an issue on GitHub
- Check OpenAI's documentation for API-related questions
- Review Emacs documentation for editor-specific issues

---

**Happy coding with ChatGPT and Emacs! 🚀**
