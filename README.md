# ChatGPT extensions for Emacs

## Overview

This file provides simple Emacs commands to interact with ChatGPT
(via the gptel package) for code-related tasks. It includes commands
to change, review, explain, and ask questions about the selected region.
Responses are either inserted back into the buffer (for changes) or
logged in a dedicated buffer (for reviews, explanations, and questions).
The commands use a consistent prompt style to guide the model's responses.
The log buffer keeps a history of interactions with timestamps and model info.

## Installation

* Install the gptel package
* Put this file in your load-path
* Put the OpenAI API key in ~/.authinfo
* Get an OpenAI API key from https://platform.openai.com/account/api-keys

Add this to your Emacs config:

```
(require 'gptx)
(setq gptx-model 'gpt-5-mini)
(global-set-key (kbd "C-c r") #'gptx-review)
(global-set-key (kbd "C-c e") #'gptx-explain)
(global-set-key (kbd "C-c a") #'gptx-ask)
(global-set-key (kbd "C-c c") #'gptx-change)
```
