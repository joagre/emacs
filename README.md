# Emacs extensions

Author: 2025 Joakim G.
URL:
Version: 0.1
Keywords: convenience, tools, ai, gpt
License: MIT
Description: Simple ChatGPT code assistant using gptel
2025-09-11 Initial version

Installation:

* This file requires the 'gptel' package. Install it via MELPA if you haven't already.
  You can install it by running M-x package-install RET gptel RET
* Add this file to your Emacs load-path and add the following to your init file (e.g. ~/.emacs or ~/.emacs.d/init.el):
* Make sure you have set your OpenAI API key in ~/.authinfo
  Example entry in ~/.authinfo:
  machine api.openai.com login apikey password sk-proj-***
* You can get an API key from https://platform.openai.com/account/api-keys

How to use:

(require 'my-chat-gpt)
(global-set-key (kbd "C-c g") #'gptel-change-region)
(global-set-key (kbd "C-c r") #'gptel-review-region)
(global-set-key (kbd "C-c e") #'gptel-explain-region)
(global-set-key (kbd "C-c a") #'gptel-ask-about-region)
