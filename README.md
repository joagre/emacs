# Emacs extensions

## Installation

* This file requires the gptel package. Install it, for example, via MELPA if you haven't already. You can install it by running `M-x package-install RET gptel RET`
* Put this file in your load-path.
* Put the OpenAI API key in `~/.authinfo`, e.g. `machine api.openai.com login apikey password sk-proj-***`
* Get an OpenAI API key from https://platform.openai.com/account/api-keys

## How to use

```
(require 'my-chat-gpt)
(global-set-key (kbd "C-c g") #'gptel-change-region)
(global-set-key (kbd "C-c r") #'gptel-review-region)
(global-set-key (kbd "C-c e") #'gptel-explain-region)
(global-set-key (kbd "C-c a") #'gptel-ask-about-region)
```
