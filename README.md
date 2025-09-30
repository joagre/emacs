# ChatGPT extensions for Emacs

## Overview

gptx is built to make AI-assisted code work feel fast, reliable, and safe.
It integrates tightly with gptel to give you repo-scoped chat sessions and a
shared, searchable log buffer so conversations and outputs stay organized by
project. Commands are intentionally simple and predictable: they are
region-aware and will operate on the active region or the whole buffer when
no region is selected (see `gptx-act-on-buffer-when-no-region'). Mutating
operations produce clean unified diffs and non-mutating operations preserve
your code while logging full responses—helping you iterate confidently and
keep an auditable trail of changes.

## Features (random order, not complete)

* Repo-scoped chat session buffer per project.
* A number of commands for reviewing, explaining, asking about, changing,
  improving, documenting, testing, debugging, and troubleshooting code.
  See below.
* Non-mutating commands leave code unchanged and log the full response.
* Mutating commands rewrite in place and log a unified diff.

## Commands made available

### Non-mutating commands

* `gptx-review` Review code. Focus on correctness and risk.
* `gptx-explain` Explain intent, flow, invariants, complexity, and edge cases.
* `gptx-ask` Answer a free-form question about the code with minimal snippets.
* `gptx-write-unit-tests` Generate focused unit tests and a compact test plan.
* `gptx-debug-code` Produce a concrete debugging plan with optional tiny diffs.
* `gptx-troubleshoot-code` Identify likely failure points and fixes by inspection.

### Mutating commands

* `gptx-change` Rewrite region or buffer in place. Logs a unified diff.
* `ptx-improve-code` Improve code quality without changing public behavior.
* `gptx-document-code` Insert idiomatic documentation comments and docstrings only.

## Installation

### Requirements

* Emacs 28.1+
* Packages (ELPA): gptel (>= 0.8), spinner (>= 1.7.4)

```
M-x package-refresh-contents
M-x package-install RET gptel RET
M-x package-install RET spinner RET
```

(Ensure GNU ELPA and/or MELPA are present in `package-archives`)

### OpenAI API key (choose ONE)

* Environment: export OPENAI_API_KEY="sk-***"
* Or a row in ~/.authinfo or ~/.authinfo.gpg such as "machine api.openai.com login apikey password sk-***"

## Load

```
(require 'gptx)
;; Optional:
;; (setq gptx-model 'gpt-5-mini)
;; (setq gptx-log-window-placement 'right
;;       gptx-log-window-size 0.50)
(global-set-key (kbd "C-c r") #'gptx-review)
(global-set-key (kbd "C-c e") #'gptx-explain)
(global-set-key (kbd "C-c a") #'gptx-ask)
(global-set-key (kbd "C-c c") #'gptx-change)
(global-set-key (kbd "C-c u") #'gptx-write-unit-tests)
(global-set-key (kbd "C-c d") #'gptx-debug-code)
(global-set-key (kbd "C-c D") #'gptx-document-code)
(global-set-key (kbd "C-c t") #'gptx-troubleshoot-code)
```
