# ChatGPT extensions for Emacs

## Overview
gptx provides lean, predictable workflows around gptel for working with code. It
keeps a persistent, repo-scoped chat buffer per project and a shared log
buffer. All high-level commands are region-aware and operate on the active
region or on the whole buffer when no region is active (see
`gptx-act-on-buffer-when-no-region').

## Features

* Repo-scoped chat session buffer per project.
* Shared log buffer.
* Mode-line spinner while requests are in flight.
* For in-place rewrites, a unified diff is logged that compares before and
  after.

## Installation

* Install the gptel package
* Put gotx.el in your load-path
* Get an OpenAI API key from https://platform.openai.com/account/api-keys
* Put the OpenAI API key in ~/.authinfo

## Commands

### Non-mutating

* `gptx-review` - Review code. Focus on correctness and risk.
* `gptx-explain` - Explain intent, flow, invariants, complexity, and edge cases.
* `gptx-ask` - Answer a free-form question about the code with minimal snippets.
* `gptx-write-unit-tests` - Generate focused unit tests and a compact test plan.
* `gptx-debug-code` - Produce a concrete debugging plan with optional tiny diffs.
* `gptx-troubleshoot-code` - Identify likely failure points and fixes by inspection.

### Mutating
* `gptx-change` - Rewrite region or buffer in place. Logs a unified diff.
* `gptx-improve-code` - Improve code quality without changing public behavior.
* `gptx-document-code` - Insert idiomatic documentation comments and docstrings only.

## Example usage

```
(require 'gptx)
(setq gptx-model 'gpt-5-mini)
;; Optional window setup
(setq gptx-log-window-placement 'right
      gptx-log-window-size 0.40)
;; Suggested key bindings
(global-set-key (kbd "C-c r") #'gptx-review)
(global-set-key (kbd "C-c e") #'gptx-explain)
(global-set-key (kbd "C-c a") #'gptx-ask)
(global-set-key (kbd "C-c c") #'gptx-change)
(global-set-key (kbd "C-c u") #'gptx-write-unit-tests)
(global-set-key (kbd "C-c d") #'gptx-debug-code)
(global-set-key (kbd "C-c D") #'gptx-document-code)
(global-set-key (kbd "C-c t") #'gptx-troubleshoot-code)
(global-set-key (kbd "C-c h") #'gptx-improve-code)
```
