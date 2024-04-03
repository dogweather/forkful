---
date: 2024-01-25 03:39:45.710623-07:00
description: "REPL, or Read-Eval-Print Loop, is an interactive programming environment\
  \ that takes single user inputs, executes them, and returns the result. Programmers\u2026"
lastmod: '2024-03-13T22:45:00.475720-06:00'
model: gpt-4-1106-preview
summary: REPL, or Read-Eval-Print Loop, is an interactive programming environment
  that takes single user inputs, executes them, and returns the result.
title: Using an interactive shell (REPL)
weight: 34
---

## How to:
In Fish, the interactive shell is the default mode when you start it up. Here's what it looks like in action:

```Fish Shell
> set color blue
> echo "The sky is $color"
The sky is blue
```

You can also run built-in functions and play with command substitutions:

```Fish Shell
> function cheer
      echo "Go Fish $argv!"
  end
> cheer Coders
Go Fish Coders!
```

Not just defining functions, you can execute code snippets on-the-fly and see the output instantly:

```Fish Shell
> math "40 / 2"
20
```

## Deep Dive
The concept of REPLs goes way back to the Lisp programming language in the 1960s. This form of interactive programming set the benchmark for environments like Python's `ipython` and Ruby's `irb`. Fish continues the trend with a focus on user-friendliness and interactive use.

Fish differs from other shells like Bash in that it's designed with interactivity in mind from the get-go. It provides syntax highlighting, autosuggestions, and tab completions that make it powerful to use in a REPL-style workflow. Better yet, your commands are remembered and searchable, making repeated testing a breeze.

Alternatives to Fish's REPL could be `bash` or `zsh` when paired with extensions like `bash-completion` or `oh-my-zsh`, but Fish tends to offer a richer out-of-the-box experience.

## See Also:
- Fish Documentation: https://fishshell.com/docs/current/index.html
- An interesting comparison of Fish vs. other shells: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- A deeper dive into REPLs: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Interactive programming in Lisp, a historical look: http://www.paulgraham.com/ilisp.html
