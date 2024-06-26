---
date: 2024-01-25 03:39:54.985454-07:00
description: "How to: In Bash, your terminal is essentially a REPL. You type a command;\
  \ it reads it, evaluates it, prints the outcome, and loops back awaiting your next\u2026"
lastmod: '2024-03-13T22:45:00.246307-06:00'
model: gpt-4-1106-preview
summary: In Bash, your terminal is essentially a REPL.
title: Using an interactive shell (REPL)
weight: 34
---

## How to:
In Bash, your terminal is essentially a REPL. You type a command; it reads it, evaluates it, prints the outcome, and loops back awaiting your next command. Here's an example of using Bash as a REPL:

```Bash
$ echo "Hello, World!"
Hello, World!
$ x=$((6 * 7))
$ echo $x
42
```

Your input follows the `$ ` prompt, with the output printed on the next line. Simple, right?

## Deep Dive
Bash, short for Bourne Again SHell, is the default shell on many Unix-based systems. It's an upgrade to the original Bourne shell, built-in the late 1970s. While Bash is a powerful scripting tool, its interactive mode allows you to execute commands line by line.

When considering alternatives, you have the Python REPL (simply type `python` in your terminal), Node.js (with `node`), and IPython, an enhanced interactive Python shell. Every language tends to have its own REPL implementation.

Underneath, REPLs are loops that parse your input (commands or code), run it, and return the result to stdout (your screen), often using the language's interpreter directly. This immediacy of feedback is excellent for learning and prototyping.

## See Also
- [Official GNU Bash documentation](https://gnu.org/software/bash/manual/bash.html)
- [Learn Shell Interactive tutorial](https://www.learnshell.org/)
- [IPython Official Website](https://ipython.org/)
- [REPL.it](https://replit.com/): A multi-language online REPL (Not just Bash!)
