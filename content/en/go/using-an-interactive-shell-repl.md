---
title:                "Using an interactive shell (REPL)"
date:                  2024-01-25T03:39:45.129877-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"

category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
A REPL (Read-Eval-Print Loop) lets you interact with code live; it reads input, evaluates it, prints the result, and loops back. Programmers use it to test snippets, debug, and learn new languages in real-time.

## How to:
Go doesn't include a built-in REPL, but you can use third-party tools. One popular tool is `gore`:

```go
// Install gore using
$ go install github.com/motemen/gore/cmd/gore@latest

// Run gore
$ gore
gore version 0.5.0  :help for help
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Hello, Go REPL!
nil
```

## Deep Dive
Originally developed for Lisp, REPLs are common in dynamic languages like Python or Ruby. Go, being statically typed, doesn't include one out-of-the-box. Alternatives to `gore` include `go-pry` and `yaegi`. These tools interpret Go code, letting you explore and validate ideas quickly without compiling a full-blown app. They're especially useful for beginners and in educational contexts where the focus is on learning and experimentation.

## See Also
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
