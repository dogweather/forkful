---
title:                "Using an interactive shell (REPL)"
aliases: - /en/swift/using-an-interactive-shell-repl.md
date:                  2024-01-25T03:39:39.492667-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
Using an interactive shell, or a Read-Eval-Print Loop (REPL), lets you code interactively. Programmers use it to test Swift snippets quickly, debug, or learn the language.

## How to:
Invoke REPL by opening a terminal and running `swift`. Type code directly and hit Enter to run it. Here's a taste:

```Swift
1> let greeting = "Hello, REPL!"
greeting: String = "Hello, REPL!"
2> print(greeting)
Hello, REPL!
```

Exit with `:quit` or `Control-D`.

## Deep Dive
REPL's roots go way back to Lisp interpreters in the '60s. Swift’s REPL sits atop LLVM, a powerful compiler framework, offering more than just basic interpretation—it’s a full-fledged tool with autocomplete, debugging, and more. REPL is great for learning or prototyping, but it's not a standalone development environment. Some people prefer using Playgrounds in Xcode for a more graphical, file-based approach, while others stick to traditional script editing and running.

Under the hood, Swift's REPL dynamically compiles code to machine language and executes it, which is why it's relatively fast. It can also access any compiles Swift modules, or even C libraries, making it quite powerful. Note, though, not everything works perfectly in REPL; some Swift features, particularly those requiring complex project setups or storyboard files, won't fly here.

## See Also
- [Swift.org - Getting Started](https://www.swift.org/getting-started/#using-the-repl)
- Apple’s [Introduction to Xcode Playgrounds](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVM Project](https://llvm.org/)
