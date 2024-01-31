---
title:                "Using an interactive shell (REPL)"
date:                  2024-01-25T03:39:38.209897-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
A REPL (Read-Eval-Print Loop) is a simple, interactive computer programming environment. Programmers use it for quick coding trials, testing snippets, or learning a language's syntax without creating a full application.

## How to:
Launching Kotlin's REPL is a breeze. Open your terminal and type `kotlinc`. You'll land in the Kotlin shell. Let's try defining a variable and printing its value:

```kotlin
Welcome to Kotlin version 1.7.10 (JRE 1.8.0_292-b10)
Type :help for help, :quit for quit
>>> val greeting = "Hello, Kotlin REPL!"
>>> println(greeting)
Hello, Kotlin REPL!
```

## Deep Dive
Kotlin's REPL debuted with the language to encourage experimentation. It's similar to Python's interactive shell but tailored for Kotlin's syntax and peculiarities. Alternatives? Interactive environments in IDEs, such as IntelliJ IDEA, and online Kotlin playgrounds. The REPL works by compiling code on-the-fly, providing instant feedback – crucial for learning and debugging.

## See Also
- Kotlin documentation on REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Try Kotlin in the browser: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- JetBrains Kotlin Playground plugin for IntelliJ IDEA.
