---
date: 2024-01-25 03:39:58.655752-07:00
description: "A REPL (Read-Eval-Print Loop) is an interactive shell that processes\
  \ single user inputs, executes code, and returns the result. Programmers use it\
  \ for\u2026"
lastmod: 2024-02-19 22:05:18.442175
model: gpt-4-1106-preview
summary: "A REPL (Read-Eval-Print Loop) is an interactive shell that processes single\
  \ user inputs, executes code, and returns the result. Programmers use it for\u2026"
title: Using an interactive shell (REPL)
---

{{< edit_this_page >}}

## What & Why?
A REPL (Read-Eval-Print Loop) is an interactive shell that processes single user inputs, executes code, and returns the result. Programmers use it for quick experiments, debugging, or learning, as it allows for immediate feedback and iteration.

## How to:
Starting a REPL in Java is simple with the `jshell` tool introduced in Java 9. Here's how to get your hands on it and start a basic session:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  created method sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Exit any time with `/exit`.

```Java
jshell> /exit
|  Goodbye
```

## Deep Dive
Before `jshell`, Java programmers didn't have an official REPL, unlike Python or Ruby devs. They used IDEs or wrote full programs even for trivial tasks. `jshell` was a game-changer as of Java 9, bridging that gap.

Alternatives include online compilers or IDE plugins, but they don't match `jshell`'s immediacy. As for internals, `jshell` uses the Java Compiler API to execute code fragments, which is pretty neat. It's more than a playground—it can import libraries, define classes, and more. This makes it a robust tool for prototyping.

## See Also
- [JShell User's Guide](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java Platform, Standard Edition Tools Reference](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
