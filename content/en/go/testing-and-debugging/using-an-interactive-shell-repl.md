---
date: 2024-02-03 17:50:12.851556-07:00
description: "How to: While Go doesn\u2019t include a built-in REPL, the community\
  \ has created tools like `gore` to fill the gap. First, install `gore` by running."
lastmod: '2024-03-13T22:44:59.632592-06:00'
model: gpt-4-0125-preview
summary: "While Go doesn\u2019t include a built-in REPL, the community has created\
  \ tools like `gore` to fill the gap."
title: Using an interactive shell (REPL)
weight: 34
---

## How to:
While Go doesn’t include a built-in REPL, the community has created tools like `gore` to fill the gap. First, install `gore` by running:

```
$ go get -u github.com/motemen/gore
```

Once installed, launch `gore` by typing `gore` in your terminal:

```
$ gore
```

You should see a prompt ready to accept Go commands. Let’s try a simple example:

```
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
```

You’d see output like:

```
Hello, Go REPL!
```

Variables and function definitions work as expected. You can declare a function:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Area of circle with radius 4:", areaCircle(4))
```

And get the output right away:

```
Area of circle with radius 4: 50.26548245743669
```

## Deep Dive:
The concept of a REPL is ancient, tracing back to the Lisp machines of the 1960s, providing an interactive programming experience. Unlike languages like Python or JavaScript, Go was designed without a REPL, focusing instead on compiled binaries for performance and simplicity. This reflects Go's philosophy of simplicity and its design for scalable and maintainable software.

However, tools like `gore` or `goplay` showcase the Go community's resourcefulness in bridging this gap. These tools parse Go code dynamically and use the `go/eval` package or similar mechanisms to execute it in real-time, albeit with some limitations compared to a native REPL environment. These limitations stem from Go's type system and compilation model, which can make on-the-fly evaluation challenging.

While REPL environments are exceptionally useful for education and quick tests, the Go ecosystem typically gravitates towards traditional compile-and-run processes for most development tasks. IDEs and editors with Go support, like Visual Studio Code or GoLand, offer integrated tools for testing and debugging that alleviate much of the need for a REPL for professional development.

For explorative programming, prototyping, or learning, though, REPLs like `gore` offer a valuable alternative, allowing programmers accustomed to REPLs in other languages to enjoy a similar experience in Go.
