---
title:                "Starting a new project"
date:                  2024-01-20T18:03:33.389753-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Starting a new project means setting up the foundation for your Go application. Programmers do this to organize code, manage dependencies, and set the stage for further development.

## How to:
Firstly, install Go, if you haven't already, from [golang.org](https://golang.org/dl/). Then, set up a new project:

1. Open a terminal.
2. Create a new directory.

   ```bash
   mkdir myproject
   cd myproject
   ```

3. Initialize the module:

   ```bash
   go mod init github.com/yourusername/myproject
   ```

4. Write a simple `main.go` file:

   ```Go
   package main

   import "fmt"

   func main() {
       fmt.Println("Hello, new world of Go!")
   }
   ```

5. Run the program:

   ```bash
   go run main.go
   ```

Sample output should be:

```
Hello, new world of Go!
```

## Deep Dive
Starting a new project in Go has evolved. Early Go projects didn't have an official package management system. This led to the "GOPATH" workspace model, which could get messy with larger projects. Nowadays, with `go mod` introduced in Go 1.11, things are more streamlined and manageable: dependencies are handled per project, not globally.

Alternatives to `go mod` are fading, but they included community tools like `dep` and `glide`. These days, `go mod` is the recommended tool due to its first-party support and integration with the Go toolchain.

When you run `go mod init`, Go creates a new `go.mod` file. This file tracks your project's dependencies. It automatically lists Go's version and any external packages you add later. With this setup, your code's dependencies are explicit and reproducible, helping avoid the "works on my machine" syndrome.

## See Also
- [Getting Started with Go](https://golang.org/doc/install)
- [How to Write Go Code](https://golang.org/doc/code.html)
- [`go mod` Documentation](https://golang.org/ref/mod)