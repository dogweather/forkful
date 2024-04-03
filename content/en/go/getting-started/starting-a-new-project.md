---
date: 2024-02-03 17:50:05.177003-07:00
description: "How to: First, ensure you have Go installed by running `go version`\
  \ in your terminal. You should see the version of Go you've installed as output.\
  \ Next,\u2026"
lastmod: '2024-03-13T22:44:59.631734-06:00'
model: gpt-4-0125-preview
summary: First, ensure you have Go installed by running `go version` in your terminal.
title: Starting a new project
weight: 1
---

## How to:
First, ensure you have Go installed by running `go version` in your terminal. You should see the version of Go you've installed as output. Next, let's start a new project. Navigate to your workspace and run:

```shell
mkdir hello-world
cd hello-world
```

This creates and moves you into a new directory for your project. Now, initialize the module:

```shell
go mod init example.com/hello-world
```

Replace `example.com/hello-world` with your module path. This command creates a `go.mod` file in your directory, signaling the start of a new Go module. Here's what `go.mod` might look like:

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` tracks your project's dependencies. Now, create a `main.go` file:

```shell
touch main.go
```

Open `main.go` in your favorite editor and add the following code to print "Hello, World!":

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

To run your program, navigate back to the terminal and execute:

```shell
go run main.go
```

You should see:

```plaintext
Hello, World!
```

Congratulations! You've just started a new Go project and ran your first Go program.

## Deep Dive
The initiative to introduce modules as the standard for dependency management in Go was a significant shift in the Go ecosystem, officially adopted in Go 1.11. Before modules, Go developers relied on the GOPATH environment variable to manage dependencies, which was less intuitive and often led to the infamous "dependency hell."

Modules provide an encapsulated way to manage project dependencies, versioning, and are a move towards making Go projects more self-contained and portable. Each module specifies its dependencies which Go tracks in the `go.mod` file, simplifying dependency management across different environments and development stages.

However, it's worth noting that while Go modules are now the standard, some legacy projects might still use GOPATH. For most new projects, modules offer a more straightforward and effective management system, but understanding GOPATH can be handy for maintaining or contributing to older Go codebases.

In terms of alternatives, while Go modules are now the de facto standard, the Go community has experimented with other dependency management tools like `dep` in the past. However, these have largely been superseded by the official module support integrated into the Go toolchain.
