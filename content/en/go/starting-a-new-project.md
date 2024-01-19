---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new Go project is about setting the foundation, which includes creating the main.go file, outlining the package structure, as well as writing the initial import statements and functions. Programmers initiate new projects to transform their innovative ideas into valuable applications.

## How to:

Kick-starting a Go project is straightforward. Let's see some essentials.

``` Go
// main.go
package main

import "fmt"

func main() {
    fmt.Println("Hello, Gophers!")
}
```
Running this will output: `Hello, Gophers!`. Cool, yeah?

Organizing your project? The following structure is commonly used.

``` 
/myproject
  /pkg      # libraries and packages
  /cmd      # application executables
  /api      # API types and protocols
  /web      # web apps
  /scripts  # scripts and data
```
## Deep Dive

The Go programming language (a.k.a Golang), developed at Google, leans heavily on simplicity and practicality. As of March 2012, when Go 1 was launched, the language has prioritized compatibility, promising that programs written in one version will continue to compile and run correctly in future versions.

Alternatives? You might consider Python, Ruby, or JavaScript - yet none offer Go's unique combination of simplicity, strong static type safety, first-class functions, and top-tier performance.

The "go mod init" command is worth noting. It creates a new module, initializing the go.mod file that tracks your project's dependencies. It enables the Go 1.11's module mode offering more control over versions used in the project.

## See Also

For more on Go project structuring guidelines, check out [Standard Go Project Layout](https://github.com/golang-standards/project-layout). 

The Go [Doc](https://golang.org/doc/) is your go-to place for information straight from the source. 

Lastly, the Go [GitHub repository](https://github.com/golang/go) is always a treasure trove for Go programmers.