---
title:                "Starting a new project"
html_title:           "Go recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Starting a new project in Go means beginning a new coding endeavor. Programmers do this to create new software, applications, or programs using the Go programming language.

## How to:
To create a new project in Go, use the `go` command followed by `new` and the name of your project.
```
Go new project_name
```
This will create a new folder called `project_name` with a basic Go file inside. To run this file, use the `go` command followed by `run`.
```
Go run project_name.go
```
This will compile and execute your Go code, showing the output in your command line.

## Deep Dive:
- Historical Context: Go was first developed by Google in 2007 and officially released to the public in 2009. It was created as an alternative to existing programming languages, with the main focus being on simplicity, concurrency, and efficiency.
- Alternatives: Some other popular programming languages for starting new projects include Java, Python, and C++. However, Go offers unique features such as built-in concurrency and garbage collection.
- Implementation Details: When creating a new project in Go, the `go` command will automatically generate a `go.mod` file, which is used for managing dependencies. This allows for easier sharing and collaboration on projects.

## See Also:
- [Official Go documentation](https://golang.org/doc/)
- [Go project structure overview](https://tutorialedge.net/golang/go-project-structure-best-practices/)
- [Go vs. other programming languages](https://www.freecodecamp.org/news/want-to-learn-go-heres-a-free-24-part-course-to-get-you-started/)