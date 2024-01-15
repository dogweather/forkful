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

## Why

Starting a new project is an exciting and challenging endeavor that can expand your skills as a coder and bring your ideas to life. With Go, the newest version of the popular programming language, you have a powerful tool at your disposal to create efficient, reliable, and scalable projects.

## How to

To start a new project in Go, you will need to have the latest version installed on your computer. Once you have that, you can create a new project by following these steps:

1. Create a new folder for your project.
2. Inside the folder, create a new Go file with the extension `.go`.
3. Open the file in your preferred code editor.
4. Begin by importing the necessary packages using the `import` statement.
5. Define the `main` function, which is where your code will start executing.
6. You can now write your code within the `main` function using the syntax of Go.
7. When you are ready to run your code, use the `go run` command followed by the name of your file. For example, `go run main.go`.

Here is an example of a simple "Hello World" program in Go, where we import the `fmt` package and use the `Printf` function to print a message to the console:

```
package main

import (
  "fmt"
)

func main() {
  fmt.Printf("Hello World!")
}
```

Running this code will output `Hello World!` to the console.

## Deep Dive

When starting a new project in Go, it's important to understand the structure of a Go program. Here are the key components to keep in mind:

- A Go program starts with the `package main` statement, indicating that this is the main package for your project.
- The `import` statement allows you to access external packages that contain functions, data types, and other resources.
- The `func` keyword is used to define functions in Go. The `main` function is the starting point of your program, but you can create other functions as needed.
- The `Printf` function from the `fmt` package allows you to format and print messages to the console. There are many other useful functions in this package that you can explore.
- Use `go build` to compile your code into an executable file, which can be executed using the command `./yourExecutableName`.

With these basic concepts in mind, you are ready to start writing your own projects in Go!

## See Also

- [Official Go website](https://golang.org/)
- [Go documentation](https://golang.org/doc/)
- [Go code examples](https://github.com/golang/example)
- [Go tutorials](https://golangbot.com/)