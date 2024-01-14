---
title:                "Go recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Are you looking to start a new project in Go? With its simple syntax, efficient performance, and cross-platform compatibility, Go has become a popular choice for developers. Whether you're a beginner or an experienced programmer, starting a new project in Go can be a rewarding experience. Let's dive in and see how you can get started.

## How To

To begin a new project in Go, you'll first need to set up your development environment. Install Go on your system by following the installation instructions from the official Go website. Once you have Go installed, you can create a new project directory.

```Go
mkdir myproject
```

Next, navigate to the project directory and create a new file named `main.go` which will serve as the entry point to your project.

```Go
cd myproject
touch main.go
```

Now, open the `main.go` file in your preferred code editor and add the following code:

```Go
package main

import "fmt"

func main() {
	fmt.Println("Hello World!")
}
```

This simple program will print "Hello World!" to the console when executed. To run this program, open your terminal, navigate to the project directory, and use the `go run` command.

```Go
go run main.go
```

Congratulations, you've just written and executed your first Go program! From here, you can continue developing and building your project using Go's powerful standard library and third-party packages.

## Deep Dive

Now that you know how to get started with a new project in Go, let's dive a little deeper into some best practices. It's recommended to use the `go mod` command to manage dependencies for your project. This will create a `go.mod` file which will keep track of your project's dependencies and their versions.

```Go
go mod init myproject
```

You can then use the `go get` command to install any external packages your project may require.

```Go
go get github.com/example/package
```

It's also a good idea to write tests for your code using Go's built-in testing framework. Simply create a new file with the suffix `_test.go` and add your tests to it. You can then use the `go test` command to run all your tests.

```Go
go test
```

For more in-depth information on starting a new project in Go, be sure to check out the official documentation and online resources such as tutorials, forums, and blogs.

## See Also

- [Official Go Documentation](https://golang.org/doc/)
- [A Tour of Go](https://tour.golang.org/welcome/1)
- [Go Forum](https://forum.golangbridge.org/c/tutorial)
- [Go Blog](https://blog.golang.org/)