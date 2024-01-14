---
title:    "Go recipe: Starting a new project"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project can be an exciting and fulfilling experience for a programmer. It allows you to explore new ideas, learn new skills, and potentially contribute something valuable to the open source community. Plus, who doesn't love the rush of creating something from scratch?

## How To

To start a new project in Go, follow these simple steps:

1. Create a new directory for your project and navigate into it.
2. In the terminal, use the command `go mod init <moduleName>` to initialize a go.mod file for your project. The module name can be anything you want, but it's recommended to use a URL-style name (e.g. github.com/username/projectname).
3. Next, open your favorite text editor and create a new file `main.go` within your project directory.
4. Inside `main.go`, import the `fmt` package and create a `main` function. This is where our code will go.
5. Now we can start writing some code. Let's print out a simple "Hello World" message:

```Go
package main

import "fmt"

func main() {
	fmt.Println("Hello World!")
}
```

6. Save the file and in the terminal, use the command `go run main.go` to run our program. You should see the message "Hello World!" printed out.
7. Congratulations, you have successfully started a new project in Go!

## Deep Dive

When starting a new project in Go, it's important to keep a few things in mind:

- Choose a clear and descriptive module name. This will make it easier for others to find and use your project.
- Utilize the `go.mod` file to declare your project dependencies. This helps keep track of the packages your project relies on and ensures that everyone working on the project is using the same versions.
- Write clean and well-documented code. This not only makes it easier for others to understand and contribute to your project, but it also helps with debugging and maintenance in the long run.

## See Also

- [Official Go Documentation](https://golang.org/doc/)
- [Effective Go](https://golang.org/doc/effective_go.html)
- [Go by Example](https://gobyexample.com/)
- [Go Projects on Github](https://github.com/golang/go/wiki/Projects)
- [Awesome Go](https://awesome-go.com/) (a curated list of Go frameworks, libraries, and software)