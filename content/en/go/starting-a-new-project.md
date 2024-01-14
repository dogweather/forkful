---
title:                "Go recipe: Starting a new project"
programming_language: "Go"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why
Starting a new project in Go can be an exciting and rewarding experience. Whether you are a beginner or an experienced programmer, Go offers a simple and powerful language that allows for efficient and scalable code. By creating a new project in Go, you can improve your skills, explore new concepts, and even create useful tools for yourself and others.

## How To
To start a new project in Go, you will need to have Go installed on your system. You can download and install Go from the official website [here](https://golang.org/doc/install). Once Go is installed, you can follow these steps to create a new project:

1. Create a new directory for your project.
2. Inside the project directory, create a file named `main.go`.
3. Open the `main.go` file in your preferred text editor.
4. Import the necessary packages (if any) by using the `import` statement.
5. Write your code using the `func main()` function.
6. Save the file and navigate to your project directory in the terminal.
7. Use the `go build` command to compile your code.
8. If there are no errors, use the `./main` command to run your program.
9. You can also use the `go run main.go` command to compile and run your code in one step.

Here is an example of a basic "Hello, World!" program in Go:

```Go
package main 

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

Output: `Hello, World!`

## Deep Dive
In addition to the basic steps outlined above, there are a few things to keep in mind when starting a new project in Go:

- You can use `go mod init <module_name>` to initialize a new module for your project. This will create a `go.mod` file that tracks your project's dependencies.
- Go has strict rules for organizing and formatting code, so it is important to follow the [official conventions](https://www.golang.org/doc/effective_go.html) to ensure readability and maintainability.
- Go has an excellent standard library with built-in packages for common tasks, such as working with text, networking, and file management. Be sure to explore and utilize these packages to make your code more efficient.
- Consider using a version control system like Git to track your changes and collaborate with others on your project.
- If you are stuck or need guidance, the official Go [documentation](https://golang.org/doc/) and [community](https://golang.org/community/) are great resources for learning and troubleshooting.

Starting a new project in Go can be a fun and fulfilling experience. With a little bit of practice and experimentation, you will be creating efficient and powerful code in no time.

## See Also
- [Official Go documentation](https://golang.org/doc/)
- [Effective Go](https://www.golang.org/doc/effective_go.html)
- [Go Community](https://golang.org/community/)