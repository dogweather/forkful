---
title:                "Writing a text file"
html_title:           "Go recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file may seem like a mundane task, but it is a fundamental aspect of programming. Whether you are creating configuration files, saving user input, or generating reports, text files are an essential part of most applications.

## How To

To write a text file in Go, we first need to import the `os` package, which provides functionality for interacting with the operating system. Then, we can use the `Create` method from the `os` package to create a file and open it for writing. Finally, we can use the `WriteString` method to write our desired text to the file and close it when we are finished. Let's take a look at an example:

```
package main

import (
    "fmt"
    "os"
)

func main() {
    // Create a file named "my-file.txt" in the current directory
    file, err := os.Create("my-file.txt")
    // Check for any errors
    if err != nil {
        fmt.Println(err)
    }
    // Write "Hello, World!" to the file
    _, err = file.WriteString("Hello, World!")
    // Check for any errors
    if err != nil {
        fmt.Println(err)
    }
    // Close the file
    file.Close()
}
```

Running this code will create a file named "my-file.txt" in the same directory as our program, and when we open it, we will see the text "Hello, World!".

## Deep Dive

When writing a text file in Go, we have a few other methods besides `WriteString` that we can use:

- `Write` - This method takes a byte slice as input and writes it to the file.
- `WriteAt` - This method allows us to write at a specific position in the file.
- `WriteByte` - This method takes a single byte as input and writes it to the file.

Additionally, we can also use the `Format` function from the `fmt` package to format our data before writing it to the file. This allows us to include variables, values, and other data types in our text file. For example:

```
// Create a file named "my-file.txt" in the current directory
file, err := os.Create("my-file.txt")
// Check for any errors
if err != nil {
    fmt.Println(err)
}
// Write formatted text to the file
fmt.Fprintf(file, "Today is %s and the time is %d", "Monday", 10)
```

This will write the text "Today is Monday and the time is 10" to our file.

## See Also

- [Go's os package documentation](https://golang.org/pkg/os/)
- [A Beginner's Guide to Writing Files in Go](https://www.digitalocean.com/community/tutorials/how-to-write-files-in-go)