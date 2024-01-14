---
title:    "Go recipe: Writing to standard error"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

As programmers, we are always striving to write clean and efficient code. But what about when our code encounters an error? This is where writing to standard error comes in. By printing error messages to the standard error output, we can easily identify and troubleshoot any issues our code may encounter.

## How To

Writing to standard error in Go is a simple process. First, we need to import the "os" package, which allows us to access the standard error output stream. Then, we can use the "fmt" package to format and print our error message to the standard error stream. Let's take a look at an example:

```
package main

import (
  "fmt"
  "os"
)

func main() {
  _, err := os.Open("nonexistentfile.txt")
  if err != nil {
    fmt.Fprintln(os.Stderr, "Error opening file:", err)
  }
}
```

In this example, we are attempting to open a non-existent file. If an error occurs, we use the "fmt.Fprintln()" function to print our error message to the standard error stream. The "os.Stderr" parameter specifies that we want to print to the standard error output instead of the standard output.

Running this code will produce the following output:

```
Error opening file: open nonexistentfile.txt: no such file or directory
```

As you can see, our error message is printed to the standard error stream, allowing us to quickly identify and handle the issue.

## Deep Dive

Writing to standard error can be useful for more than just printing error messages. It can also be used to log important information and debug our code. By printing to the standard error stream, we can keep our standard output clean and focused on the intended output of our program.

One thing to keep in mind when writing to standard error is that it is typically used for printing unformatted messages. This means that we should avoid using any colors or special formatting in our error messages, as it may not be properly displayed depending on the terminal being used.

## See Also

- [Golang "os" package documentation](https://golang.org/pkg/os/)
- [Golang "fmt" package documentation](https://golang.org/pkg/fmt/)
- [Writing to standard error in Go tutorial](https://www.digitalocean.com/community/tutorials/how-to-use-variables-and-strings-in-go)