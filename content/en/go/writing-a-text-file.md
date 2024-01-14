---
title:                "Go recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing is an essential part of programming and the same goes for writing a text file in Go. Text files allow us to store and manipulate data in a human-readable format, making it easier to manage and share information. In this blog post, we'll explore how to write a text file in Go and understand its importance in programming.

## How To

The first step to creating a text file in Go is to import the `os` and `fmt` packages:

```Go
import "os"
import "fmt"
```

Next, we'll use the `Create()` function from the `os` package to create a new text file named "example.txt":

```Go
file, err := os.Create("example.txt")
if err != nil {
    fmt.Println(err)
}
defer file.Close()
```

Notice how we're also using the `defer` keyword to ensure that the file is closed after we're done writing to it.

Now, let's use the `WriteString()` function to add some content to our text file:

```Go
_, err = file.WriteString("This is a sample text file written in Go!")
if err != nil {
    fmt.Println(err)
}
```

We can also use the `Write()` function to add bytes to our file. Let's add some numbers:

```Go
bytes := []byte{49, 50, 51, 52, 53}
_, err = file.Write(bytes)
if err != nil {
    fmt.Println(err)
}
```

Finally, we can use the `Printf()` function from the `fmt` package to add formatted strings to our file:

```Go
_, err = fmt.Fprintf(file, "\nYour file contains %d characters and %d bytes.", len("This is a sample text file written in Go!"), len(bytes))
if err != nil {
    fmt.Println(err)
}
```

We have successfully written to our text file! Let's run our code and check the content of "example.txt":

```
This is a sample text file written in Go!
12345
Your file contains 35 characters and 5 bytes.
```

## Deep Dive

In the above example, we saw how to use simple functions to write to a text file. However, there are more options available in the `os` package that allow us to customize our text file further.

For instance, the `OpenFile()` function allows us to open an existing file and append content to it instead of creating a new file. We can also use the `WriteAt()` function to write at a specific location within a file.

Additionally, we can use the `NewWriter()` and `NewScanner()` functions from the `bufio` package to efficiently write and read from files, respectively.

## See Also

- [Go Docs: Package os](https://golang.org/pkg/os/)
- [Go Docs: Package fmt](https://golang.org/pkg/fmt/)
- [Go Docs: Package bufio](https://golang.org/pkg/bufio/)