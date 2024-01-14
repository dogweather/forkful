---
title:                "Go recipe: Reading a text file"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Reading a text file may seem like a simple task, but it is an important skill to have in any programming language. In Go, reading a text file can be an essential step in many programs, such as data processing or file manipulation.

## How To
To read a text file in Go, we can use the `os` and `bufio` packages. Let's take a look at a simple example:

```
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    // Open the text file
    file, err := os.Open("test.txt")
    if err != nil {
        fmt.Println("Error opening file:", err)
        return
    }
    // Close the file when we're done
    defer file.Close()

    // Create a new scanner to read the file
    scanner := bufio.NewScanner(file)

    // Loop through each line in the file
    for scanner.Scan() {
        // Print out the line
        fmt.Println(scanner.Text())
    }

    // Check for any errors during scanning
    if err := scanner.Err(); err != nil {
        fmt.Println("Error scanning file:", err)
    }
}
```

In the code above, we first open the text file using the `os` package's `Open()` function. We also handle any errors that may occur during this process. Then, we use a `defer` statement to ensure that the file is closed once we're done reading it.

Next, we create a `scanner` using the `bufio` package. This allows us to read the file line by line, using the `Scan()` function. Inside our loop, we print out each line using the `Text()` method of the `scanner` object.

Finally, we check for any errors that may have occurred during the scanning process. And that's it! We have successfully read the text file.

## Deep Dive
Now, let's dive a bit deeper into the `Scanner` object and its various methods.

Firstly, the `Scan()` function reads the next line from the file and stores it in the `scanner` object. It also returns a boolean value, which is `true` as long as there is a line to be scanned.

Next, the `Text()` method simply returns the line that was scanned by the `Scan()` function. It is important to note that the line returned still includes the newline character at the end.

Lastly, the `Err()` method returns any errors that may have occurred during scanning. This allows us to handle any issues that may arise in a more graceful manner.

## See Also
- [GoDocs - os Package](https://golang.org/pkg/os/)
- [GoDocs - bufio Package](https://golang.org/pkg/bufio/)
- [Reading and Writing Files in Go](https://www.digitalocean.com/community/tutorials/reading-and-writing-files-in-go)