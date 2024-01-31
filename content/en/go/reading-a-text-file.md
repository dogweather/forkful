---
title:                "Reading a text file"
date:                  2024-01-20T17:54:15.050746-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading a text file"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file is grabbing the data stashed inside the file on your disk. Programmers do it to process logs, configs, user data – you name it – because that's where the action often is: the data.

## How to:

Reading a file in Go is straightforward. Use the `ioutil` package for a quick solution, or go with `os` and `bufio` for more control. Here's the `ioutil` way, easy-peasy:

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    data, err := ioutil.ReadFile("example.txt")
    if err != nil {
        panic(err)
    }
    fmt.Println(string(data))
}
```

For more finesse, let's get hands-on with `os` and `bufio`:

```Go
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        panic(err)
    }
}
```

In both cases, replace "example.txt" with your file's name. Run the code, and it'll spit out the file's contents.

## Deep Dive

Originally, Go's `ioutil.ReadFile` was the go-to for quick file reads. It's a one-liner, but it reads the whole file at once. Not ideal for huge text files where memory is a concern.

Enter `os` and `bufio`. They allow you to stream the file, handling it line by line. This means you can process gigabytes without breaking a sweat (or your app).

Alternatives? Sure. There are packages like `afero` for a consistent file system interface, which can be handy for testing.

A bit of implementation detail: `bufio.Scanner` has a default max token size (usually a line), so super long lines might need special handling. Tune it with `scanner.Buffer()` if you run into this edge case.

## See Also

- To get into the nitty-gritty, check the Go [package documentation for ioutil](https://pkg.go.dev/io/ioutil), [os](https://pkg.go.dev/os), and [bufio](https://pkg.go.dev/bufio).
- Curious about `afero`? Here's the [GitHub repo](https://github.com/spf13/afero).
