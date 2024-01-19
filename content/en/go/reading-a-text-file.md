---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is pulling in and accessing information from a file stored as text on your computer. Programmers do it to interact with and manipulate data.

## How to:

Simple text file reading in Go involves the `os` and `bufio` packages.

```Go
package main

import (
	"fmt"
	"bufio"
	"os"
)

func main() {
    file, err := os.Open("test.txt")

    if err != nil {
        log.Fatalf("failed to open file: %s", err)
    }

    scanner := bufio.NewScanner(file)
    scanner.Split(bufio.ScanLines)

    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    file.Close()
}
```

This script chooses a file (`test.txt`), checks it can be opened, scans it line by line, prints each line, then finally closes the file.

## Deep Dive:

- **Historical Context:** File reading has been around since programmers starting storing data in files. It's a basic but vital operation.

- **Alternatives:** 
    - `ioutil` package: In older Go versions, the `ioutil.ReadFile` function was a simpler way to read a file. But it's been deprecated in Go 1.16.
    - `os` package: Apart from `os.Open`, you can also use `os.ReadFile` to directly get the file content.

- **Implementation Details:** `bufio.Scanner` is efficient for files with smaller lines as it buffers the input. For large files or if you want more control, consider `bufio.Reader`.

## See Also:

- Full Go documentation on file reading: [Go Docs on package os](https://golang.org/pkg/os/)
- Walkthrough on Go file operations: [File handling in Go](https://www.golangprograms.com/go-language/file-handling)
- Good comparison of `ioutil` vs `os`: [Reading Files in Go - The Full Guide](https://levelup.gitconnected.com/reading-files-in-go-the-full-guide-83f59ab5e7a9)