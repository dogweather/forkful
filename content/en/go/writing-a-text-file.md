---
title:                "Writing a text file"
date:                  2024-01-19
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file means saving data in a file that contains text, usually in a human-readable format such as `.txt` or `.csv`. Programmers write files to save and persist data, which can be read by humans or used by other programs later.

## How to:

Here’s how you write a string to a text file in Go:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	message := "Hello, Go!"

	file, err := os.Create("example.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	_, err = file.WriteString(message)
	if err != nil {
		log.Fatal(err)
	}

	log.Println("File written successfully!")
}
```

Run it. If successful, you won't see errors but `example.txt` is created.

## Deep Dive

Writing to text files in Go uses the `os` package, which provides a platform-independent interface to operating system functionality. The `os.Create` function creates or truncates a file. The `File.WriteString` method is straightforward for writing strings.

Historically, text file handling evolved from C's `stdio.h` library. In Go, simplicity is key; you do less for more action, avoiding boilerplate. Alternatives like `ioutil.WriteFile` exist but aren't advised for big files due to memory inefficiency. `bufio` provides buffered I/O, reducing system calls and improving performance.

## See Also

- Go by Example: Writing Files: https://gobyexample.com/writing-files
- Go Documentation for the os package: https://pkg.go.dev/os
- Go `bufio` package: https://pkg.go.dev/bufio
