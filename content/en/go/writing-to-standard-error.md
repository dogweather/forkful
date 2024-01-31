---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) is how your program reports errors and warnings. Programmers do it to separate regular output (stdout) from error messages, making it easier to handle and track issues.

## How to:

In Go, you write to standard error using the `os` package's `os.Stderr` file descriptor. Here's how to do it:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	message := "Error: something went wrong!"
	_, err := fmt.Fprintln(os.Stderr, message)

	if err != nil {
		panic(err)
	}
}
```

Sample output to stderr might look like this:

```
Error: something went wrong!
```

## Deep Dive

Historically, Unix-like operating systems provide three standard streams: stdin, stdout, and stderr. Go inherits this concept. Alternatives include logging packages like `log` or `zap`, which offer more control over output format and destination. When writing to stderr directly, Go uses `os.Stderr`, which implements `io.Writer`, making it consistent with Go's general approach to I/O by providing a well-defined interface.

## See Also

- The Go Blog on error handling: https://blog.golang.org/error-handling-and-go
- `log` package documentation: https://golang.org/pkg/log/
- `zap` logger: https://godoc.org/go.uber.org/zap
