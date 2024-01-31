---
title:                "Handling errors"
date:                  2024-01-21T21:19:40.750818-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/handling-errors.md"
changelog:
  - 2024-01-21, dogweather, Reviewed for accuracy
---

{{< edit_this_page >}}

## What & Why?

Error handling in Go is about gracefully catching and responding to runtime hiccups. We do it to prevent crashes and ensure our programs act predictably, even when things go south.

## How to:

Go uses explicit error handling. That means you'll check if a function returns an error every time you call it. No Exceptions. Here's how it looks:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Uh oh:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Pretending something went wrong
	return fmt.Errorf("something went wrong")
}
```

Run this, and you'll get:

```
Uh oh: something went wrong
```

But what if it succeeds?

```Go
func doSomething() error {
	// All good this time
	return nil
}
```

No output. Cool, no news is good news.

## Deep Dive:

In Go, error handling has been a point of contention. Since the beginning, Go decided against exceptions for a more explicit approach, which some developers love for its simplicity and others find verbose. The built-in `error` type is an interface. Any type with a `Error() string` method satisfies it. This ties in with Go's ethos of simplicity and explicitness.

Alternatives? There's the `panic` and `recover` duo, but they're for exceptional cases (pun intended) when the program can't continue. Think of `panic` as the eject button you hit when you know there's no coming back. Use it sparingly.

As for mainstream error handling, Go 1.13 introduced error wrapping, making it easier to figure out the "error chain" with functions like `errors.Is()` and `errors.As()`.

## See Also:

For all things error handling in Go:

- The Go Blog on Error Handling: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Effective Go – Error handling section: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13 Error Wrapping documentation: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Dave Cheney's post on error handling strategies: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
