---
title:                "Reading command line arguments"
date:                  2024-01-20T17:55:54.432052-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading command line arguments"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments lets your program take input when it's run from a terminal, which can guide its behavior without hardcoding values. Programmers use it to customize software execution, handle user preferences, and respond to different operation modes.

## How to:

Go makes it pretty easy to grab those command line arguments using the `os` package. Here's how you do it:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:] // os.Args[0] is the path to the program itself
	for i, arg := range args {
		fmt.Printf("Argument %d: %s\n", i+1, arg)
	}
}
```

Run your program like this:

```
$ go run yourprogram.go these are command line args
```

And you'd get:

```
Argument 1: these
Argument 2: are
Argument 3: command line
Argument 4: args
```

That's it. You now have the power to influence your program’s behavior from the terminal. 

## Deep Dive

Long before GUIs, command line arguments were the standard for telling programs what to do. They stem from UNIX conventions, which Go inherits in part due to its POSIX-compliant environments relationship.

Alternatives for argument parsing in Go include using more sophisticated packages like `flag` for flags (e.g., `--name=value`) or third-party libraries such as `cobra` or `urfave/cli` for building complex CLI applications.

The `os.Args` slice captures all the arguments, with `os.Args[0]` being the program itself. Its simplicity is perfect for straightforward tasks, but beware of cases needing structured commands or flags.

## See Also

- The `flag` package for a more powerful option parsing: [https://pkg.go.dev/flag](https://pkg.go.dev/flag)
- Cobra for building powerful command-line applications: [https://github.com/spf13/cobra](https://github.com/spf13/cobra)
- `urfave/cli` for a simple, fast, and fun package for building CLIs in Go: [https://github.com/urfave/cli](https://github.com/urfave/cli)
