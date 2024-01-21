---
title:                "Створення тимчасового файлу"
date:                  2024-01-20T17:40:29.885973-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке & Навіщо?

Creating a temporary file is setting up a short-lived storage in the computer's file system. Programmers do it to save data that's only needed during a particular operation or session — like a scratchpad for the computer's thoughts.

## How to:
Як це зробити:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	// Create a temporary file
	tmpfile, err := ioutil.TempFile("", "sample")
	if err != nil {
		log.Fatal(err)
	}

	// Remember to clean up!
	defer os.Remove(tmpfile.Name())

	// Write something to the file
	content := []byte("Temporary file's content.\n")
	if _, err := tmpfile.Write(content); err != nil {
		log.Fatal(err)
	}

	// Close the file
	if err := tmpfile.Close(); err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Temporary file created: %s\n", tmpfile.Name())
}
```

**Sample Output:**
```
Temporary file created: /tmp/sample123456
```

## Deep Dive
Глибоке занурення:

Historically, temporary files provided a way to handle large data without hogging memory — important in the days of limited RAM. Alternatives to creating temporary files include using in-memory data structures, which are faster but use more RAM. For implementation, Go's `ioutil.TempFile` function is simple and secure. It generates a unique filename to avoid conflicts and places the file in the system's temp directory.

## See Also
Дивись також:

- [Go by Example: Temporary Files and Directories](https://gobyexample.com/temporary-files-and-directories)
- [Package ioutil documentation](https://pkg.go.dev/io/ioutil)
- [The Go Blog: Defer, Panic, and Recover](https://blog.golang.org/defer-panic-and-recover)