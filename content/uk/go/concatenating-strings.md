---
title:                "Об'єднання рядків"
date:                  2024-01-20T17:34:45.311858-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
## Що і Чому?

String concatenation? It's when you join strings end-to-end. Programmers do it to craft messages, combine data, and build paths. Essential, like salt in cooking.

## How to:
## Як робити:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
    // Simple plus operator
    greeting := "Привіт, " + "світ!"
    fmt.Println(greeting) // Привіт, світ!

    // Join() from strings package
    words := []string{"Go", "loves", "simplicity."}
    sentence := strings.Join(words, " ")
    fmt.Println(sentence) // Go loves simplicity.

    // sprintf
    name := "Дмитро"
    welcome := fmt.Sprintf("Вітаю, %s!", name)
    fmt.Println(welcome) // Вітаю, Дмитро!
}
```

## Deep Dive:
## Поглиблений Занурення:

Concatenating strings is easy, but doing it wisely is an art. Back in the day, programmers abused '+' cause it's handy, but it can hurt performance when overused due to memory reallocation. Go's `strings.Builder` is your friend for heavy-duty jobs because it minimizes memory copying. And remember, `Join()` beats pluses for a slice of strings; it's cleaner and quicker.

## See Also:
## Дивись Також:

- Go Doc for strings package: [pkg.go.dev/strings](https://pkg.go.dev/strings)
- Effective Go: [golang.org/doc/effective_go#strings](https://golang.org/doc/effective_go#strings)
- Go Blog on string handling: [blog.golang.org/strings](https://blog.golang.org/strings)
