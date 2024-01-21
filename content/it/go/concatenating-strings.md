---
title:                "Concatenazione di stringhe"
date:                  2024-01-20T17:34:41.630236-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenazione di stringhe"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Concatenare stringhe significa unirle in una sola. Lo facciamo per costruire frasi dinamiche, manipolare testo, o per creare formati di dati più complessi tipo JSON.

## How to: (Come Fare:)
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Metodo semplice con l'operatore +
	saluto := "Ciao, " + "mondo!"
	fmt.Println(saluto) // Output: Ciao, mondo!

	// Utilizzando fmt.Sprintf
	nome := "Luca"
	eta := 30
	messaggio := fmt.Sprintf("%s ha %d anni.", nome, eta)
	fmt.Println(messaggio) // Output: Luca ha 30 anni.

	// Usando il builder di stringhe per concatenazioni multiple
	var builder strings.Builder
	frammenti := []string{"Go", "è", "fantastico", "!"}
	for _, v := range frammenti {
		builder.WriteString(v)
		builder.WriteString(" ")
	}
	fmt.Println(builder.String()) // Output: Go è fantastico !
}
```

## Deep Dive (Approfondimento)
Historically, string concatenation could be a costly operation due to memory allocations and copies. In Go, the `+` operator is efficient for simple cases, but not for multiple concatenations in a loop. Here, `strings.Builder` and `bytes.Buffer` shine, minimizing allocations.

Alternatives to `+` such as `fmt.Sprintf`, `strings.Join`, or even `bytes.Buffer` should be considered when building complex strings or when performance is a concern.

Internally, `strings.Builder` is optimized for minimal memory copying, and since Go 1.10, it's the recommended way for joining strings when performance matters.

## See Also (Vedi Anche)
- The Go Blog, "Strings, bytes, runes and characters in Go": https://blog.golang.org/strings
- Go By Example, "String Formatting": https://gobyexample.com/string-formatting
- Go documentation on `strings.Builder`: https://pkg.go.dev/strings#Builder