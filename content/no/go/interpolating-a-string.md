---
title:                "Interpolering av en streng"
date:                  2024-01-20T17:50:58.867566-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Strenginterpolasjon betyr å sette variabler inn i strenger. Vi gjør det for å bygge dynamisk tekst, som brukernavn i meldinger.

## How to:
I Go kan du bruke `fmt.Sprintf` for strenginterpolasjon. Her er et enkelt eksempel:

```go
package main

import (
	"fmt"
)

func main() {
	user := "Ola"
	message := fmt.Sprintf("Hei, %s! Velkommen tilbake.", user)
	fmt.Println(message)
}
```

Kjøre dette gir følgende utskrift:

```
Hei, Ola! Velkommen tilbake.
```

## Deep Dive
Før Go kom på banen, brukte språk som Python `%`-formatting eller `.format()` metoden, mens JavaScript brukte konkatenasjon med `+` eller template literals. Go introduserte `fmt`-biblioteket for enkel formatering. Alternativt kan du bruke plustegnet `+` for å sette sammen strenger, men `fmt.Sprintf` er mer effektivt når det er flere variabler.

Strenginterpolasjon i Go gjøres internt ved å parse formatstrengen og erstatte format-spesifierere (som `%s` for strenger) med tilsvarende argumentverdier. Dette gjør at du kan formatere ulike typer data enkelt.

## See Also
- Go's `fmt` package documentation: https://golang.org/pkg/fmt/
- Go by Example - String Formatting: https://gobyexample.com/string-formatting
- The Go Blog - Strings, bytes, runes and characters in Go: https://blog.golang.org/strings
