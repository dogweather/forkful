---
title:                "Ricerca e sostituzione del testo"
date:                  2024-01-20T17:57:51.451048-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e perché?)
Cercare e sostituire testo è l'arte di scovare stringhe e cambiarle con altre. I programmatori lo fanno per aggiornare codice, correggere errori, o modificare dati – velocemente e senza sbagli.

## How to: (Come fare:)
Ecco un esempio in Go per cercare e sostituire testo usando il pacchetto `strings`.

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalText := "Ciao, mondo! Programmare in Go è divertente."
	searchFor := "divertente"
	replaceWith := "fantastico"
	modifiedText := strings.Replace(originalText, searchFor, replaceWith, -1)
	fmt.Println(modifiedText)
}
```

Output:
```
Ciao, mondo! Programmare in Go è fantastico.
```

## Deep Dive (Approfondimento)
La funzione `strings.Replace` ha origine nelle librerie standard UNIX, dove `sed` faceva opere simili. Go offre `strings.Replace` per un singolo cambio, o `strings.ReplaceAll` per sostituire tutte le occorrenze. Attenzione alle performance con stringhe molto grandi — Go lavora meglio con `bytes.Buffer` o `strings.Builder` per modifiche pesanti.

## See Also (Vedi anche)
- Go by Example: strings.Replace [https://gobyexample.com/string-functions]
- Package strings documentation [https://golang.org/pkg/strings/]
- Dave Cheney: Clear is better than clever (blog post discussing code readability) [https://dave.cheney.net/practical-go/presentations/qcon-china.html]
