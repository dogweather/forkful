---
title:                "Estrazione di sottostringhe"
date:                  2024-01-20T17:45:52.345306-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Estrarre sottostringhe significa selezionare parti specifiche di una stringa. I programmatori lo fanno per analizzare dati, validare input, estrarre informazioni rilevanti o manipolare testi.

## How to:
Go utilizza il package `strings` per lavorare con le stringhe e offre semplici meccanismi per estrarre sottostringhe. Ecco alcuni esempi:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "Buon giorno, mondo!"
	
	// Utilizzare slice per estrarre sottostringhe
	substr1 := s[6:12] // "giorno"
	fmt.Println(substr1)

	// Estrarre una sottostringa con strings.Split
	parts := strings.Split(s, ",")
	substr2 := parts[0] // "Buon giorno"
	fmt.Println(substr2)

	// Uso di strings.TrimSpace per rimuovere spazi bianchi
	substr3 := strings.TrimSpace(parts[1]) // "mondo!"
	fmt.Println(substr3)
}
```

Output:
```
giorno
Buon giorno
mondo!
```

## Deep Dive
Le sottostringhe in Go sono gestite con l'indicizzazione e taglio di una stringa, nota come slicing. Questo era già possibile in linguaggi precedenti come C, ma Go lo rende più sicuro evitando errori di memoria comuni nella gestione delle stringhe a basso livello.

Alternativamente, tecniche come `strings.Split()` dividono una stringa in base a un separatore, mentre funzioni come `strings.TrimSpace()` sono utili per pulire la sottostringa dai caratteri indesiderati.

Dal punto di vista dell'implementazione, le stringhe in Go sono immutabili, ciò significa che ogni modifica produce una nuova stringa. Tuttavia, Go ottimizza questo comportamento per prevenire sprechi di memoria quando possibile.

## See Also
- Documentazione ufficiale Go su stringhe e slicing: [Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
- Articolo su manipolazione di stringhe in Go: [String handling in Go](https://www.calhoun.io/6-tips-for-using-strings-in-go/)
- Pacchetto strings GoDoc: [strings package](https://pkg.go.dev/strings)