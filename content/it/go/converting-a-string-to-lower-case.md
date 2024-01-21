---
title:                "Conversione di una stringa in minuscolo"
date:                  2024-01-20T17:38:16.262179-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una stringa in minuscolo"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Con la conversione in minuscolo trasformi tutti i caratteri di una stringa in lettere minuscole. Gli sviluppatori lo fanno per uniformare i dati, facilitare i confronti fra stringhe e ignorare le differenze di maiuscole/minuscole.

## How to:
Go ha una funzione pronta all'uso, `strings.ToLower`, che fa il lavoro per te. Ecco come si usa:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originale := "Ciao Mondo!"
	convertita := strings.ToLower(originale)
	fmt.Println(convertita) // "ciao mondo!"
}
```
Output:
```
ciao mondo!
```

## Deep Dive
La necessità di convertire le stringhe in minuscolo risale ai primi giorni dell'informatica, quando i confronti tra testi erano influenzati dalla capitalizzazione. La funzione `ToLower` di Go è un'eredità di tale esigenza.

Alternative? Potresti convertire manualmente ogni carattere usando un ciclo e le tabelle Unicode, ma, davvero, perché complicarsi la vita? 

In termini di implementazione, `strings.ToLower` considera le peculiarità di diverse linghe e culture grazie all'Unicode standard, che è molto più complesso delle vecchie mappature ASCII.

## See Also
- Documentazione Go per `strings.ToLower`: https://pkg.go.dev/strings#ToLower
- Un confronto su Unicode e ASCII: https://en.wikipedia.org/wiki/Unicode
- Istruzioni sul confronto di stringhe in Go: https://golang.org/pkg/strings/#Compare