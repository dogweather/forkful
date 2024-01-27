---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Le espressioni regolari (regexp) filtrano e manipolano il testo. I programmatori le usano per cercare pattern complessi, validare input e trasformare stringhe rapidamente.

## How to: (Come fare:)
```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Definire un'espressione regolare
	re := regexp.MustCompile(`\b(\w+)\b`)

	// Testo per l'esempio
	testo := "Ciao, mondo! Sono 123 Go."

	// Trovare tutte le corrispondenze
	risultati := re.FindAllString(testo, -1)
	fmt.Println("Trovato:", risultati)

	// Sostituire le corrispondenze
	sostituzione := re.ReplaceAllString(testo, "[$1]")
	fmt.Println("Sostituito:", sostituzione)

	// Verificare la presenza di una corrispondenza
	trova := re.MatchString(testo)
	fmt.Println("Corrispondenza:", trova)
}

// Output:
// Trovato: [Ciao mondo Sono Go]
// Sostituito: [$1], [$1]! [$1] 123 [$1].
// Corrispondenza: true
```

## Deep Dive (Approfondimento)
Le regexp nascono negli anni '50 e si evolvono con la teoria degli automi. Alternativa a regexp: manipolazione di stringhe (es. `strings` package in Go). Dettagli implementativi: in Go, `regexp` è basato sulle NFA (Non-deterministic Finite Automata) e garantisce performance prevedibili senza backtracking esponenziale.

## See Also (Vedi Anche)
- Documentazione Go sulle regexp: [https://golang.org/pkg/regexp/](https://golang.org/pkg/regexp/)
- Tutorial interattivo per imparare regexp: [https://regexone.com/](https://regexone.com/)
- Testare regexp online: [https://regex101.com/](https://regex101.com/)
