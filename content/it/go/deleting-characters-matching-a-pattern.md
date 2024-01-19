---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Eliminazione di Caratteri Corrispondenti a un Modello in Go

## Cos'è & Perché?
La cancellazione dei caratteri corrispondenti a un modello consiste nell'identificare e rimuovere tutti i caratteri in una stringa che soddisfano un certo modello o criterio. Lo facciamo per pulire ed elaborare i dati, migliorando la qualità e l'affidabilità delle informazioni.

## Come Fare:
Ecco un esempio di come fare in Go. Si supponga di voler rimuovere tutti i numeri da una stringa.

```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	test := "Ciao4 Mondo9! 7"

	fmt.Println(strings.Map(func(r rune) rune {
		if unicode.IsDigit(r) {
			return -1
		}
		return r
	}, test))
}
```

Questo codice restituirà: `Ciao Mondo! `.

## Approfondimento
Sebbene la funzione `strings.Map` sia una soluzione comunemente usata e pulita per eliminare i caratteri in base direttamente a un modello in Go, alcune altre lingue offrono funzioni integrate che possono realizzare la stessa cosa con meno codice. Go si impegna a mantenere un linguaggio pulito e minimale, evitando di gonfiare il core del linguaggio con funzioni specifiche che possono essere facilmente realizzate con le sue funzioni esistenti.

Alternativamente, è possibile utilizzare un'espressione regolare per raggiungere lo stesso obiettivo, sebbene questo possa essere più lento se la stringa è particolarmente lunga, poiché implica la compilazione dell'espressione regolare.

## Vedi Anche
- Documentazione Go [strings.Map](https://golang.org/pkg/strings/#Map)
- Go By Example: [String Functions](https://gobyexample.com/string-functions)
- Documentazione Go [unicode.IsDigit](https://golang.org/pkg/unicode/#IsDigit)