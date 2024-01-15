---
title:                "Eliminazione di caratteri corrispondenti a un modello"
html_title:           "Go: Eliminazione di caratteri corrispondenti a un modello"
simple_title:         "Eliminazione di caratteri corrispondenti a un modello"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore alla ricerca di un linguaggio di programmazione semplice ma potente, allora Go potrebbe essere quello che fa per te. Inoltre, è un linguaggio in continua crescita e utilizzato da grandi aziende come Google, Dropbox e Uber.

## Come

Per eliminare caratteri che corrispondono a un determinato pattern, è possibile utilizzare la libreria "strings" di Go. Di seguito un esempio di codice che rimuove tutte le vocali da una stringa.

```Go
package main

import (
	"fmt"
	"strings"
)

func removeVowels(s string) string {
	return strings.NewReplacer("a", "", "e", "", "i", "", "o", "", "u", "").Replace(s)
}

func main() {
	text := "Ciao, come stai?"
	fmt.Println(removeVowels(text))
}

//Output: C, cm st?
```

## Deep Dive

La funzione "strings.NewReplacer" consente di sostituire più stringhe contemporaneamente all'interno di una stringa, utilizzando una sintassi chiave-valore. Questo rende il processo di eliminazione dei caratteri più semplice e chiaro. Inoltre, si possono aggiungere altre coppie di caratteri da eliminare alla funzione "NewReplacer" senza dover modificare il codice principale.

## Vedi anche

- Documentazione ufficiale di Go sulla libreria "strings": https://golang.org/pkg/strings/
- Un'ottima guida su come usare la libreria "strings" in Go: https://zetcode.com/golang/strings/
- Un tutorial su come utilizzare le funzioni di rimpiazzo in Go: https://gobyexample.com/string-functions