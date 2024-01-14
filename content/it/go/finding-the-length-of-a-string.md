---
title:                "Go: Trova la lunghezza di una stringa"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione molto utile nel linguaggio di programmazione Go. Conoscere la lunghezza di una stringa è fondamentale per poter gestire e manipolare i dati in modo efficace. In questo post, esploreremo come trovare la lunghezza di una stringa nel linguaggio Go e come può essere utile nella nostra codifica.

## Come fare

Per trovare la lunghezza di una stringa, utilizzando il linguaggio Go, possiamo utilizzare la funzione `len()`. Questa funzione prende come parametro una stringa e restituisce il numero di caratteri presenti all'interno della stringa. Ecco un esempio di codice in Go che mostra come utilizzare la funzione `len()` per trovare la lunghezza di diverse stringhe:

```
package main

import "fmt"

func main() {
	stringa1 := "Ciao"
	stringa2 := "Questo è un esempio di stringa"
	stringa3 := "Ho una lunghezza di 30 caratteri"
	
	fmt.Println(len(stringa1)) // Restituisce 4
	fmt.Println(len(stringa2)) // Restituisce 27
	fmt.Println(len(stringa3)) // Restituisce 30
}
```

## Approfondimento

Oltre alla funzione `len()`, Go ha anche un package chiamato `unicode/utf8` che ci permette di trovare la lunghezza di una stringa in un modo più avanzato. Questo package ci consente di gestire anche i caratteri unicode. Possiamo utilizzare la funzione `utf8.RuneCountInString()` per trovare la lunghezza della stringa contando i punti di codice Unicode e i surrogati UTF-16. Ecco un esempio di codice in Go che utilizza il package `unicode/utf8`:

```
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	stringa := "Ciao 😊"
	fmt.Println(utf8.RuneCountInString(stringa)) // Restituisce 6
}
```

## Vedi anche

- [Documentazione ufficiale di Go sulle funzioni di stringhe](https://golang.org/pkg/strings/)
- [Funzione `len()` su Golang Docs](https://golang.org/pkg/builtin/#len)
- [Package `unicode/utf8` su Golang Docs](https://golang.org/pkg/unicode/utf8/)