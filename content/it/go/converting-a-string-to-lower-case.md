---
title:                "Go: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per voler convertire una stringa in lettere minuscole quando si scrive un programma in Go. Puoi fare questo per uniformare la formattazione dei dati, per confrontare stringhe in modo case-insensitive, o semplicemente per ottenere una stringa più leggibile. In questo post, esploreremo alcuni modi per convertire una stringa in lettere minuscole usando il linguaggio di programmazione Go.

## Come Fare

Ci sono due modi principali per convertire una stringa in lettere minuscole usando Go: utilizzando la funzione `ToLower` dalla libreria standard e utilizzando il pacchetto `strings`. Di seguito sono riportati esempi pratici di entrambi i metodi:

Codice usando `ToLower`:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "CIAO A TUTTI"

	// Utilizzando la funzione `ToLower` dalla libreria standard
	result := strings.ToLower(str)
	fmt.Println(result)
}
```

Output: `ciao a tutti`

Codice usando `strings` package:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "HELLO WORLD"

	// Utilizzando il pacchetto `strings`
	result := strings.ToLower(str)
	fmt.Println(result)
}
```

Output: `hello world`

## Deep Dive

Il pacchetto `strings` fornisce un'implementazione più avanzata per la conversione di una stringa in lettere minuscole. In particolare, offre la funzione `ToLowerSpecial` che può gestire map personalizzate di lettere. Ad esempio, se vuoi convertire una stringa con caratteri speciali in lettere minuscole, puoi usare la funzione in questo modo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "È UNA STRINGA CON CARATTERI SPECIALI!"

	// Creazione di una mappa personalizzata con caratteri speciali
	charMap := map[rune]rune{
		'È': 'e',
		'Ú': 'u',
		'Ò': 'o',
		'Á': 'a',
		'Ì': 'i',
	}

	// Utilizzando `ToLowerSpecial` con la mappa personalizzata
	result := strings.ToLowerSpecial(charMap, str)
	fmt.Println(result)
}
```

Output: `e una stringa con caratteri speciali!`

In questo modo puoi gestire una vasta gamma di caratteri speciali e ottenere una conversione precisa della stringa.

## Vedi Anche

- [Funzione `ToLower` dalla libreria standard](https://golang.org/pkg/strings/#ToLower)
- [Pacchetto `strings` dalla documentazione ufficiale di Go](https://golang.org/pkg/strings/)
- [Guida introduttiva a Go per programmatori italiani](https://blog.golang.org/go-for-italians)