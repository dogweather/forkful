---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Calcolare la lunghezza di una stringa significa determinare il numero di caratteri contenuti in essa. Programmare richiede spesso questo tipo di operazione per gestire testi, condizioni logiche o problemi legati all'allocazione della memoria.

## Come Fare:

Per calcolare la lunghezza di una stringa in Go, useremo la funzione built-in `len()`. 

```Go
package main

import "fmt"

func main() {
    str := "Programmazione in Go"
    fmt.Println(len(str))
}
```

A questo punto a console apparirà il valore `21`, che è la lunghezza della stringa.

## Approfondimenti:

In Go, le stringhe sono costituite da una sequenza immutabile di bytes. La funzione `len()` restituisce il numero di byte, non il numero di caratteri, che potrebbe essere diverso se la stringa contiene rune (caratteri unicode) di dimensione superiore a un byte.

Una soluzione alternativa sarebbe l'uso della libreria standard `unicode/utf8` per calcolare il numero di rune invece che il numero di byte.

```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "Programmazione con č, ć, đ, š e ž"
	fmt.Println(utf8.RuneCountInString(str))
}
```

## Vedi Anche:

Controlla i link sottostanti per ulteriori informazioni ed esempi:

- Documentazione ufficiale: [Pacchetto stringhe](https://golang.org/pkg/strings/)
- Go by Example: [Stringhe](https://gobyexample.com/strings)
- Una guida completa sulle stringhe in Go [link](https://yourbasic.org/golang/strings-explained/)
- Post del blog di Go: [Stringhe, byte, rune e caratteri in Go](https://blog.golang.org/strings)