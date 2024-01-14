---
title:    "Go: Estrazione di sottostringhe."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore Go, è probabile che tu abbia familiarità con il concetto di stringhe. Le stringhe sono una parte importante di molti programmi, e talvolta è necessario estrarre una sottostringa da una stringa più grande. In questo articolo parleremo di come e perché estrarre substrings in Go.

## Come fare
Per estrarre una sottostringa in Go, dobbiamo utilizzare la funzione "substring" della libreria "strings". Possiamo specificare l'indice di inizio e di fine della sottostringa che vogliamo estrarre, come mostrato nell'esempio qui sotto:
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Questo è un esempio di stringa."
	substr := strings.Substring(str, 7, 19)

	fmt.Println(substr)
}
```
L'output di questo codice sarà "è un esempio".

## Deep Dive
La funzione "substring" di Go utilizza l'indice di inizio e di fine per determinare quale parte della stringa originale verrà estratta. L'indice di inizio è l'indice del primo carattere della sottostringa che vogliamo, mentre l'indice di fine è l'indice del primo carattere dopo la fine della sottostringa. Inoltre, se l'indice di fine è superiore alla lunghezza della stringa, la sottostringa verrà estratta fino alla fine della stringa originale.

Un'altra opzione per estrarre substrings in Go è utilizzare la funzione "split" della libreria "strings". Questa funzione divide una stringa in base a un carattere o una sequenza di caratteri specificata e restituisce una slice contenente le sottostringhe ottenute. Ad esempio, se volessimo dividere una stringa per ogni spazio tra le parole, potremmo utilizzare il seguente codice:
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Questo è un esempio di stringa."
	substr := strings.Split(str, " ")

	fmt.Println(substr)
}
```
L'output di questo codice sarà "[Questo è un esempio di stringa.]".

## Vedi anche
- [La libreria strings di Go](https://golang.org/pkg/strings/)
- [Documentazione sulla funzione substring di Go](https://golang.org/pkg/strings/#Substring)
- [Documentazione sulla funzione split di Go](https://golang.org/pkg/strings/#Split)