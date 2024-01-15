---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Go: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore, sicuramente avrai incontrato situazioni in cui hai bisogno di determinare la lunghezza di una stringa. Questa operazione è molto comune nella programmazione e può essere utile per svolgere una varietà di compiti, come la verifica della validità dei dati o la manipolazione delle stringhe.

## Come Fare

Per trovare la lunghezza di una stringa in Go, possiamo utilizzare la funzione `len()`. Questa funzione restituisce il numero di caratteri della stringa incluse anche le eventuali lettere accentate.

```Go
package main

import "fmt"

func main() {
  stringa := "Ciao mondo"
  fmt.Println(len(stringa))
}
```

Output: 10

Possiamo anche utilizzare questa funzione per ottenere la lunghezza di un array di caratteri.

```Go
package main

import "fmt"

func main() {
  array := [5]string{"a", "b", "c", "d", "e"}
  fmt.Println(len(array))
}
```

Output: 5

## Approfondimento

La funzione `len()` non è solo limitata alle stringhe e agli array di caratteri, può essere utilizzata anche su altri tipi di dati come mappe e slice. Inoltre, è interessante notare che in Go, una stringa è in realtà un'array di byte e quindi, la sua lunghezza dipende dalla codifica dei caratteri utilizzata.

## Vedi Anche

- [Tutorial di Go ufficiale](https://golang.org/doc/tutorial/)
- [Documentazione di Go](https://golang.org/doc/)