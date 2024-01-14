---
title:    "Go: Convertire una stringa in minuscolo"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

Ci sono diverse ragioni per cui potresti voler convertire una stringa in lettere minuscole (lower case) in Go. Ad esempio, potrebbe essere necessario per confrontare due stringhe in modo case-insensitive o per garantire l'uniformità dei dati che stai manipolando.

## Come Fare

In Go, puoi facilmente convertire una stringa in minuscolo utilizzando la funzione `ToLower` del pacchetto di librerie `strings.` Ad esempio:

```Go
package main

import (
   "fmt"
   "strings"
)

func main() {
   str := "CIAO AMICI!"

   result := strings.ToLower(str)
   fmt.Println(result)

  // output: ciao amici!
}
```

## Deep Dive

La funzione `ToLower` utilizza le regole della tabella Unicode per convertire le lettere maiuscole in minuscole. Ciò significa che funziona con qualsiasi lingua o alfabeto supportato da Unicode.

Inoltre, è importante notare che la funzione non modifica la stringa originale, ma restituisce una nuova stringa convertita. Quindi, se desideri modificare la stringa originale, dovrai assegnarla nuovamente alla variabile.

## Vedi Anche

- [Documentazione ufficiale sulla funzione ToLower](https://golang.org/pkg/strings/#ToLower)
- [Corso introduttivo su Go in italiano](https://github.com/biancarosa/GoCourse-Italian)