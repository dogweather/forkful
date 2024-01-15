---
title:                "Convertire una stringa in minuscolo."
html_title:           "Go: Convertire una stringa in minuscolo."
simple_title:         "Convertire una stringa in minuscolo."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Spesso, durante la scrittura di un programma, abbiamo bisogno di manipolare le stringhe per ottenere i risultati desiderati. Una delle operazioni più comuni è la conversione di una stringa in minuscolo. Questo può essere utile per confrontare stringhe senza considerare le maiuscole o per formattare l'output in un modo specifico.

## Come fare

Per convertire una stringa in minuscolo in Go, possiamo utilizzare la funzione `ToLower` del pacchetto `strings` di Go. Ad esempio:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // Definiamo una stringa di esempio
    str := "Go è un linguaggio di programmazione moderno"

    // Convertiamo la stringa in minuscolo
    result := strings.ToLower(str)

    fmt.Println(result) // Output: go è un linguaggio di programmazione moderno
}
```

## Approfondimento

La conversione in minuscolo di una stringa può sembrare una semplice operazione, ma è importante capire come funziona esattamente. In Go, le stringhe sono immutabili, il che significa che ogni volta che vengono modificate, viene create una nuova stringa. La funzione `ToLower` sfrutta questo concetto per restituire una nuova stringa in minuscolo, lasciando la stringa originale immutata.

Un altro aspetto importante da considerare è il set di caratteri utilizzato per la conversione. In Go, le stringhe sono codificate utilizzando UTF-8, il che significa che anche i caratteri non-ASCII verranno convertiti in minuscolo correttamente.

## Vedi anche

- La documentazione ufficiale di Go sul pacchetto `strings`: https://golang.org/pkg/strings/
- Un esempio su come utilizzare la funzione `ToLower`: https://play.golang.org/p/KQpT2WSCqbd