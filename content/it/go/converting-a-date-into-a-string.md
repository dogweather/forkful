---
title:                "Go: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con dati in un programma Go, è necessario convertire una data in una stringa. Ciò può essere utile per visualizzare la data in un formato specifico o per eseguire operazioni su di essa. In questo post, esploreremo come convertire una data in una stringa utilizzando il linguaggio di programmazione Go.

## Come fare

Per convertire una data in una stringa in Go, è necessario utilizzare la funzione `Format` del pacchetto `time`. Questa funzione accetta due argomenti: il formato desiderato per la stringa e la data da convertire. Vediamo un esempio pratico utilizzando una data di oggi:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Otteniamo la data di oggi
	data := time.Now()

	// Convertiamo la data in una stringa
	stringaData := data.Format("02/01/2006")

	// Stampiamo la stringa
	fmt.Println(stringaData)
}
```

Output:
```
02/06/2021
```

Nell'esempio sopra, abbiamo utilizzato il formato `02/01/2006` che produce una stringa nel formato `giorno/mese/anno`. È possibile utilizzare diversi formati a seconda delle proprie esigenze. Ad esempio, se vogliamo includere anche l'ora nella stringa, possiamo utilizzare il formato `02/01/2006 15:04`.

## Approfondimento

Oltre ai formati standard, è possibile anche creare formati personalizzati per le date in Go utilizzando il metodo `Format` della struttura `Time`. Questo metodo accetta una stringa contenente i caratteri specifici del formato desiderato. Ad esempio, il carattere `M` rappresenta il mese senza zero iniziale, mentre il carattere `MM` rappresenta il mese con zero iniziale. 

Per un elenco completo dei caratteri di formattazione disponibili, si può consultare la [documentazione ufficiale di Go](https://golang.org/pkg/time/#Time.Format).

## Vedi anche

- [Documentazione ufficiale di Go sul pacchetto `time`](https://golang.org/pkg/time/)
- [Tutorial su come utilizzare la funzione `Format` per convertire una data in una stringa in Go](https://gobyexample.com/time-formatting-parsing)