---
title:    "Go: Calcolare una data nel futuro o nel passato"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

Calcolare una data futura o passata può essere utile in diverse situazioni, ad esempio per la gestione di eventi o per valutare scadenze.

## Come fare

Per calcolare una data in Go, dobbiamo utilizzare la funzione `AddDate` che accetta tre parametri: l'anno, il mese e il giorno da aggiungere alla data attuale. Ad esempio, per ottenere la data di 5 giorni fa possiamo utilizzare il seguente codice:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	fiveDaysAgo := time.Now().AddDate(0, 0, -5)
	
	fmt.Println(fiveDaysAgo)
}
```

Eseguendo questo codice, otterremo l'output `2021-06-01 14:32:57.346952269 +0200 CEST`, se eseguito il 6 giugno 2021. Possiamo anche specificare valori positivi per ottenere una data futura.

## Approfondimento

La funzione `AddDate` utilizza il pacchetto `time` di Go per effettuare i calcoli sulla data. Questo pacchetto fornisce una vasta gamma di funzioni per manipolare le date e gli orari. Ad esempio, possiamo utilizzare la funzione `Parse` per convertire una stringa in un oggetto di tipo `time.Time`, oppure la funzione `Format` per formattare la data in base al nostro desiderio.

## Vedi anche

- Documentazione ufficiale di Go sulla gestione del tempo: https://golang.org/pkg/time/
- Articolo su come calcolare una data futura o passata in Python: https://dev.to/codebubb/calculating-a-date-in-the-past-or-future-in-python-3hc
- Altre funzioni utili per la manipolazione delle date in Go: https://flaviocopes.com/go-time/