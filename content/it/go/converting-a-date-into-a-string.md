---
title:                "Conversione di una data in una stringa"
html_title:           "Go: Conversione di una data in una stringa"
simple_title:         "Conversione di una data in una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Convertire una data in una stringa è il processo di trasformare una data, rappresentata da un valore numerico o oggetto, in una rappresentazione testuale leggibile per gli utenti. I programmatori spesso fanno questo per visualizzare le date in un formato più comprensibile per gli utenti finali.

## Come fare:
Ecco come è possibile convertire una data in una stringa in Go utilizzando la funzione `Format()`:

```Go
package main
import (
    "fmt"
    "time"
)

func main() {
    // Ora corrente
    now := time.Now()

    // Formato standard per data e ora
    fmt.Println(now.Format("2006-01-02 15:04:05 PM"))

    // Formato personalizzato
	fmt.Println(now.Format("02/01/2006"))
}
```

**Output**:
```
2019-12-22 09:00:00 AM
22/12/2019
```

## Approfondimento:
Questa pratica di convertire date in stringhe risale ai primi giorni della programmazione, quando i computer erano ancora limitati da memoria e risorse. Nelle versioni più vecchie di Go, questa operazione era più laboriosa e richiedeva l'uso di funzioni come `datetime` e `strftime()`. Tuttavia, grazie ai miglioramenti nella gestione delle date in Go, ora è possibile utilizzare semplicemente la funzione `Format()` per convertire una data in una stringa.

## Vedi anche:
- Documentazione ufficiale di Go sulla funzione `Format()`: https://golang.org/pkg/time/#Time.Format
- Tutorial su come gestire le date in Go: https://golangbot.com/strftime/