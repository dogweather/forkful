---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Go: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Calcolare una data nel futuro o nel passato significa semplicemente spostarsi avanti o all'indietro nel tempo a partire da una data specifica. I programmatori lo fanno per gestire situazioni come pianificare eventi, impostare promemoria o calcolare la differenza tra date.

## Come fare:
In Go usiamo il pacchetto "time" incorporato per calcolare una data futura o passata. Ecco un esempio che aggiunge 3 giorni alla data corrente.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    oggi := time.Now()
    futuro := oggi.AddDate(0, 0, 3)
    fmt.Println("Oggi è:", oggi)
    fmt.Println("Fra tre giorni sarà:", futuro)
}
```

Quando lo esegui, vedrai output simile a:

```Go
Oggi è: 2022-01-12 14:52:35.8859813 +0100 CET m=+0.001999001
Fra tre giorni sarà: 2022-01-15 14:52:35.8859813 +0100 CET m=+0.001999001
```
## Approfondimento:

La manipolazione delle date è una componente chiave della programmazione sin dai primi giorni dell'informatica. In Go, il pacchetto "time" ci offre strumenti potenti per farlo. Ci sono alternativa come il pacchetto "date" dalle librerie esterne, ma "time" è la scelta predefinita per la sua semplicità e l'efficienza.

Dato il design del tipo "time.Time" in Go, una data futura o passata viene calcolata aggiungendo o sottraendo una durata a un momento specifico. Si noti che ciò non tiene conto dei cambiamenti di orario legati all'ora legale. Se hai bisogno di una logica più complessa, dovrai implementarla separatamente.

## Vedi anche:

Qui ci sono alcuni collegamenti a risorse correlate che potrebbero essere utili:

- [Pacchetto time di Go](https://golang.org/pkg/time/)
- [Github di date package](https://github.com/jinzhu/now)