---
title:                "Estrarre una data da una stringa"
html_title:           "Go: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Analizzare una data da una stringa è il processo di estrarre le informazioni di una data (giorno, mese, anno) da una stringa di testo. I programmatori lo fanno per lavorare con le date in modo più preciso e per automatizzare alcune operazioni che coinvolgono le date.

## Come fare:

L'utilizzo di Go è molto semplice per analizzare una data da una stringa. In questo esempio, partiamo da una stringa contenente la data "20/01/2021" e la trasformiamo in un tipo time.Time utilizzando il metodo Parse del pacchetto time. Inoltre, possiamo specificare il formato della data, in questo caso "02/01/2006".

```Go
import "time"

dataString := "20/01/2021"
data, _ := time.Parse("02/01/2006", dataString)
data.Format("2 January 2006") // output: "20 January 2021"
```

## Approfondimento:

Parsing delle date è stato a lungo un problema per i programmatori, soprattutto quando ci si trova di fronte a diversi formati di data provenienti da diverse fonti. Fortunatamente, Go fornisce un potente pacchetto time che semplifica notevolmente questo processo. Inoltre, ci sono anche altre librerie di terze parti che possono essere utili, come ad esempio dateparse.

## Vedi anche:

Per ulteriori approfondimenti su come lavorare con le date in Go, puoi consultare la documentazione ufficiale del pacchetto time: https://golang.org/pkg/time/

Oppure puoi esplorare alcune delle librerie di terze parti per il parsing delle date, come dateparse: https://github.com/araddon/dateparse