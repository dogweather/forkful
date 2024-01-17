---
title:                "Ottenere la data corrente."
html_title:           "Go: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Ottenere la data corrente è un'operazione comune per i programmatori in Go. Ciò consente loro di ottenere informazioni sulla data attuale, che può essere utilizzata per varie attività, come ad esempio eseguire un programma in base alla data o registrare quando un'attività è stata eseguita.

## Come fare:
```Go
import "time"

func main() {
    // Ottenere la data corrente
    currentDate := time.Now()
    // Stampare la data corrente nel formato desiderato
    fmt.Println(currentDate.Format("02-01-2006"))
}
```

Output:
```
23-07-2021
```

## Approfondimento:
Ottenere la data corrente in Go è una funzionalità integrata nel pacchetto "time". Ci sono altre funzioni disponibili in questo pacchetto che consentono di manipolare e formattare la data in diversi modi. In alternativa, è anche possibile utilizzare pacchetti di terze parti per ottenere la data corrente in formati diversi o per gestire fusi orari diversi.

## Vedi anche:
- [Pacchetto "time" in Go](https://golang.org/pkg/time/)
- [Alternativa per la gestione della data in Go: "dateparse"](https://github.com/araddon/dateparse)
- [Un articolo su come ottenere la data corrente in Go](https://golangcode.com/get-todays-date/)