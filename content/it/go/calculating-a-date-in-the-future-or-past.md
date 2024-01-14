---
title:                "Go: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per una serie di ragioni, come pianificare eventi o rimanere organizzati.

## Come Fare

È possibile utilizzare la funzione ```time.Add()``` per aggiungere un numero specifico di secondi, minuti, ore, giorni, mesi o anni a una data esistente. Ad esempio:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Data attuale
    now := time.Now()

    // Aggiungi 1 anno alla data attuale
    futureDate := now.AddDate(1, 0, 0)

    // Output della data futura
    fmt.Println("La data futura è", futureDate)
}
```

**Output:** La data futura è 2021-05-12 16:26:48.77579627 +0100 BST

## Approfondimento

La libreria standard di Go offre diverse funzioni utili per il calcolo di date nel futuro o nel passato, come ad esempio ```time.Parse()``` per convertire una stringa in una data, ```time.Truncate()``` per ridurre la precisione di una data e ```time.Date()``` per creare una nuova data a partire da specifici valori di anno, mese, giorno, ora, minuto e secondo.

Per ulteriori informazioni sulla gestione delle date in Go, è possibile consultare la documentazione ufficiale della libreria time: [https://golang.org/pkg/time/](https://golang.org/pkg/time/)

## Vedi Anche

- [Ottenere una data e un orario in Go](https://golangbyexample.com/date-and-time-in-go/)
- [Calcolare il tempo trascorso in Go](https://www.calhoun.io/computing-days-elapsed-since-an-event-in-go/)
- [Utilizzare il pacchetto time in Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-time-package-in-go-ut)