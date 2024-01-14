---
title:                "Go: Ottenere la data attuale"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Esistono diverse motivazioni per cui un programmatore potrebbe aver bisogno di gestire la data attuale all'interno del proprio codice. Ad esempio, si potrebbe utilizzare la data per creare delle funzionalità dinamiche nei propri programmi, oppure per effettuare confronti e calcoli. Inoltre, la gestione accurata delle date è fondamentale per garantire che le proprie applicazioni funzionino correttamente.

## Come Fare

Per ottenere la data corrente all'interno di un programma Go, è possibile utilizzare il pacchetto `time`. Qui di seguito è riportato un esempio di codice che mostra come ottenere la data e l'ora attuali, e stamparle a schermo:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    currentTime := time.Now()
    fmt.Println("Data e ora attuali:", currentTime)
}
```

L'output di questo codice sarà qualcosa del genere:

`Data e ora attuali: 2021-10-18 14:30:00.000000 +0000 UTC m=+0.000000000`

Come puoi vedere, il pacchetto `time` ci fornisce una serie di informazioni sulla data attuale, tra cui l'anno, il mese, il giorno, l'ora e il fuso orario.

## Approfondimento

Per gestire al meglio le date, è importante comprendere anche il concetto di fuso orario. Nel codice precedente, abbiamo visto che la data attuale è stata stampata seguita dalla sigla "UTC". Questa indica il fuso orario coordinato, il quale è il punto di riferimento per tutti gli altri fusi orari.

Se si vuole ottenere la data attuale in un fuso orario specifico, è possibile utilizzare il metodo `Local()` del pacchetto `time`. Di seguito un esempio di codice che ci restituisce la data attuale nel fuso orario di Roma:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    location, err := time.LoadLocation("Europe/Rome")
    if err != nil {
        panic(err)
    }

    currentTime := time.Now().In(location)
    fmt.Println("Data e ora attuali a Roma:", currentTime)
}
```

L'output di questo codice sarà qualcosa del genere:

`Data e ora attuali a Roma: 2021-10-18 16:30:00.000000 +0200 CEST`

Come si può notare, la data attuale è stata correttamente convertita nel fuso orario di Roma.

## Vedi Anche

- [Documentazione ufficiale pacchetto `time` in Go](https://golang.org/pkg/time/)
- [Tutorial su come gestire le date in Go](https://www.geeksforgeeks.org/how-to-handle-different-date-formats-in-golang/)

Grazie per aver letto questo articolo! Speriamo ti sia stato utile per imparare a gestire la data attuale all'interno dei tuoi programmi in Go. Continua a seguire gli altri articoli sul nostro blog per scoprire altre utili informazioni sulla programmazione in Go!