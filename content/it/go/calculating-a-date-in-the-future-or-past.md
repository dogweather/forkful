---
title:                "Go: Calcolare una data in futuro o passato"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Go, sicuramente avrai bisogno di calcolare date nel tuo codice. Ci sono molte ragioni per cui potresti aver bisogno di farlo, come ad esempio programmi di prenotazione, sistemi di scadenza di abbonamenti o semplicemente per visualizzare la data corrente su un'applicazione. In questo articolo, ti mostreremo come calcolare una data nel futuro o nel passato utilizzando il linguaggio di programmazione Go.

## Come fare

Per calcolare una data nel futuro o nel passato in Go, è necessario utilizzare il pacchetto "time". Iniziamo con un semplice esempio di come ottenere la data e l'ora corrente:

```
package main

import (
    "fmt"
    "time"
)

func main() {

    now := time.Now()
    fmt.Println("La data e ora correnti sono:", now)
}
```

Ora, per calcolare una data nel futuro o nel passato, possiamo utilizzare il metodo "Add" del pacchetto "time". Vediamo un esempio di come aggiungere 1 giorno alla data corrente:

```
package main

import (
    "fmt"
    "time"
)

func main() {

    now := time.Now()
    future := now.AddDate(0, 0, 1) //aggiunge 1 giorno
    fmt.Println("La data nel futuro è:", future)
}
```

Noterai che abbiamo utilizzato il metodo "AddDate" e abbiamo specificato il numero di anni, mesi e giorni da aggiungere alla data corrente. Puoi anche sottrarre una data utilizzando lo stesso metodo, basta utilizzare numeri negativi per gli argomenti.

Ora, se vuoi calcolare una data basandoti su una data specifica, puoi utilizzare il metodo "Date" del pacchetto "time". Vediamo un esempio di come calcolare la data dell'ultimo anno nuovo usando una data specifica:

```
package main

import (
    "fmt"
    "time"
)

func main() {

    newYear := time.Date(2022, time.January, 1, 0, 0, 0, 0, time.UTC) //data del prossimo anno nuovo
    lastYear := newYear.AddDate(-1, 0, 0) //sottrae 1 anno alla data
    fmt.Println("La data dell'ultimo anno nuovo è:", lastYear)
}
```

In questo esempio, abbiamo utilizzato il metodo "Date" per creare una data specifica nel futuro e poi abbiamo utilizzato il metodo "AddDate" per sottrarre 1 anno, ottenendo così la data dell'ultimo anno nuovo.

## Approfondimento

Oltre ai metodi "Add" e "Date", il pacchetto "time" ha altri metodi utili per gestire le date e gli orari. Alcuni di questi includono "Sub", per sottrarre una data da un'altra, "Round", per arrotondare una data all'unità di tempo specificata e "Truncate", per troncare una data all'unità di tempo specificata.

È importante notare che il pacchetto "time" utilizza il fuso orario UTC (Universal Time Coordinated), quindi potrebbe essere necessario fare alcune conversioni se desideri utilizzare una data specifica in un fuso orario diverso.

## Vedi anche

- Documentazione ufficiale del pacchetto "time" in Go: https://golang.org/pkg/time/
- Un articolo su come gestire le date e gli orari in Go: https://www.callicoder.com/golang-datetime-tutorial/
- Una guida dettagliata sull'utilizzo del pacchetto "time" in Go: https://zetcode.com/golang/time/

Grazie per aver letto questo articolo sul calcolo delle date nel futuro o nel passato in Go. Speriamo che ti sia stato utile e ti aiuti a gestire meglio le date nel tuo codice. Buona programmazione!