---
title:                "Confrontare due date"
html_title:           "Go: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Hai mai avuto la necessità di confrontare due date in un programma Go? Puoi trovarlo utile per evidenziare le differenze tra due date, ad esempio nel caso di aggiornamenti di un database o di generazione di report.

## Come fare

```Go
import (
    "fmt"
    "time"
)

func main() {
	today := time.Now()
	// Creazione di una data arbitraria
	arbitraryDate := time.Date(2020, time.Month(10), 27, 0, 0, 0, 0, time.UTC)
	// Confronto tra la data attuale e quella arbitraria
	if today.After(arbitraryDate) {
		fmt.Println("La data attuale è successiva alla data arbitraria.")
	} else if today.Before(arbitraryDate) {
		fmt.Println("La data attuale è precedente alla data arbitraria.")
	} else {
		fmt.Println("Le due date sono uguali.")
	}
}
```

L'output del codice sarà: "La data attuale è successiva alla data arbitraria." Nel codice precedente, utilizziamo il pacchetto "time" di Go per creare una data arbitraria e confrontarla con la data attuale. In questo esempio, viene utilizzata la funzione "After" per verificare se la data attuale è successiva a quella arbitraria. Tuttavia, puoi utilizzare anche la funzione "Before" per verificare se una data è precedente a un'altra.

## Approfondimento

Il pacchetto "time" di Go ci fornisce una serie di funzioni utili per confrontare due date. Oltre a "After" e "Before", è possibile utilizzare anche la funzione "Equal" per verificare se due date sono uguali. Inoltre, è possibile eseguire operazioni matematiche sulle date, come sommarle o sottrarle tra loro, utilizzando le funzioni "Add" e "Sub". È importante notare che il pacchetto "time" utilizza il fuso orario UTC per default, quindi potrebbe essere necessario effettuare delle conversioni prima di confrontare le date.

## Vedi anche

- [Documentazione del pacchetto time di Go](https://pkg.go.dev/time)
- [Tutorial su come lavorare con le date in Go](https://tutorialedge.net/golang/working-with-dates-golang/)