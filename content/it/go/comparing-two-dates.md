---
title:    "Go: Confronto tra due date"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui è necessario confrontare due date in un programma Go. Ad esempio, si potrebbe voler determinare se un appuntamento è stato già fissato, o se una determinata data è antecedente o successiva rispetto a un'altra. Imparare a confrontare correttamente due date è un'abilità utile per qualsiasi sviluppatore di Go.

## Come fare

Il modo più semplice per confrontare due date in Go è utilizzare il metodo `Before()` o `After()` dell'oggetto `time.Time`. In questo esempio, creeremo due date e le confrontaremo per vedere se la prima è precedente alla seconda:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // creiamo due date
    data1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
    data2 := time.Date(2021, time.February, 1, 0, 0, 0, 0, time.UTC)

    // confrontiamo le date utilizzando il metodo Before()
    if data1.Before(data2) {
        fmt.Println("La data1 è precedente alla data2")
    }
}
```

L'output di questo programma sarà: `La data1 è precedente alla data2`.

È importante notare che il confronto viene effettuato utilizzando l'orario UTC (Coordinated Universal Time). Se si vuole confrontare le date utilizzando un fuso orario diverso, è possibile utilizzare il metodo `Date()` dell'oggetto `time.Time` per ottenere l'orario locale della data.

```Go
loc, _ := time.LoadLocation("America/New_York")

// creiamo due date, una utilizzando il fuso orario UTC e una utilizzando l'orario locale di New York
data1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
data2 := time.Date(2021, time.January, 1, 0, 0, 0, 0, loc)

// confrontiamo le date utilizzando il metodo After()
if data1.After(data2) {
    fmt.Println("La data1 è successiva alla data2")
}
```

L'output di questo programma sarà: `La data1 è successiva alla data2`.

## Approfondimento

Se si vuole confrontare le date in modo più preciso, è possibile utilizzare il metodo `Equal()` dell'oggetto `time.Time`. Questo metodo restituire un valore booleano che indica se le due date sono uguali.

Inoltre, si può utilizzare il pacchetto `time` per eseguire operazioni più avanzate sulle date, come la conversione tra fusi orari o l'estrazione di informazioni specifiche (come il giorno della settimana) dalle date.

## Vedi anche

Per ulteriori informazioni sulle date in Go, si possono consultare questi link:

- https://golang.org/pkg/time/
- https://www.calhoun.io/how-to-work-with-dates-and-times-in-go/
- https://programming.guide/go/time-date-manipulation.html

Happy coding! 🚀