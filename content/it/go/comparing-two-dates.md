---
title:                "Go: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è un'operazione comune nella programmazione e può avere molteplici scopi. Ad esempio, potresti voler confrontare la data attuale con una data di scadenza per determinare se un'applicazione ha superato il termine previsto o verificare se un evento è già avvenuto o deve ancora accadere.

## Come fare

Per confrontare due date in Go, è necessario prima convertirle in oggetti di tipo `time.Time`, che rappresentano una data e un'ora specifiche. Ci sono diverse opzioni per creare un oggetto `time.Time`, sia a partire da una stringa che rappresenta una data, sia utilizzando funzioni specifiche come `time.Now()` per ottenere la data attuale.

Una volta creati gli oggetti `time.Time`, è possibile utilizzare gli operatori di confronto come `<`, `<=`, `==`, `>=` e `>` per confrontare le due date. Ad esempio, supponiamo di avere due date chiamate `dataUno` e `dataDue` e vogliamo verificare se `dataUno` è successiva a `dataDue`. Possiamo fare ciò utilizzando l'operatore `>`:

```
if dataUno > dataDue {
    fmt.Println("La dataUno è successiva alla dataDue.")
}
```

Di seguito un esempio completo che mostra come confrontare due date utilizzando le funzioni disponibili nella libreria standard di Go:

```
package main

import (
    "fmt"
    "time"
)

func main() {
    // Creiamo due oggetti time.Time con date differenti
    primaData := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)
    secondaData := time.Date(2020, 1, 31, 0, 0, 0, 0, time.UTC)

    // Confrontiamo le due date
    if primaData > secondaData {
        fmt.Println("La primaData è successiva alla secondaData.")
    } else if primaData < secondaData {
        fmt.Println("La primaData è precedente alla secondaData.")
    } else {
        fmt.Println("Le due date sono uguali.")
    }
}
```

L'output di questo codice sarà:

```
La primaData è successiva alla secondaData.
```

## Approfondimenti

Go offre diverse funzioni per manipolare e comparare date con precisione. Ad esempio, è possibile utilizzare la funzione `time.Parse()` per convertire una stringa in un oggetto `time.Time` in base a un formato specifico, oppure la funzione `time.Add()` per aggiungere o sottrarre una quantità di tempo da una data. Per maggiori informazioni su queste funzioni e su altre possibilità per gestire le date in Go, consulta la documentazione ufficiale della libreria `time`

## Vedi anche

- [Documentazione ufficiale sulla libreria `time` di Go](https://golang.org/pkg/time/)
- [Tutorial su come manipolare le date in Go](https://tutorialedge.net/golang/go-dates-tutorial/)