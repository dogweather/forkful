---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il parsing di una data da una stringa consiste nel leggere un testo (stringa) e nel trasformarlo in un oggetto data leggibile da un programma. I programmatori lo fanno per tradurre i dati in input in un formato utilizzabile dal proprio software.

## Come Fare:

Per convertire una stringa in una data in Go, è possibile utilizzare il pacchetto `time`. Ecco un esempio di base:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	const input = "2006-01-02"
	t, _ := time.Parse("2006-01-02", input)
	fmt.Println(t)
}
```

Quando si esegue questo codice, il risultato sarà:

```Go
2006-01-02 00:00:00 +0000 UTC
```

In pratica, si converte la stringa di input in un `time.Time`.

## Approfondimento:

Il pacchetto `time` di Go risale ai primi giorni del linguaggio. È parte integrante della standard library di Go e fornisce funzionalità per la misurazione e la visualizzazione del tempo.

Come alternativa al pacchetto `time`, esiste `jodaTime`, una libreria molto popolare in Java. Altre alternative potrebbero includere i pacchetti `dateparse` e `strtime`.

Un dettaglio importante da considerare quando si fa parsing di date in Go è che il layout della data di riferimento usato in `Time.Parse` e `Time.Format` è "2006-01-02 15:04:05". È il layout della data dell'epoca Unix zero (1 gennaio 1970 00:00:00 UTC), ma spostato indietro di 113 anni.

## Vedi Anche:

1. [Il pacchetto time della libreria standard Go](https://golang.org/pkg/time/).
2. [Pacchetto dateparse](https://github.com/araddon/dateparse).