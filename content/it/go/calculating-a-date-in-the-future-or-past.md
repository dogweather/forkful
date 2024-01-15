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

## Perché

La programmazione comporta spesso il calcolo di date sia nel futuro che nel passato. Utilizzando Go, questo processo può essere semplificato utilizzando le opzioni integrate per il calcolo delle date.

## Come fare

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Calcola una data nel futuro
	futuro := time.Now().AddDate(0, 0, 7)

	// Calcola una data nel passato
	passato := time.Now().AddDate(0, 0, -7)

	// Stampa le date calcolate
	fmt.Println("Data nel futuro:", futuro.Format("02/01/2006"))
	fmt.Println("Data nel passato:", passato.Format("02/01/2006"))
}

```

L'output di questo codice sarà il seguente:

```
Data nel futuro: 29/04/2021
Data nel passato: 15/04/2021
```

È possibile utilizzare le funzioni `AddDate()` e `Sub()` per aggiungere o sottrarre anni, mesi e giorni dalle date. Inoltre, è possibile utilizzare il metodo `Format()` per impostare il formato di visualizzazione desiderato.

## Approfondimento

In Go, le date sono rappresentate come oggetti di tipo `time.Time`. Quando si utilizzano le funzioni per il calcolo delle date, si deve sempre fare riferimento alla data corrente utilizzando la funzione `Now()`.

Inoltre, Go offre anche la possibilità di eseguire operazioni più complesse sulle date, come la comparazione tra date o il calcolo del numero di giorni tra due date.

## Vedi anche

- Documentazione ufficiale sul pacchetto` time`: https://golang.org/pkg/time/
- Tutorial su Go per principianti: https://talkgo.org/t/topic/29
- Risorse e community di Go: https://golang.org/help/