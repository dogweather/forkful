---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?

Convertire una data in stringa significa trasformare una data (che è un tipo di dato strutturato) in una stringa di testo. Lo facciamo per semplificare la visualizzazione e il salvataggio dei dati. 

## Come Fare:

In Go, per convertire una data in stringa, usiamo il pacchetto `time` e il suo metodo `Format`.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date := time.Now()
	fmt.Println(date.Format("2006-01-02"))
}
```
Esempio di output:
```Go
2022-05-15
```

## Approfondimento:

Storicamente, Go introduce il metodo `Format` per consentire una conversione semplice e flessibile tra date e stringhe. Rispetto ad altre lingue, è un po' diverso perché Go usa un approccio basato su un layout di riferimento ('2006-01-02'). 

Un'alternativa sarebbe usare la funzione `Sprintf` del pacchetto `fmt`, ma `Format` è più specifico per le date e offre maggiore flessibilità. 

Nella realizzazione, `Format` lavora interpretando il layout fornito e sostituendo ciascuno dei segnaposto con il corrispondente valore della data.

## Vedi Anche:

1. Documentazione ufficiale su `time.Format`: https://pkg.go.dev/time#Time.Format
2. Articolo dettagliato sulla formattazione della data in golang: https://yourbasic.org/golang/format-parse-string-time-date-example/
3. Comparazione tra Go e altri linguaggi nella conversione delle date: https://programming.guide/go/format-parse-string-time-date-example.html