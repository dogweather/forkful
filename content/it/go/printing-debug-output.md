---
title:                "Stampa dell'output di debug"
date:                  2024-01-20T17:52:27.565838-07:00
model:                 gpt-4-1106-preview
simple_title:         "Stampa dell'output di debug"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
La stampa del debug è come spieghi al computer di raccontarti storie di cosa sta succedendo nel codice. I programmatori lo fanno per capire problemi, flussi di dati e comportamento del programma.

## How to:
Ecco come stampare semplici messaggi di debug in Go:

```Go
package main

import (
	"fmt"
	"log"
)

func main() {
	// Stampa basilare
	fmt.Println("Ciao, sto debuggando!")

	// Stampa con formattazione
	animal := "gatto"
	action := "salta"
	fmt.Printf("Il %s %s.\n", animal, action)

	// Log con ora e data
	log.Println("Messaggio di debug con log.")
}
```

Output:

```
Ciao, sto debuggando!
Il gatto salta.
2009/11/10 23:00:00 Messaggio di debug con log.
```

## Deep Dive:
Nella stampa di debug, Go segue la tradizione di linguaggi come C con `printf`. Ma con `log`, offre più context come timestamp e configurazioni per livelli di log e destinazioni diverse (file, stdout). Sono disponibili anche pacchetti di terze parti come `zap` e `logrus` per funzionalità avanzate. La semplicità e la composabilità in Go permettono di creare soluzioni su misura ma è importante non esagerare con i messaggi di debug per non affogare in un mare di log.

## See Also:
- Go standard log package: [https://pkg.go.dev/log](https://pkg.go.dev/log)
- Go fmt package: [https://pkg.go.dev/fmt](https://pkg.go.dev/fmt)
- Logrus, pacchetto di logging avanzato: [https://github.com/sirupsen/logrus](https://github.com/sirupsen/logrus)
- Zap, pacchetto di logging ad alte prestazioni: [https://github.com/uber-go/zap](https://github.com/uber-go/zap)