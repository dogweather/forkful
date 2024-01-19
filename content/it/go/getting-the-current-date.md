---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Ottenere la data corrente in Go

## Cos'è & Perché?
Ottenere la data corrente in programmazione significa recuperare la data e l'ora al momento dell'esecuzione del codice. Questo è utile per documentare quando si verificano determinati eventi, ad esempio timestamp di log, contrassegnare record nel database e per calcoli di tempo.

## Come fare:
In Go, possiamo ottenere la data corrente utilizzando il modulo `time`. Ecco come:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Ottenere la data corrente
	oggi := time.Now()
	fmt.Println(oggi)
}
```

Quando esegui questo codice, otterrai un output simile a: `2022-06-10 14:50:49.394985 +0200 CEST m=+0.000360924`

## Approfondimento
Go, originariamente sviluppato da Google, è noto per la sua semplicità e l'efficienza. Il modulo `time` fa parte della libreria standard di Go dal suo rilascio iniziale nel 2007.

Ci sono vari modi per ottenere la data corrente in altri linguaggi di programmazione. Ad esempio, in Python si usa `datetime.datetime.now()`, in Java `new Date()`, ecc. Ma in Go, la funzione `time.Now()` è quella tipicamente usata.

L'implementazione di `time.Now()` in Go utilizza i servizi forniti dal sistema operativo per ottenere l'attuale ora del sistema. I dettagli specifici possono variare a seconda del sistema operativo.

## Leggi anche
Per ulteriori dettagli, consultare questi link:

- Documentazione ufficiale del modulo `time` di Go: https://golang.org/pkg/time/
- Approfondimenti della Community di Go: https://blog.golang.org/
- Tutorial interattivo di Go: https://tour.golang.org/welcome/1