---
title:                "Calcolo di una data futura o passata"
date:                  2024-01-20T17:31:19.511951-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcolo di una data futura o passata"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calcolare una data futura o passata significa semplicemente aggiungere o rimuovere giorni da una data di partenza. I programmatori lo fanno per gestire scadenze, archiviare eventi, o pianificare attività future.

## How to:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Data di partenza: Oggi
	oggi := time.Now()
	fmt.Println("Data di oggi:", oggi.Format("02-01-2006"))

	// Calcolo una data futura (10 giorni dopo)
	futuro := oggi.AddDate(0, 0, 10)
	fmt.Println("Data futura:", futuro.Format("02-01-2006"))

	// Calcolo una data passata (30 giorni prima)
	passato := oggi.AddDate(0, 0, -30)
	fmt.Println("Data passata:", passato.Format("02-01-2006"))
}
```

Sample output:
```
Data di oggi: 24-03-2023
Data futura: 03-04-2023
Data passata: 22-02-2023
```

## Deep Dive
Calcolare date future o passate è fondamentale per sistemi di pianificazione e cronologia. Prima, queste operazioni richiedevano complessi calcoli manuali, considerando anche anni bisestili, fusi orari, ecc. In Go, il package `time` rende tutto più semplice con funzioni come `AddDate` e `Sub`. Esistono altre alternative all'interno e all'esterno di Go, come i package `dateparse` per parsing flessibile e `now` per metodi più user-friendly.

Dettagli di implementazione: `AddDate` gestisce automaticamente mesi, anni, e anni bisestili. In Go, una dato è immutabile: ogni operazione che sembra "modificarla" in realtà ne crea una nuova.

## See Also
- Documentazione ufficiale di Go sul package `time`: [https://pkg.go.dev/time](https://pkg.go.dev/time)
- Articolo su Go date and time formatting: [https://yourbasic.org/golang/format-parse-string-time-date-example/](https://yourbasic.org/golang/format-parse-string-time-date-example/)
- GitHub repo per il package `now`: [https://github.com/jinzhu/now](https://github.com/jinzhu/now)
