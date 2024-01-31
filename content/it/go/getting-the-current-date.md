---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:14:49.705773-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente in programmazione significa semplicemente riuscire a conoscere il giorno presente. I programmatori lo fanno per timbrature, log, operazioni temporali, etc.

## How to:

Ottenere la data corrente in Go è un gioco da ragazzi. Ecco come:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	oggi := time.Now()
	fmt.Println("Data e ora correnti:", oggi)
}
```

Output di esempio:

```
Data e ora correnti: 2022-12-31 15:04:05.123456 +0200 EET
```

È possibile formattare la data come si preferisce:

```Go
fmt.Println("Solo la data:", oggi.Format("2006-01-02"))
```

Output:

```
Solo la data: 2022-12-31
```

## Deep Dive

All'inizio, ottenere la data e l'ora era legato alla necessità di sistemi operativi di mantener traccia di eventi e task. In Go, la gestione della data e dell'ora si appoggia al pacchetto `time`, che fornisce funzionalità per lavorare con date, ore e loro differenze.

Alternativamente, se non ti serve l'ora precisa, potresti usare `time.Date` per costruire una data manualmente.

Implementare la gestione delle date tiene conto dei fusi orari e della localizzazione. `time.Now()` restituisce la data e l'ora correnti nel fuso orario locale. È possibile specificare un fuso orario differente creando una `location` e passandola come parametro alla funzione `In`.

## See Also

- Pacchetto `time` in Go: [Pacchetto Time](https://pkg.go.dev/time)
- Formattazione e parsing delle date in Go: [Formattare date in Go](https://yourbasic.org/golang/format-parse-string-time-date-example/)
