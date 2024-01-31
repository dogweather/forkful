---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:36:31.338204-07:00
simple_title:         "Estrarre una data da una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Analizzare una data da una stringa significa trasformarla in un formato che il computer può comprendere e utilizzare. Programmare tale operazione è fondamentale per manipolare e confrontare date, programmare eventi o generare timeline.

## How to:
In Go, la libreria "time" è il tuo migliore alleato per queste operazioni:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Supponi di avere una stringa con una data
	dateStr := "02/03/2021"

	// Definisci il format della data atteso (gg/mm/aaaa in questo caso)
	layout := "02/01/2006" // Attenzione: usa sempre questa data come riferimento

	// Analizza la stringa per ottenere un valore di tipo Time
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Errore durante il parsing:", err)
		return
	}
	
	// Usa la data come preferisci
	fmt.Println("La data parsata è:", parsedDate)
}

```

Esempio di output:

```
La data parsata è: 2021-03-02 00:00:00 +0000 UTC
```

## Deep Dive
Parsing una data da una stringa è pratica comune fin dall'adozione dei primi sistemi informatici. Con l'evoluzione del web è diventato ancora più necessario vista l’ampia varietà di formati e fusi orari.

In Go, la data di riferimento per il formato di parsing è sempre il 2 gennaio del 2006 alle 15:04:05, rispettando l'ordine da anno a secondo (anno, mese, giorno, ora, minuto, secondo).

Alternativamente, si possono utilizzare altre librerie come "github.com/araddon/dateparse" per gestire automaticamente molti formati di data, ma la standard library "time" è più che sufficiente per la maggior parte dei casi.

L'implementazione richiede di specificare il layout giusto. Una discrepanza tra questo e la stringa di input genera un errore evidente nel parsing.

## See Also
- Documentazione Go per il package "time": https://pkg.go.dev/time
- Tutorial Go per il parsing di date e tempo: https://gobyexample.com/time-formatting-parsing
- Libreria "dateparse": https://github.com/araddon/dateparse
