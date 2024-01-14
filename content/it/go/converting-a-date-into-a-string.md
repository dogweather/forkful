---
title:                "Go: Trasformare una data in una stringa"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa può essere una necessità comune nei progetti di sviluppo di software. Puoi voler visualizzare la data in un formato specifico o passarla come argomento a una funzione che richiede una stringa.

## Come Fare

Per convertire una data in una stringa in Go, puoi utilizzare il metodo `Format()` della struttura `time.Time`. Questo metodo accetta un formato di stringa che specifica come vuoi visualizzare la data. Ad esempio:

```Go
data := time.Date(2021, time.August, 23, 12, 0, 0, 0, time.UTC)
stringa := data.Format("02/01/2006") // output: 23/08/2021
```

Puoi anche utilizzare la funzione `Format()` del pacchetto `fmt` per stampare la stringa direttamente sulla console:

```Go
fmt.Printf("Oggi è il %s\n", data.Format("02/01/2006")) //output: Oggi è il 23/08/2021
```

## Approfondimento

La conversione di una data in una stringa coinvolge anche la gestione dei fusi orari. Nel codice precedente, abbiamo utilizzato il fuso orario UTC per impostare la data. Questo garantisce che la data sia coerente indipendentemente dal fuso orario del computer in cui viene eseguito il programma. È importante prestare attenzione ai fusi orari quando si manipolano le date, in modo da evitare equivoci e errori.

Inoltre, è possibile specificare formati di stringa personalizzati utilizzando gli stessi placeholder usati dalla funzione `Format()` della struttura `time.Time`. Ad esempio, se vuoi visualizzare anche l'ora e il fuso orario, puoi utilizzare il formato "02/01/2006 15:04 -0700", dove "15" rappresenta l'ora e "04" i minuti. Puoi trovare l'elenco completo dei placeholder nella documentazione ufficiale di Go.

## Vedi Anche
- Documentazione ufficiale di Go sulla package `time`: https://golang.org/pkg/time/
- Un articolo che approfondisce la conversione di date in stringhe in Go (in inglese): https://yourbasic.org/golang/format-parse-string-time-date-example/