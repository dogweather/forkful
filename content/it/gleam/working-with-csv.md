---
title:                "Gleam: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore o uno sviluppatore di software, lavorare con file CSV può sembrare una noiosa attività. Tuttavia, ci sono molte ragioni per cui dovresti considerare di utilizzare il linguaggio di programmazione Gleam per lavorare con i file CSV. In primo luogo, Gleam è un linguaggio di programmazione funzionale che rende facile e intuitivo lavorare con i dati. Inoltre, ha una sintassi pulita e concisa che lo rende perfetto per la manipolazione dei file CSV.

## Come fare

Per lavorare con i file CSV in Gleam, tutto ciò che devi fare è importare il modulo CSV tramite la dichiarazione "import csv" all'inizio del tuo codice. Una volta fatto ciò, puoi utilizzare le funzioni fornite dal modulo per leggere, scrivere e manipolare i dati CSV. Di seguito è riportato un esempio di codice per leggere un file CSV e stampare le righe su console:

```Gleam
import csv

file := csv.open("dati.csv")
rows := csv.get_rows(file)

for row in rows {
  csv.print(row)
}
```

L'output di questo codice sarà l'elenco delle righe del file CSV sulla console.

## Approfondimento

Se vuoi saperne di più su come lavorare con i file CSV in Gleam, puoi consultare la documentazione ufficiale sul modulo CSV o leggere gli esempi di codice presenti nella libreria standard di Gleam. Inoltre, puoi esplorare le numerose funzioni fornite dal modulo CSV, come "filter", "map" e "reduce", per manipolare i dati in modo più avanzato.

## Vedi anche
- Documentazione ufficiale del modulo CSV di Gleam: https://gleam.run/modules/csv/
- Esempi di codice nella libreria standard di Gleam: https://github.com/gleam-lang/gleam_stdlib/blob/master/csv/csv.gleam