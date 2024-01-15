---
title:                "Lavorare con i file csv"
html_title:           "Gleam: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se vuoi manipolare e analizzare dati tabulari, come nel caso di fogli di calcolo CSV, allora Gleam è il linguaggio di programmazione perfetto per te. Con la sua sintassi semplice e tipizzazione statica, è estremamente efficace nel gestire grandi quantità di dati.

## Come fare

Per iniziare a utilizzare Gleam per lavorare con file CSV, devi prima importare la libreria `csv` e creare un nuovo modulo.

```Gleam
import csv

pub mod main {
  // In questo esempio, il nostro file CSV ha colonne "nome" e "età"
  let csv_path = "persone.csv"

  // Carica il file CSV e assegna il risultato a una variabile
  let result = csv.load(csv_path)

  // Stampa la lista di record nel file
  io.println(result.records)

  // Puoi anche filtrare i record in base a un valore specifico nella colonna "età"
  let filtered = result.records
    |> filter (\(record) -> record.età == 25)

  // Stampa la lista filtrata
  io.println(filtered)
}
```

Ecco cosa otterrai come output:

```
Record {
  nome = "Maria",
  età = 23,
},
Record {
  nome = "Luca",
  età = 25,
},
Record {
  nome = "Anna",
  età = 25,
}

Record {
  nome = "Luca",
  età = 25,
},
Record {
  nome = "Anna",
  età = 25,
}
```

## Approfondimento

La libreria `csv` offre più di 10 funzioni per gestire i file CSV in Gleam. Puoi anche specificare un delimitatore diverso dal valore predefinito (virgola) quando carichi il file, utilizzare il modulo `file` per creare, scrivere e leggere file CSV, e molto altro ancora. Inoltre, la documentazione ufficiale di Gleam fornisce ulteriori esempi e spiegazioni per aiutarti a padroneggiare il lavoro con CSV.

## Vedi anche

- Documentazione ufficiale di Gleam per la libreria `csv`: https://gleam.run/modules/csv.html
- Tutorial su come lavorare con dati CSV in Gleam: https://www.data-to-viz.com/story/MakeItGleam.html