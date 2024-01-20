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

## Cosa & Perché?

Lavorare con i file CSV è una pratica comune tra i programmatori. In sostanza, un file CSV (Comma-Separated Values) è un documento di testo che contiene dati strutturati in colonne e righe, dove ogni campo è separato da una virgola. I programmatori usano i file CSV per memorizzare e manipolare grandi quantità di dati in modo efficiente.

## Come fare:

Ecco un esempio di codice in ```Gleam``` per leggere un file CSV e stampare il suo contenuto in console:

```Gleam
let file = import("file.csv") // Importa il file CSV nel programma
let data = csv.parse(file) // Utilizza il modulo csv per analizzare il file
for row in data do // Utilizza un ciclo for per accedere alle righe del file
  Console.log(row) // Stampa ogni riga sul terminale
```

Ecco un esempio di output:

```Gleam
["Nome", "Cognome", "Età"]
["Alice", "Rossi", 25]
["Luca", "Verdi", 30]
["Giulia", "Bianchi", 22]
```

## Approfondimento:

La diffusa adozione dei file CSV risale agli anni '70, quando gli sviluppatori dei primi software di database introdussero il formato come standard per l'importazione ed esportazione di dati. Oggi, anche se ci sono alternative più avanzate come i database relazionali, i file CSV rimangono uno strumento importante per la gestione dei dati.

Per manipolare i file CSV, esistono diversi moduli e librerie disponibili per vari linguaggi di programmazione, tra cui anche in ```Gleam```. Con la sua sintassi semplice ed intuitiva, ```Gleam``` è una scelta eccellente per lavorare con i file CSV.

## Vedi anche:

- [Documentazione ufficiale di Gleam](https://gleam.run/documentation/)
- [Il formato CSV su Wikipedia](https://it.wikipedia.org/wiki/Comma-separated_values)