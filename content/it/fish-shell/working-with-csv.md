---
title:                "Lavorare con i file csv"
html_title:           "Fish Shell: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Cos'è e perché: 
Lavorare con i file CSV può sembrare noioso, ma in realtà è un'attività molto comune per i programmatori. Le CSV (Comma Separated Values) vengono utilizzate per archiviare e organizzare grandi quantità di dati in formato tabellare.

## Come fare: 
Per lavorare con i file CSV in Fish Shell, è possibile utilizzare il comando `fgetcsv`. Questo comando legge un file CSV e restituisce un array di righe, ogni riga è a sua volta un array di valori.

```
set rows (fgetcsv file.csv)
echo $rows[1][2]
```
Questo esempio mostra come accedere al valore della riga 1 nella colonna 2 del file CSV.

## Approfondimento: 
L'uso dei file CSV è diventato molto popolare negli ultimi anni perché sono facili da gestire e possono essere letti da molti tipi di software. In passato, i file CSV venivano utilizzati principalmente per il trasferimento dei dati tra diversi programmi o per l'importazione dei dati da tabelle Excel.

Esistono diversi tipi di file CSV, come ad esempio quelli delimitati da tabulazioni o da altri simboli anziché da virgole. Inoltre, ci sono molti altri formati di file che possono essere utilizzati per organizzare dati tabellari, come ad esempio XML o JSON.

L'implementazione di Fish Shell per lavorare con i file CSV si basa su una libreria esterna chiamata `libcsv`. Questa libreria fornisce funzioni di lettura e scrittura per i file CSV e viene utilizzata anche da altri programmi come Excel o LibreOffice.