---
title:                "Lavorare con csv"
html_title:           "Bash: Lavorare con csv"
simple_title:         "Lavorare con csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con una grande quantità di dati, è probabile che incontrerai il formato CSV. Questo è uno dei formati di file più comuni per i dati tabellari e imparare a lavorare con esso può semplificare notevolmente il tuo lavoro.

## Come Fare

Per iniziare a lavorare con CSV in Bash, è necessario utilizzare il comando `csvtool` che fa parte del pacchetto `csvkit` disponibile su Linux e macOS. Ad esempio, se vogliamo leggere un file CSV, possiamo usare il comando seguente:

```
csvtool -t ',' col 1,2,3 input.csv
```

Questo comando leggerà il file `input.csv`, separando i campi utilizzando la virgola (`,`) e stampando solo la prima, la seconda e la terza colonna del file. Ad esempio, se il file contiene:

```
nome,cognome,età
Mario,Rossi,35
Giovanna,Bianchi,28
```

L'output sarà:

```
nome,cognome,età
Mario,Rossi,35
Giovanna,Bianchi,28
```

Inoltre, è possibile utilizzare `csvtool` per ordinare e filtrare i dati, creare nuove colonne e convertire il formato del file. Per ulteriori informazioni sui comandi disponibili, consulta la documentazione `csvtool`.

## Approfondimento

Per lavorare con CSV in modo più approfondito, è importante comprendere il suo formato. CSV sta per "Comma-Separated Values" e i dati sono organizzati in righe e colonne utilizzando una determinata delimitazione dei campi, come la virgola o il punto e virgola.

Puoi anche utilizzare altri strumenti, come `awk` e `sed`, per manipolare i dati CSV in modalità più avanzate. Inoltre, puoi anche creare script Bash personalizzati per automatizzare il processo di lavorazione dei dati CSV.

## Vedi Anche

- [Documentazione di csvtool](https://github.com/CSVKit/csvkit/blob/master/docs/tutorial/1%20-%20Working%20with%20CSV%20files.md)
- [Introduzione a CSV](https://www.dataquest.io/blog/csv-tutorial/)
- [Manipolazione di CSV con awk](https://www.cyberciti.biz/faq/awk-bash-scripting-extract-columns-from-csv-file/)