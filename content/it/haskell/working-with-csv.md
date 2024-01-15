---
title:                "Lavorare con i file csv"
html_title:           "Haskell: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei un appassionato di dati o lavori con grandi quantità di informazioni, probabilmente hai incontrato file CSV (Comma Separated Values). Questo formato di file è ampiamente utilizzato per memorizzare dati in forma tabellare, rendendolo uno strumento utile per analisi e elaborazioni di dati. Utilizzando Haskell, puoi facilmente manipolare e gestire i file CSV in modo efficiente e preciso.

## Come fare

Per lavorare con i file CSV in Haskell, abbiamo bisogno di alcuni moduli esterni. Includiamo il modulo "csv", che ci fornisce funzioni per leggere e scrivere file CSV e il modulo "vector", che ci permette di lavorare con i dati in forma di vettori anziché liste.

```Haskell
import Text.CSV
import Data.Vector
```

Ora possiamo caricare il nostro file CSV in un vettore utilizzando la funzione "parseCSVFromFile". Assicuriamoci di avere il file "data.csv" nella stessa cartella del nostro codice.

```Haskell
file <- parseCSVFromFile "data.csv"
let data = fromRight (fromList []) file
```

Possiamo ora stampare il contenuto del nostro vettore utilizzando la funzione "print" e specificando l'indice desiderato.

```Haskell
print (data ! 0)
```

Questo ci fornirà la prima riga del nostro file CSV.

```
["Nome","Cognome","Età"]
```

Possiamo anche filtrare il nostro vettore in base a delle condizioni utilizzando la funzione "filter" e specificando una funzione di controllo. Ad esempio, se vogliamo solo i dati delle persone con un'età superiore a 30, possiamo fare così:

```Haskell
let filteredData = filter (\row -> (row ! 2) > "30") data
print filteredData
```

```
[["John","Smith","35"], ["Maria","Rossi","32"]]
```

## Approfondimento

Oltre alle funzioni di base per leggere e scrivere file CSV, il modulo "csv" ci fornisce anche alcune funzioni utili per manipolare i nostri dati. Ad esempio, la funzione "mapRows" ci permette di applicare una determinata funzione a ogni riga del nostro vettore, mentre la funzione "groupBy" ci permette di raggruppare i dati in base a una determinata colonna.

Un'altra funzionalità interessante di Haskell per lavorare con i file CSV è l'utilizzo delle espressioni "do". Ciò ci consente di scrivere codice più leggibile e strutturato, senza dover utilizzare troppe funzioni nidificate.

```Haskell
do
  file <- parseCSVFromFile "data.csv"
  let data = fromRight (fromList []) file
  let filteredData = filter (\row -> (row ! 2) > "30") data
  print filteredData
```

Inoltre, se stiamo lavorando con file CSV molto grandi, possiamo anche utilizzare il metodo di lettura "loadCsvFile" del modulo "Data.Csv.Streaming". Questo ci permette di leggere i dati dal file a blocchi, riducendo il carico sulla memoria del nostro programma.

## Vedi anche

- [Haskell Basics](https://wiki.haskell.org/Introduction)
- [Haskell Documentation](https://www.haskell.org/documentation)
- [Working with CSVs in Haskell](https://www.haskell.org/documentation#csv)