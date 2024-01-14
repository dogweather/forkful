---
title:                "Clojure: Utilizzo dei file csv"
simple_title:         "Utilizzo dei file csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché
La lavorazione dei file CSV è fondamentale per la gestione di dati strutturati. In Clojure, è possibile manipolare questi file in modo efficiente e conciso, risultando uno strumento utile per l'analisi e la visualizzazione dei dati.

## Come Fare
Per lavorare con i file CSV in Clojure, è necessario utilizzare la libreria clojure.data.csv. Ecco un esempio di codice che legge un file CSV e lo stampa su console:

```Clojure 
(require '[clojure.data.csv :as csv])
(with-open [file (clojure.java.io/reader "data.csv")]
  (doall
    (csv/read-csv file)))
```
L'output di questo codice sarà una sequenza di sequenze di valori, corrispondenti alle righe e alle colonne del file CSV.

## Approfondimento
La libreria clojure.data.csv offre diverse funzioni per lavorare con file CSV, come la possibilità di specificare il delimitatore e il carattere di qualificazione dei valori. Inoltre, è possibile scrivere nuovi file CSV utilizzando la funzione ```clojure.data.csv/write-csv```.

## Vedi Anche
- [Documentazione della libreria clojure.data.csv](https://clojure.github.io/data.csv/)
- [Esempi di lavorazione di file CSV in Clojure](https://github.com/clojure/data.csv/blob/master/examples/examples.clj)