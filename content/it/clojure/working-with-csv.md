---
title:                "Lavorare con i file csv"
html_title:           "Clojure: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con i file CSV è un'attività comune per i programmatori poiché i dati sono spesso memorizzati in questo formato. CSV, o Comma-Separated Values, è un formato di file di testo che utilizza una virgola come separatore per organizzare i dati in colonne e righe.

## Come fare:

```Clojure
(require '[clojure.data.csv :as csv])

;; Caricare un file CSV in una collezione
(csv/read-csv "mio_file.csv")

;; Scrivere una collezione in un file CSV
(csv/write-csv "nuovo_file.csv" [["Gianni" "Rossi" 25]
                                ["Maria" "Bianchi" 33]
                                ["Luca" "Verdi" 42]])

;; Con questo codice, convertiamo la collezione in una struttura di dati tabellare che può essere facilmente manipolata dal programma.

```

## Approfondimento:

Il formato CSV è stato introdotto negli anni '70 per semplificare lo scambio di dati tra le applicazioni. Oggi, nonostante l'uso di diverse alternative più avanzate, il CSV è ancora ampiamente utilizzato per la sua semplicità e praticità. Nel mondo Clojure, esistono diverse librerie per lavorare con i file CSV, tra cui la libreria standard `clojure.data.csv` e la popolare libreria `incanter.io`.

## Vedi anche:

- [Documentazione ufficiale della libreria clojure.data.csv](https://clojure.github.io/data.csv/)
- [Libreria incanter.io per il lavoro scientifico in Clojure](https://incanter.github.io/incanter/tables.html)