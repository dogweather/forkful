---
title:                "Lavorare con i CSV"
date:                  2024-02-03T19:19:02.289171-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Lavorare con file CSV (Valori Separati da Virgola) comporta l'analisi e la generazione di dati testuali strutturati come righe e colonne, simili ai dati di un foglio di calcolo. Questo processo è essenziale per lo scambio di dati tra applicazioni, database e per compiti di trasformazione dei dati, a causa dell'ampia adozione del CSV come formato leggero e interoperabile.

## Come fare:

### Leggere un File CSV
Clojure non ha un'analisi CSV incorporata nella sua libreria standard, ma puoi usare la libreria `clojure.data.csv` per questo scopo. Prima, aggiungi la libreria alle dipendenze del tuo progetto.

Nel tuo `project.clj`, aggiungi la seguente dipendenza:
```clojure
[clojure.data.csv "1.0.0"]
```
Per leggere un file CSV e stampare ogni riga:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "path/to/yourfile.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
Questo stamperà ogni riga del CSV come un vettore di Clojure.

### Scrivere su un File CSV
Per scrivere dati su un file CSV, puoi usare la stessa libreria `clojure.data.csv`:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "path/to/outputfile.csv")]
    (csv/write-csv writer data)))
``` 
Questo crea o sovrascrive `outputfile.csv`, riempiendolo con i dati specificati.

### Usare una Libreria di Terze Parti: `clojure.data.csv`

Sebbene `clojure.data.csv` sia probabilmente la libreria più diretta per la gestione dei CSV in Clojure, per compiti più complessi, come la gestione di CSV con caratteri speciali o delimitatori non convenzionali, potresti esplorare opzioni aggiuntive all'interno dell'ecosistema o addirittura considerare l'interop di Java con librerie come Apache Commons CSV. Tuttavia, per la maggior parte dei compiti standard di elaborazione CSV in Clojure, `clojure.data.csv` fornisce un insieme di strumenti semplice ed efficace.
