---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con CSV significa manipolare dati "Comma-Separated Values" - utile per import/export. I programmatori lo fanno per scambiare dati con altre app, database o sistemi che supportano questo formato universale.

## How to:
```clojure
(require '[clojure.java.io :as io])
(require '[clojure.data.csv :as csv])

; Leggere un file CSV
(with-open [reader (io/reader "dati.csv")]
  (doall (csv/read-csv reader)))

; Scrivere in un file CSV
(let [dati [["nome" "età"] ["Giulia" "30"] ["Marco" "25"]]]
  (with-open [writer (io/writer "nuovi_dati.csv")]
    (csv/write-csv writer dati)))
```

Output di esempio dopo lettura di "dati.csv":
```clojure
(["nome" "età"] ["Giulia" "30"] ["Marco" "25"])
```

## Deep Dive
CSV, esistente dagli anni '70, è semplice e compatibile con fogli di calcolo. Alternativa a JSON/XML per dati tabellari. Clojure, funzionale, tratta CSV come collezioni, offre librerie per manipolazione.

## See Also
- [Clojure Data CSV](https://github.com/clojure/data.csv) - Libreria per lavorare con i CSV.
- [Clojure Cookbook](https://github.com/clojure-cookbook/clojure-cookbook) - Ricette per programmazione Clojure, incluso CSV.
