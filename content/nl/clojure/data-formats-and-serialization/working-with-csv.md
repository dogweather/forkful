---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:02.630943-07:00
description: Hoe? Laten we onze mouwen opstropen en een CSV-bestand parsen in Clojure.
lastmod: '2024-04-05T22:38:49.302121-06:00'
model: gpt-4-0125-preview
summary: Hoe? Laten we onze mouwen opstropen en een CSV-bestand parsen in Clojure.
title: Werken met CSV
weight: 37
---

## Hoe?
Laten we onze mouwen opstropen en een CSV-bestand parsen in Clojure.

```Clojure
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(with-open [reader (io/reader "data.csv")]
  (let [data (csv/read-csv reader)]
    (doseq [row data]
      (println row))))
```

Voorbeelduitvoer voor een CSV met "naam,leeftijd" zou zijn:

```Clojure
["John" "30"]
["Jane" "25"]
["Doe" "40"]
```

Om gegevens naar een CSV-bestand te schrijven:

```Clojure
(with-open [writer (io/writer "output.csv")]
  (csv/write-csv writer [["naam" "leeftijd"]
                         ["John" "30"]
                         ["Jane" "25"]
                         ["Doe" "40"]]))
```

Dit schrijft de gegeven rijen naar `output.csv`.

## Diepere Duik
De omgang met CSV in Clojure is vrij eenvoudig vergeleken met andere talen - geen extra franje. Historisch gezien maakte de eenvoud van CSV het wijdverspreid voor gegevensuitwisseling, en dateert het van voor vele gegevensformaten. Alternatieven zijn onder andere JSON, XML, of YAML, maar CSV wint waar eenvoud of compatibiliteit met spreadsheets sleutel is. De `clojure.data.csv` bibliotheek biedt het gereedschap voor CSV-parsing en -schrijven, gebouwd op de efficiënte I/O-stromen van Java voor goede prestaties.

## Zie Ook
1. De CSV-bibliotheek van Clojure: [https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)
2. Lees meer over CSV: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
3. Voor een duik in Clojure: [https://clojure.org/](https://clojure.org/)
