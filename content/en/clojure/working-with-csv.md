---
title:                "Working with CSV"
date:                  2024-01-19
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) means processing tabular data stored in a plain-text format. Programmers do it because handling CSV is a common need for data exchange and quick storage, since it's readable, simple, and supported by numerous tools.

## How to:

Let's roll up our sleeves and parse a CSV file in Clojure.

```Clojure
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(with-open [reader (io/reader "data.csv")]
  (let [data (csv/read-csv reader)]
    (doseq [row data]
      (println row))))
```

Sample output for a CSV with "name,age" might be:

```Clojure
["John" "30"]
["Jane" "25"]
["Doe" "40"]
```

To write data to a CSV file:

```Clojure
(with-open [writer (io/writer "output.csv")]
  (csv/write-csv writer [["name" "age"]
                         ["John" "30"]
                         ["Jane" "25"]
                         ["Doe" "40"]]))
```

This writes the given rows to `output.csv`.

## Deep Dive

CSV handling in Clojure is pretty straightforward compared to other languages - no extra fluff. Historically, CSV's simplicity made it widespread for data interchange, predating many data formats. Alternatives include JSON, XML, or YAML, but CSV wins where simplicity or spreadsheet compatibility is key. The `clojure.data.csv` library provides the nuts and bolts for CSV parsing and writing, built upon Java's efficient I/O streams for good performance.

## See Also

1. Clojure's CSV library: [https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)
2. Read up more on CSV: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
3. For a dive into Clojure: [https://clojure.org/](https://clojure.org/)
