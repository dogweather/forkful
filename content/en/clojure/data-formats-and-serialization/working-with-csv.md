---
date: 2024-02-03 19:03:00.222394-07:00
description: "How to: Clojure doesn't have built-in CSV parsing in its standard library,\
  \ but you can use the `clojure.data.csv` library for this purpose. First, add the\u2026"
lastmod: '2024-03-13T22:44:59.765357-06:00'
model: gpt-4-0125-preview
summary: Clojure doesn't have built-in CSV parsing in its standard library, but you
  can use the `clojure.data.csv` library for this purpose.
title: Working with CSV
weight: 37
---

## How to:


### Reading a CSV File
Clojure doesn't have built-in CSV parsing in its standard library, but you can use the `clojure.data.csv` library for this purpose. First, add the library to your project dependencies.

In your `project.clj`, add the following dependency:
```clojure
[clojure.data.csv "1.0.0"]
```
To read a CSV file and print each row:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "path/to/yourfile.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
This will output each row of the CSV as a Clojure vector.

### Writing to a CSV File
To write data to a CSV file, you can use the same `clojure.data.csv` library:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "path/to/outputfile.csv")]
    (csv/write-csv writer data)))
```
This creates or overwrites `outputfile.csv`, filling it with the specified data.

### Using a Third-Party Library: `clojure.data.csv`
While `clojure.data.csv` is arguably the most straightforward library for CSV handling in Clojure, for more complex tasks, such as handling CSVs with special characters or unconventional delimiters, you might explore additional options within the ecosystem or even consider Java interop with libraries such as Apache Commons CSV. However, for most standard CSV processing tasks in Clojure, `clojure.data.csv` provides a simple and effective toolset.
