---
title:                "Working with csv"
html_title:           "Clojure recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma Separated Values) means being able to create, read, write and manipulate data in a tabular format, where each row represents a record and each column represents a field. Programmers often work with CSV files when dealing with large datasets or when sharing data with other systems. It is a simple and versatile format that allows for easy processing and analysis of data.

## How to:
To work with CSV in Clojure, we first need to use the clojure.java-csv library, which provides functions to read and write CSV files. Let's create a CSV file with some sample data first:

```Clojure
(require '[clojure.java-csv :as csv])

(with-open [writer (csv/writer "sample.csv" :end-of-line "\n")]
  (csv/write-all writer [["Name" "Age" "Country"]
                         ["John" "25" "USA"]
                         ["Jane" "30" "Canada"]
                         ["Mark" "28" "UK"]]))
```
This creates a CSV file called "sample.csv" with three columns (Name, Age, Country) and three rows of data.

To read a CSV file, we can use the csv/read-csv function and provide the file path as an argument:

```Clojure
(with-open [reader (csv/reader "sample.csv")]
  (doall (map csv/read-csv reader)))
```

This will return a list of lists, with each inner list representing a row in the CSV file. We can also specify options such as delimiter and headers to customize how the data is read.

To write data to a CSV file, we can use the csv/write-csv function:

```Clojure
(with-open [writer (csv/writer "new_sample.csv")]
  (doall (map #(csv/write-csv writer %) [["Adam" "35" "Australia"]
                                          ["Sarah" "32" "USA"]])))
```

This will append the new data to the end of the existing CSV file. We can also use the `:end-of-line` option to customize the line endings.

To manipulate data in CSV files, we can use functions like map, filter, and reduce, just like we would with any other data structure in Clojure. Additionally, we can use the csv/parse-csv-string function to convert a CSV string into Clojure data structures.

## Deep Dive:
CSV has been around since the early days of computing and was first standardized in 1972. It is a popular format for storing and exchanging data due to its simplicity and compatibility with different systems and programming languages.

There are a few alternatives to CSV such as JSON, XML, and Excel files, but CSV remains a popular choice for working with structured data due to its ease of use and human readability.

Clojure's java-csv library uses the OpenCSV library under the hood, which is a well-tested and widely used library for working with CSV files in Java.

## See Also:
- [clojure.java-csv documentation](https://clojure.github.io/java.jdbc/)
- [OpenCSV library](http://opencsv.sourceforge.net/)
- [Clojure for Data Science](https://clojure.org/guides/data_science)