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

## Why

If you are a data analyst or work with large datasets, chances are you have encountered CSV (Comma-Separated Values) files. These files are popular for storing and exchanging data due to their simplicity and universal compatibility. Learning how to work with CSV files in Clojure can greatly enhance your data manipulation skills and save you time in your projects.

## How To

Working with CSV files in Clojure is made easy with the [clojure-csv](https://github.com/clojure/data.csv) library. First, we need to import the library:

```Clojure
(require '[clojure.data.csv :as csv])
```

To read a CSV file, we can use the `csv/read-csv` function. Let's say we have a file named "data.csv" that contains the following data:

```
Name, Age, Occupation
John, 25, Engineer
Sarah, 29, Designer
Chris, 32, Developer
```

We can read this file and store it into a variable:

```Clojure
(def data (csv/read-csv "data.csv"))
```

The `data` variable will be a sequence of sequences, where each inner sequence represents a row in the CSV file. We can access and manipulate this data using Clojure functions, such as `map` and `filter`. For example, let's say we want to filter the data to only include people over the age of 30:

```Clojure
(def filtered-data (filter #(< 30 (Integer. (nth % 1))) data))
```

The `filtered-data` variable will now contain:

```
(Sarah, 29, Designer)
(Chris, 32, Developer)
```

We can also write data to a CSV file using the `csv/write-csv` function. Let's write the `filtered-data` variable to a new file named "filtered_data.csv":

```Clojure
(csv/write-csv "filtered_data.csv" filtered-data)
```

This will create a new CSV file with the filtered data.

## Deep Dive

The `csv/read-csv` function allows us to specify various options for parsing CSV files. For example, we can specify a custom delimiter or quote character, or specify headers for our data. This gives us more control over how our CSV files are read and handled.

Another useful library for working with CSV files in Clojure is [clojure-csv-reader](https://github.com/davidechicco/clojure-csv-reader). This library provides functions for writing CSV files, reading CSV files into different data structures, and even creating CSV data from scratch.

See Also

- [Clojure data.csv library documentation](https://github.com/clojure/data.csv/blob/master/README.md)
- [Clojure csv-reader library documentation](https://github.com/davidechicco/clojure-csv-reader/blob/master/README.md)