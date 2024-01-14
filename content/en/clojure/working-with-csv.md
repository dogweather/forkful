---
title:                "Clojure recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Why
CSV (Comma Separated Values) files are a common data format used for storing tabular data. It is often used in business and scientific applications to store large datasets in a simple and readable format. As a functional programming language, Clojure provides powerful tools for working with CSV files, making it an ideal choice for data manipulation and analysis.

## How To
To work with CSV files in Clojure, we will be using the `clojure.data.csv` library. This library provides functions for parsing, reading, and writing CSV files.

To start, we will first need to include the library in our project by adding the following dependency to our `project.clj` file:

```Clojure
[clojure/data.csv "1.0.0"]
```

Next, let's require the library in our Clojure file:

```Clojure
(:require [clojure.data.csv :as csv])
```

### Reading CSV Files
To read a CSV file, we can use the `clojure.data.csv` function `parse-csv`. This function takes in a string or a file input and returns a sequence of sequences, where each inner sequence represents a row in the CSV file.

Let's say we have a CSV file named `data.csv` with the following content:

```
Name, Age, Occupation
John, 30, Engineer
Mary, 25, Teacher
```

We can read this file and store the data in a variable using the following code:

```Clojure
(def data (csv/parse-csv "data.csv"))
```

We can then access specific rows or columns by using standard Clojure functions like `first`, `second`, `nth`, etc.

### Writing CSV Files
To write data to a CSV file, we can use the `csv/write-csv` function. This function takes in a sequence of sequences representing the data and outputs a string or a file with the CSV format.

Let's say we have the following list of lists representing data:

```Clojure
(def data [["Name" "Age" "Occupation"]
           ["John" 30 "Engineer"]
           ["Mary" 25 "Teacher"]])
```

We can write this data to a CSV file named `output.csv` using the following code:

```Clojure
(csv/write-csv "output.csv" data)
```

This will create a new file with the following content:

```
Name, Age, Occupation
John, 30, Engineer
Mary, 25, Teacher
```

## Deep Dive
The `clojure.data.csv` library provides many useful options for handling different types of CSV files. These options can be specified as additional parameters when using the `parse-csv` and `write-csv` functions.

Some of the notable options include specifying custom delimiters, handling headers, and choosing different string encodings.

For a full list of available options and their usage, check out the official [clojure.data.csv documentation](https://clojure.github.io/data.csv/).

## See Also
- [Official documentation for clojure.data.csv library](https://clojure.github.io/data.csv/)
- [Tutorial on working with CSV files in Clojure](https://purelyfunctional.tv/courses/csv-files-in-clojure/)
- [Using Clojure to process large CSV files](https://medium.com/@ianterrell/how-i-built-a-fast-csv-importer-with-clojure-173b7fc75942)