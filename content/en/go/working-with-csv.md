---
title:                "Working with csv"
html_title:           "Go recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) in Go is the process of reading and manipulating data that is stored in a CSV file, which is a plain text format used to organize and store tabular data. Programmers often work with CSV files when dealing with data from external sources, such as spreadsheets or databases, that need to be converted and processed in their applications.

## How to:
To work with CSV in Go, first import the "encoding/csv" package. Then, use the "Open" function to open the CSV file and create a reader to read the data. Next, use a loop to read each row of the CSV file and access the data using the "Read" and "ReadAll" functions. Lastly, use the "Parse" function to convert the data into the desired data types.

```
import (
  "encoding/csv"
  "os"
)

//Open and read the CSV file
csvFile, err := os.Open("data.csv")
if err != nil {
  fmt.Println("Error:", err)
}
defer csvFile.Close()

//Create a reader to read the data from the CSV file
reader := csv.NewReader(csvFile)

//Read and print each row of the CSV file
for {
  row, err := reader.Read()
  if err == io.EOF {
    break
  }
  if err != nil {
    fmt.Println("Error:", err)
    continue
  }
  fmt.Println(row)
}

//Convert the data into the desired types
records, err := reader.ReadAll()
if err != nil {
  fmt.Println("Error:", err)
}
fmt.Println(records)
```

## Deep Dive:
CSV files have been used since the 1970s and have become a popular way to exchange data between different systems and applications. While Go has its own built-in packages for working with CSV, there are also third-party packages available, such as "gocsv" or "csvutil", which offer additional features and performance optimizations.

When working with CSV in Go, it is important to handle errors properly and consider the performance implications of data conversion and manipulation. Go's strict typing and error handling make it a reliable language for working with CSV data.

## See Also:
- [Package "encoding/csv" documentation](https://golang.org/pkg/encoding/csv/)
- [Third-party packages for working with CSV in Go](https://go.libhunt.com/categories/941-csv)