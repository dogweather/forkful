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

## Why

CSV (Comma Separated Values) files are commonly used for storing and exchanging tabular data. This makes them essential for many data-related tasks, such as data analysis, data manipulation, and data migration. As such, learning how to work with CSV files is a valuable skill for anyone working with data in the digital age.

## How To

To start working with CSV files in Go, you first need to import the built-in "encoding/csv" package. This package provides functions for reading and writing CSV files.

To read data from a CSV file, you can use the `ReadFile()` function, passing in the name of the file and a `File` struct. Here's an example:

```Go
import (
	"encoding/csv"
	"log"
	"os"
)

// read data from a CSV file
func main() {
    // open the CSV file
    file, err := os.Open("data.csv")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    // read file as a CSV
    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        log.Fatal(err)
    }

    // print out the records
    for _, r := range records {
        fmt.Println(r)
    }
}
```

This code uses the `ReadAll()` function to read all the records in the CSV file and prints them out. You can also access individual fields by using the `Read()` function, which reads one record at a time.

To write data to a CSV file, you can use the `Write()` function, passing in a `File` struct and a slice of strings (representing the values for each field). Here's an example:

```Go
import (
	"encoding/csv"
	"log"
	"os"
)

// write data to a CSV file
func main() {
    // open the CSV file
    file, err := os.Create("output.csv")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    // create a CSV writer
    writer := csv.NewWriter(file)

    // set headers
    writer.Write([]string{"Name", "Age", "Occupation"})

    // write data
    writer.Write([]string{"John Doe", "35", "Software Engineer"})
    writer.Write([]string{"Jane Smith", "28", "Data Analyst"})

    // flush the writer to write the changes to the file
    writer.Flush()
}
```

This code creates a new CSV file called "output.csv" and writes data to it in a format that can be easily read by other applications.

## Deep Dive

Besides reading and writing, the "encoding/csv" package also provides functions for parsing and formatting CSV data. For example, the `ParseCSV()` function can be used to parse CSV data from a string. Here's an example:

```Go
import (
	"encoding/csv"
	"log"
)

// parse CSV data from a string
func main() {
    // dummy CSV data
    csvData := "Name,Age,Occupation\nJohn Doe,35,Software Engineer\nJane Smith,28,Data Analyst"

    // parse the CSV data into a slice of slices
    records, err := csv.ParseCSV(csvData)
    if err != nil {
        log.Fatal(err)
    }

    // print out the records
    for _, r := range records {
        fmt.Println(r)
    }
}
```

This code uses the `ParseCSV()` function to parse the CSV data from `csvData` and prints out the records.

## See Also

- Official Go documentation for "encoding/csv" package: https://golang.org/pkg/encoding/csv/
- Tutorial on working with CSV files in Go: https://rshipp.com/go-csv-parse/#parsing-csv-in-go
- GitHub repository with examples of working with CSV files in Go: https://github.com/gocarina/gocsv