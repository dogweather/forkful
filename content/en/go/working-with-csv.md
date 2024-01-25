---
title:                "Working with CSV"
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV in programming means reading from and writing to comma-separated values files—a simple, plain-text storage format for tabular data. Programmers use it because it's widely supported, easy to create and interpret, and simple to import into databases and spreadsheet programs.

## How to:
### Reading a CSV file:
```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("data.csv")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	reader := csv.NewReader(file)
	records, err := reader.ReadAll()
	if err != nil {
		panic(err)
	}

	for _, record := range records {
		fmt.Println(record)
	}
}
```
### Writing to a CSV file:
```Go
package main

import (
	"encoding/csv"
	"os"
)

func main() {
	records := [][]string{
		{"Name", "Age", "City"},
		{"Alice", "25", "New York"},
		{"Bob", "30", "San Francisco"},
	}

	file, err := os.Create("output.csv")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	for _, record := range records {
		if err := writer.Write(record); err != nil {
			panic(err)
		}
	}
}
```

## Deep Dive
The CSV format has been around since the early 1970s, originating from the IBM Fortran (level G) compiler. While JSON or XML formats might offer more features and complexity, CSV stands its ground due to sheer simplicity. In Go, the `encoding/csv` package handles CSV parsing and serialization. This package supports customization, like setting different field delimiters or handling variable numbers of fields per record. While it doesn't handle every CSV variant, it works excellently for standard CSV formats.

## See Also
For more on working with CSV in Go, check out these resources:
- The official Go documentation for the [`csv` package](https://pkg.go.dev/encoding/csv).
- Check out [Go by Example](https://gobyexample.com/reading-files) for additional file reading and writing procedures.