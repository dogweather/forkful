---
title:                "Go recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma Separated Values) files are a popular way to store and exchange data in a tabular format. They are widely used in data analysis, data transfer between different systems, and in many other applications. If you are a Go developer, it is essential to know how to work with CSV files to efficiently handle data in your programs.

## How To

To start working with CSV files in Go, you will need to import the `encoding/csv` package. This package has functions that allow you to read and write CSV files.

To read a CSV file, you can use the `csv.NewReader()` function. It takes as input a `io.Reader` interface, which can be a file, a network connection or any other source of data. Once you have the reader, you can use the `Read()` method to read the file line by line. Each line of the file will be returned as an array of strings representing the columns of that line.

```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {
	// Open the CSV file
	file, err := os.Open("data.csv")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	// Create a new CSV reader
	reader := csv.NewReader(file)

	// Read the file line by line
	for {
		record, err := reader.Read()
		if err != nil {
			fmt.Println("Error reading line:", err)
			break
		}
		// Print the columns of each line
		fmt.Println(record)
	}
}
```

The output of this example will look like this:

```
[Name Age Email]
[John 25 john@example.com]
[Amy 30 amy@example.com]
[David 40 david@example.com]
```

To write to a CSV file, you can use the `csv.NewWriter()` function. It takes as input a `io.Writer` interface, which can be a file, a network connection or any other destination for the data. You can then use the `Write()` method to write each line of the file, passing in an array of strings representing the columns of that line.

```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {
	// Create a new CSV file
	file, err := os.Create("output.csv")
	if err != nil {
		fmt.Println("Error creating file:", err)
		return
	}
	defer file.Close()

	// Create a new CSV writer
	writer := csv.NewWriter(file)

	// Write data to the file
	writer.Write([]string{"Name", "Age", "Email"})
	writer.Write([]string{"John", "25", "john@example.com"})
	writer.Write([]string{"Amy", "30", "amy@example.com"})
	writer.Write([]string{"David", "40", "david@example.com"})

	// Flush the writer to write the data to the file
	writer.Flush()
}
```

This code will create a CSV file named `output.csv` with the following content:

```
Name,Age,Email
John,25,john@example.com
Amy,30,amy@example.com
David,40,david@example.com
```

## Deep Dive

While working with CSV files, one point to keep in mind is the potential presence of a header row. Some CSV files have a header row as the first line, which contains the names of the columns. You can use the `ReadAll()` method to read the entire file at once, and it will return a slice of all the records. You can then access the header row using `records[0]` and use it to map the columns to the respective data in the records.

It is also essential to handle errors while reading and writing CSV files, as any issues with the file format or the data can result in unexpected behavior in your program.

## See Also

- [The `encoding/csv` package documentation](https://golang.org/pkg/encoding/csv/)
- [A tutorial on working with CSV files in Go](https://www.thepolyglotdeveloper.com/2017/03/parse-csv-string-go-programming-language/)