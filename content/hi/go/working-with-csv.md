---
title:                "CSV के साथ काम करना"
html_title:           "Go: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma Separated Values) is a popular format used for storing and exchanging tabular data. If you're working with data, whether it's for data analysis, database management, or any other task, chances are you'll come across CSV files. Learning how to work with CSV in Go can greatly improve your ability to manage and manipulate data efficiently.

## How To

Working with CSV in Go is made easy with the built-in "encoding/csv" package. Let's take a look at a simple example of how to read data from a CSV file and display it in the console:

```
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

	// Create a new CSV reader
	reader := csv.NewReader(file)

	// Read all rows from the CSV file
	rows, err := reader.ReadAll()
	if err != nil {
		fmt.Println("Error while reading file:", err)
		return
	}

	// Loop through rows and display data
	for _, row := range rows {
		fmt.Println(row)
	}
}
```

In this code, we first import the "encoding/csv" package, which provides us with the necessary functions to work with CSV files. Then, we open our CSV file and create a new CSV reader. Using the "ReadAll()" function, we can easily read all the rows from the file. Finally, we loop through the rows and display each row in the console.

Now, let's see how we can write data to a CSV file using Go:

```
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {

	// Create a new CSV writer
	file, err := os.Create("new_data.csv")
	if err != nil {
		fmt.Println("Error creating file:", err)
		return
	}

	writer := csv.NewWriter(file)

	// Write data to the CSV file
	data := [][]string{
		{"Name", "Age", "Country"},
		{"John", "25", "USA"},
		{"Jane", "30", "Canada"},
		{"Tom", "22", "UK"},
	}

	for _, row := range data {
		err := writer.Write(row)
		if err != nil {
			fmt.Println("Error writing row:", err)
			return
		}
	}

	// Flush and close the writer
	writer.Flush()
	file.Close()
}
```

We first create a new CSV writer and then use the "Write()" function to write our data to the file. It is important to call the "Flush()" function to ensure all the data is written to the file before closing it.

## Deep Dive

The "encoding/csv" package also provides us with additional functionalities for working with CSV files. Some notable ones include:

- Customizing the delimiter (default is comma)
- Adding headers to the CSV file
- Reading specific columns instead of all columns
- Reading and writing to and from a CSV file using struct types

For more information, you can refer to the official documentation of the "encoding/csv" package or check out some of the helpful resources in the "See Also" section below.

## See Also

- Official Documentation: https://golang.org/pkg/encoding/csv/
- Working with CSV files in Go (tutorial): https://www.sohamkamani.com/golang/working-with-csv-files/
- CSV file handling in Go (video): https://youtu.be/vruvz_WW0Bc