---
title:                "Working with csv"
html_title:           "Gleam recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma Separated Values) is a commonly used file format for storing and exchanging tabular data. It is a popular choice among developers and data analysts due to its simplicity and versatility. In this article, we will explore how to work with CSV files in Gleam and why it is a useful skill to have in your programming arsenal.

## How To

To start working with CSV files in Gleam, we first need to import the `csv` module and its associated functions:

```Gleam
import csv
```

Next, we need to open the CSV file using the `open` function and providing the file path as an argument:

```Gleam
let file = open("data.csv")
```

Once we have opened the file, we can use the `read` function to read its contents:

```Gleam
let data = file.read()
```

We can then iterate through the data using a `for` loop and access each row's elements using the `get` function:

```Gleam
for row in data {
    let column1 = row.get(0)
    let column2 = row.get(1)

    // Do something with the values
}
```

Finally, we can close the file when we are done working with it using the `close` function:

```Gleam
file.close()
```

### Sample Output

Assuming our CSV file has the following data:

| Column 1 | Column 2 |
|----------|----------|
| Value 1  | Value 2  |
| Value 3  | Value 4  |

The code snippet above would output the following:

```Gleam
column1 = "Value 1"
column2 = "Value 2"

column1 = "Value 3"
column2 = "Value 4"
```

## Deep Dive

When working with CSV files, it is important to note that the data is stored in a 2-dimensional matrix, with rows and columns. This means that you can access specific columns using their index, and specific rows using their row number. For example, if we wanted to access the second row in our CSV file, we would use the following code:

```Gleam
let row2 = data.get(1)
```

We can also use the `length` function to get the number of rows in our CSV file:

```Gleam
let num_rows = data.length()
```

Additionally, we can use the `write` function to write new data to our CSV file. Keep in mind that this will overwrite any existing data, so use it with caution.

```Gleam
let new_data = [["New Value 1", "New Value 2"], ["New Value 3", "New Value 4"]]
file.write(new_data)
```

## See Also

- [Gleam CSV Module Documentation](https://gleam.run/documentation/stdlib/csv/)
- [Working with CSV Files in Python](https://realpython.com/python-csv/)
- [CSV File Format Explained](https://www.howtogeek.com/348960/what-is-a-csv-file-and-how-do-i-open-it/)