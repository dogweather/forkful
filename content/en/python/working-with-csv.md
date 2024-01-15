---
title:                "Working with csv"
html_title:           "Python recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

Working with CSV (Comma Separated Values) files is a common task in many data analysis and manipulation projects. CSV files are widely used for storing and sharing tabular data, making them an essential skill for anyone working with data.

## How To

To work with CSV files in Python, we first need to import the built-in **csv** module. This module provides functions for reading and writing to CSV files.

```Python
import csv

# Open CSV file in read mode
with open('data.csv', 'r') as csvfile:
    # Create CSV reader object
    reader = csv.reader(csvfile)
    # Loop through each row in the CSV file
    for row in reader:
        # Do something with the row data
        print(row)
```

In the code above, we open the CSV file named "data.csv" and use the `csv.reader()` function to create a reader object. Then, we loop through each row in the CSV file and print its data.

To write data to a CSV file, we use the `csv.writer()` function. 

```Python
import csv

# Open CSV file in write mode
with open('data.csv', 'w') as csvfile:
    # Create CSV writer object
    writer = csv.writer(csvfile)

    # Create a list of data
    data = [
        ['Name', 'Age', 'City'],
        ['John', '25', 'New York'],
        ['Jane', '30', 'London'],
        ['Bob', '45', 'Tokyo']
    ]

    # Write data to CSV file
    writer.writerows(data)
```

In the code above, we open the CSV file in write mode and use the `csv.writer()` function to create a writer object. Then, we use the `writerows()` function to write multiple rows of data to the file.

## Deep Dive

CSV files typically have a header row that contains the column names. To skip the header row when reading a CSV file, we can use the `next()` function.

```Python
import csv

# Open CSV file in read mode
with open('data.csv', 'r') as csvfile:
    # Create CSV reader object
    reader = csv.reader(csvfile)
    # Skip header row
    next(reader)
    # Loop through each row in the CSV file
    for row in reader:
        # Do something with the row data
        print(row)
```

We can also specify the delimiter used in the CSV file by passing it as an argument to the `csv.reader()` or `csv.writer()` functions. By default, the delimiter is a comma, but it can be changed to suit the format of the CSV file.

```Python
# Reading a pipe-delimited CSV file
reader = csv.reader(csvfile, delimiter='|')

# Writing to a tab-delimited CSV file
writer = csv.writer(csvfile, delimiter='\t')
```

## See Also

For more information on working with CSV files in Python, check out the following resources:

- [Official Python 3 Documentation for CSV Module](https://docs.python.org/3/library/csv.html)
- [Real Python Tutorial on Reading and Writing CSV Files](https://realpython.com/python-csv/)
- [Tutorialspoint Guide to CSV in Python](https://www.tutorialspoint.com/python/python_csv_files.htm)