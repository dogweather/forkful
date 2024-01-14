---
title:                "Python recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma-Separated Values) is a common format for storing and exchanging data. It is used in various applications and industries, making it a valuable skill for any developer to have. Working with CSV files allows you to easily read, manipulate, and analyze large amounts of data in a structured format.

## How To

To work with CSV in Python, we will be using the built-in `csv` module. First, we need to import the module in our code:

```Python
# Import csv module
import csv 
```

Next, we will open our CSV file and use a `csv.reader()` object to read the data. The `delimiter` argument specifies the character used to separate the values in the file, which in most cases is a comma.

```Python
# Open CSV file
with open('data.csv') as csv_file:
    # Create csv.reader object
    csv_reader = csv.reader(csv_file, delimiter=',')

    # Loop through each row in the file
    for row in csv_reader:
        # Access the values in the current row
        print(row)
```

Assuming our CSV file has the following data:

```
Name,Age,Country
John,28,USA
Emily,24,Canada
```

The output would be:

```
['Name', 'Age', 'Country']
['John', '28', 'USA']
['Emily', '24', 'Canada']
```

We can also use the `csv.writer()` object to write data to a CSV file. Let's say we want to create a new CSV file called `output.csv` and write some data to it:

```Python
# Open output CSV file in write mode
with open('output.csv', mode='w') as csv_file:
    # Create csv.writer object
    csv_writer = csv.writer(csv_file, delimiter=',')

    # Write data to CSV file
    csv_writer.writerow(['Name', 'Age'])
    csv_writer.writerow(['Jane', 32])
    csv_writer.writerow(['Mike', 36])
```

This will result in a new CSV file with the following data:

```
Name,Age
Jane,32
Mike,36
```

## Deep Dive

There are many other useful functions and options in the `csv` module for working with CSV files. Here are some examples:

- `csv.DictReader()` and `csv.DictWriter()` can be used to read and write CSV files as dictionaries, which make it easier to access specific values in larger datasets.
- The `quotechar` and `quoting` arguments in `csv.reader()` and `csv.writer()` allow us to handle special characters and formatting in our data.
- We can also specify our own dialect for the CSV file, if it doesn't follow the standard conventions, using the `csv.register_dialect()` function.

For a more comprehensive understanding of the `csv` module, check out the official documentation [here](https://docs.python.org/3/library/csv.html).

## See Also

- [Working with CSV files in Python](https://realpython.com/python-csv/)
- [Python Module of the Week - csv](https://pymotw.com/3/csv/index.html)
- [Reading and Writing CSV Files in Python](https://www.geeksforgeeks.org/working-csv-files-python/)