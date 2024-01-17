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

## What & Why?
CSV stands for Comma Separated Values and is a file format used to store tabular data in a plain text form. It is widely used by programmers as it allows for easy data manipulation and sharing across different platforms and programming languages.

## How to:
Working with CSV files in Python is simple and straightforward. There are two main libraries that we can use: ```csv``` and ```pandas```.

### Using ```csv``` library
To read a CSV file, we first need to import the ```csv``` library. Then, we can use the ```reader``` function to create a reader object which allows us to iterate through each row of the CSV file.

```
import csv

with open('data.csv') as csv_file:
    csv_reader = csv.reader(csv_file)
    for row in csv_reader:
        print(row)
```
The output of this code will be each row of the CSV file printed as a list.

To write data to a CSV file, we use the ```writer``` function. We can either pass in a list of values or use the ```writerow``` function to write them one row at a time.

```
import csv

with open('data.csv', 'w') as csv_file:
    csv_writer = csv.writer(csv_file)
    csv_writer.writerow(['Name', 'Age', 'Occupation'])
    csv_writer.writerow(['John', 25, 'Teacher'])
    csv_writer.writerow(['Jane', 30, 'Engineer'])
```
This code will write a new CSV file with three columns: Name, Age, and Occupation.

### Using ```pandas``` library
The ```pandas``` library provides more advanced data manipulation options for CSV files. We can read a CSV file using the ```read_csv``` function and store the data in a ```DataFrame```.

```
import pandas as pd

data = pd.read_csv('data.csv')
```
We can then use various functions and methods to manipulate and analyze the data, such as selecting specific columns or rows, grouping data, or calculating summary statistics. We can also write a DataFrame to a CSV file using the ```to_csv``` function.

```
import pandas as pd

data = pd.read_csv('data.csv')
print(data['Occupation']) # prints the values in the Occupation column

# write a new CSV file with only the Name and Age columns
data[['Name', 'Age']].to_csv('new_data.csv')
```

## Deep Dive
CSV files have been around since the early days of computing and were originally used to transfer data between databases and spreadsheets. They are a common format for storing data in a tabular form and are widely supported by many different applications and programming languages.

An alternative to using CSV files for data storage is using a database, which allows for faster and more efficient data retrieval and manipulation. However, CSV files still have their benefits, such as being more lightweight and easier to share with others.

When working with CSV files, it is important to handle any potential issues with encoding and formatting of data. The ```csv``` library in Python has options for specifying different dialects and encoding types, while the ```pandas``` library has built-in functions for handling different data types and formatting.

## See Also
- [Official documentation for ```csv``` library](https://docs.python.org/3/library/csv.html)
- [Official documentation for ```pandas``` library](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.read_csv.html)
- [Working with CSV files in Python tutorial by RealPython](https://realpython.com/python-csv/)