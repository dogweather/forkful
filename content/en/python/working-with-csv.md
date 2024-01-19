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

# Working with CSV in Python

## What & Why?
Comma Separated Values (CSV) files are a universal data format used to store tabular data, like a spreadsheet. Programmers often need to read and write CSV files in Python for data processing tasks.

## How to:
Python's CSV module makes it easy to read and write CSV files. Here's how you do it:

Read csv:

```python
import csv

with open('file.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)
```
This code reads `file.csv` and prints its content line by line.

Write to csv:

```python
import csv

with open('file.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(["SN", "Name", "Contribution"])
    writer.writerow([1, "Linus Torvalds", "Linux Kernel"])
```
This code writes rows to `file.csv`.

## Deep Dive
CSV date back to the '70s. Its simplicity and human-readability are why it's still widely used. Alternatives include JSON and XML, but CSV is great when handling 2D numerical and text data. Python's CSV module implementation uses a `Dialect` class to hold together many CSV-specific format traits that let you customize your data handling process.

## See Also
[Python's CSV library documentation](https://docs.python.org/3/library/csv.html), [Real Python's article on handling CSV data](https://realpython.com/python-csv/).