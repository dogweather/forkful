---
title:                "Praca z formatem csv"
html_title:           "Python: Praca z formatem csv"
simple_title:         "Praca z formatem csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Working with CSV (Comma Separated Values) is a common task for programmers, especially when dealing with large amounts of data. CSV is a simple file format that stores tabular data in plain text, with each row representing a line and each column separated by a comma. Programmers often work with CSV files to analyze and manipulate data in a more efficient way.

## Jak to zrobić:
```python
import csv

# Reading a CSV file
with open('data.csv', 'r') as csv_file:
    csv_reader = csv.reader(csv_file)
    
    # Print each row
    for row in csv_reader:
        print(row)
        
# Writing to a CSV file
with open('output.csv', 'w') as csv_file:
    csv_writer = csv.writer(csv_file)
    data = [['Name', 'Age', 'Gender'], ['John', 25, 'Male'], ['Lisa', 30, 'Female']]
    
    # Write each row
    for row in data:
        csv_writer.writerow(row)
```

Output:
```
['Name', 'Age', 'Gender']
['John', '25', 'Male']
['Lisa', '30', 'Female']
```

## Głębsza analiza:
CSV formatę has been around since the early 1970s and was created as a way to easily transfer data between different computer systems. While CSV is a popular choice for storing and exchanging data, it is not the only option. Other file formats such as JSON and XML also offer similar capabilities. Working with CSV also has its drawbacks, such as lack of data type enforcement and difficulty handling large files.

## Zobacz także:
- [Python CSV module](https://docs.python.org/3/library/csv.html)
- [History of CSV Format](https://en.wikipedia.org/wiki/Comma-separated_values#History)