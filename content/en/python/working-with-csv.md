---
title:                "Working with CSV"
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) files means reading from and writing data to plain-text files where each row is a data record. Programmers dig CSVs because they're light, human-readable, and jig with nearly any data-processing tool.

## How to:
```python
# Import the CSV module
import csv

# Reading a CSV file
with open('data.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)

# Output:
# ['Name', 'Age', 'City']
# ['Alice', '30', 'New York']
# ...

# Writing to a CSV file
with open('output.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['Name', 'Age', 'City'])
    writer.writerow(['Bob', '22', 'Los Angeles'])

# Check output.csv to see results
```

## Deep Dive
Back when data transmission was slower and storage costlier, CSV gained fans for its simplicity and low overhead. Alternatives like JSON and XML provide structure but at the cost of verbosity. For CSV, parsing speed is a win, but it may struggle with complex hierarchies or data types. 

Libraries like `pandas` can also handle CSVs, offering more power but requiring more resources. Under the hood, csv.reader() is a generator, yielding rows one by one—smart for memory management.

## See Also
- Python's CSV reading/writing documentation: https://docs.python.org/3/library/csv.html
- `pandas` library for complex data handling: https://pandas.pydata.org/
- CSV vs. JSON vs. XML: A comparison of data formats: https://www.datacamp.com/community/tutorials/json-xml-csv
