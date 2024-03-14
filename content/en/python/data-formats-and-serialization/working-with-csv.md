---
date: 2024-02-03 19:03:15.173777-07:00
description: "Working with CSV (Comma-Separated Values) involves reading from and\
  \ writing data to CSV files, a common format for storing tabular data. Programmers\
  \ do it\u2026"
lastmod: '2024-03-13T22:44:59.727693-06:00'
model: gpt-4-0125-preview
summary: "Working with CSV (Comma-Separated Values) involves reading from and writing\
  \ data to CSV files, a common format for storing tabular data. Programmers do it\u2026"
title: Working with CSV
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) involves reading from and writing data to CSV files, a common format for storing tabular data. Programmers do it to easily exchange and store data in a simple, text-based format that is widely supported across different platforms and languages.

## How to:
Python provides the built-in `csv` module to handle CSV files, making it straightforward to read from and write to them. For more robust and complex data manipulation, the third-party library `pandas` is highly popular.

### Using the `csv` module

#### Reading a CSV file
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*Assuming `sample.csv` contains:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*Output:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### Writing to a CSV file
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*Creates or overwrites `output.csv` with:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### Using `pandas` for CSV
`pandas` is a powerful library for data manipulation that simplifies working with CSV files among other data formats.

#### Install pandas
```shell
pip install pandas
```

#### Reading a CSV file with pandas
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*Output:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### Writing to a CSV file with pandas
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*Creates or overwrites `output_pandas.csv` with:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
