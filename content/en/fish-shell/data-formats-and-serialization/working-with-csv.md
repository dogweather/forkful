---
date: 2024-02-03 19:03:13.532792-07:00
description: "How to: Fish Shell, by itself, doesn't have built-in functions specifically\
  \ designed for CSV manipulation. However, you can leverage Unix utilities like\u2026"
lastmod: '2024-03-13T22:45:00.494073-06:00'
model: gpt-4-0125-preview
summary: Fish Shell, by itself, doesn't have built-in functions specifically designed
  for CSV manipulation.
title: Working with CSV
weight: 37
---

## How to:
Fish Shell, by itself, doesn't have built-in functions specifically designed for CSV manipulation. However, you can leverage Unix utilities like `awk`, `sed`, and `cut` for basic operations or use specialized tools like `csvkit` for more advanced tasks.

### Reading a CSV file and printing the first column:
Using `cut` to extract the first column:
```fish
cut -d ',' -f1 data.csv
```
Sample output:
```
Name
Alice
Bob
```

### Filtering CSV rows based on column value:
Using `awk` to find rows where the second column matches "42":
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
Sample output:
```
Bob,42,London
```

### Modifying a CSV file (e.g., adding a column):
Using `awk` to add a column with a static value "NewColumn":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
Sample output in `modified.csv`:
```
Name,Age,City,NewColumn
Alice,30,New York,NewColumn
Bob,42,London,NewColumn
```

### Using `csvkit` for more advanced operations:
First, ensure you have `csvkit` installed. If not, install it using pip: `pip install csvkit`.

**Converting a CSV file to JSON:**
```fish
csvjson data.csv > data.json
```
Sample `data.json` output:
```json
[{"Name":"Alice","Age":"30","City":"New York"},{"Name":"Bob","Age":"42","City":"London"}]
```

**Filtering with `csvkit`'s `csvgrep`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
This command replicates the filtering task but using `csvkit`, targeting column 2 for the value "42".

In conclusion, while Fish Shell itself might not offer direct CSV manipulation capabilities, its seamless integration with Unix utilities and the availability of tools like `csvkit` provide powerful options for working with CSV files.
