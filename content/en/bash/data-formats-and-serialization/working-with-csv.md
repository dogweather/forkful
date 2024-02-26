---
date: 2024-02-03 19:02:59.597450-07:00
description: "Working with CSV (Comma-Separated Values) files in Bash is about processing\
  \ and manipulating tabular data stored in plain text format. This is essential\u2026"
lastmod: '2024-02-25T18:49:56.703108-07:00'
model: gpt-4-0125-preview
summary: "Working with CSV (Comma-Separated Values) files in Bash is about processing\
  \ and manipulating tabular data stored in plain text format. This is essential\u2026"
title: Working with CSV
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) files in Bash is about processing and manipulating tabular data stored in plain text format. This is essential for programmers as it allows the automation of data transformation, analysis, and integration tasks directly from the command line, without the need for more heavyweight tools or programming environments.

## How to:

**Reading a CSV File Line by Line**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "Column 1: $column1, Column 2: $column2, Column 3: $column3"
done < sample.csv
```

*Sample output:*

```
Column 1: id, Column 2: name, Column 3: email
...
```

**Filtering CSV Rows Based on a Condition**

Using `awk`, you can easily filter rows. For example, to find rows where the second column equals "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**Modifying a Column Value**

To change the second column to uppercase:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**Sorting a CSV File Based on a Column**

You can sort a CSV file based on, let's say, the third column (numerically):

```bash
sort -t, -k3,3n sample.csv
```

**Using `csvkit` for More Complex Tasks**

`csvkit` is a suite of command-line tools for converting to and working with CSV. It can be installed via pip.

To convert a JSON file to CSV:

```bash
in2csv data.json > data.csv
```

To query a CSV file using SQL:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*Note: Installing `csvkit` requires Python and can be done using `pip install csvkit`.*
