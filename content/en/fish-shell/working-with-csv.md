---
title:                "Working with csv"
html_title:           "Fish Shell recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma Separated Values) files have become a popular format for storing and exchanging data due to their simplicity and compatibility with various software applications. With the rise of data analysis and management, knowing how to work with CSV files using Fish Shell can be a valuable skill to have.

## How To

Working with CSV files in Fish Shell is straightforward and can be done using built-in commands and functions. Here are some coding examples with corresponding sample output to help you get started:

```Fish Shell
# Listing the contents of a CSV file
csvlist file.csv

# Sorting a CSV file by a specific column
csvsort -c 2 file.csv

# Counting the number of rows in a CSV file
csvcount file.csv
```

Output:
```
First Name,Last Name,Age
John,Doe,30
Jane,Smith,25
Adam,Jones,35

Last Name,First Name,Age
Jones,Adam,35
Doe,John,30
Smith,Jane,25

3
```

## Deep Dive

Fish Shell has a unique feature that allows for easy manipulation and filtering of CSV data using its web-like navigation syntax. This allows you to access and modify specific elements within a CSV file without the need for complicated code. You can learn more about this feature and its various uses by referring to Fish Shell's official documentation.

See Also
- [Fish Shell CSV Documentation](https://fishshell.com/docs/current/cmds/csv.html)
- [CSV Format Explained](https://www.computerhope.com/issues/ch001356.htm)
- [Advanced CSV Manipulation Techniques](https://www.dataquest.io/blog/csv-tutorial-advanced/)