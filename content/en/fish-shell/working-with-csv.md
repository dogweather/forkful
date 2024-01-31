---
title:                "Working with CSV"
date:                  2024-01-19
simple_title:         "Working with CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) involves parsing and manipulating data structured as rows and columns in text format. Programmers use CSV files because they're simple, widely supported, and easy to import or export from databases and spreadsheets.

## How to:

1. Reading a CSV file line-by-line:
```Fish Shell
for line in (cat file.csv)
    echo $line
end
```

2. Splitting fields and printing a specific column (e.g., the second column):
```Fish Shell
cat file.csv | while read -l line
    set -l fields (string split "," $line)
    echo $fields[2]
end
```

3. Writing to a CSV file:
```Fish Shell
echo "name,age,city" > users.csv
echo "Alice,30,New York" >> users.csv
echo "Bob,25,Los Angeles" >> users.csv
```

Sample output (content of `users.csv`):
```
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

## Deep Dive

CSV handling has been around since the early days of personal computing, evolving as a simple format for data interchange. Though basic, CSV's lack of a standard can lead to parsing issues, like different delimiters and text encoding. While Fish Shell doesn't have built-in CSV parsing tools, `awk`, `sed`, and `cut` are often used alongside it for more complex tasks. 

Fish’s approach to CSV is more manual and script-based, leveraging its string manipulation capabilities to handle CSV fields. For heavy-duty data processing, consider alternatives like Python's `pandas` library, or command-line tools such as `csvkit`.

## See Also

- Getting Started with `awk`: [AWK - A Tutorial and Introduction](https://www.grymoire.com/Unix/Awk.html)
- Introduction to `sed`: [Sed - An Introduction and Tutorial](https://www.grymoire.com/Unix/Sed.html)
- Official Fish Shell Documentation: [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- `csvkit` Documentation: [csvkit - A suite of utilities for converting to and working with CSV](https://csvkit.readthedocs.io/en/latest/)
