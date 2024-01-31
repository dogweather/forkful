---
title:                "Working with CSV"
date:                  2024-01-19
simple_title:         "Working with CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV, which stands for "Comma-Separated Values", involves parsing and manipulating data in a tabular text format. Programmers do this because CSV is a common, straightforward file format used for exchanging data between different applications and systems.

## How to:

### Read from a CSV file:

```Bash
while IFS=, read -r col1 col2 col3
do
  echo "Column 1: $col1 | Column 2: $col2 | Column 3: $col3"
done < myfile.csv
```

Sample output:

```
Column 1: data1 | Column 2: data2 | Column 3: data3
```

### Write to a CSV file:

```Bash
echo "data1,data2,data3" > myfile.csv
```

### Append to a CSV file:

```Bash
echo "data4,data5,data6" >> myfile.csv
```

## Deep Dive

CSV format has roots in early computing and has become a mainstay in data interchange because it's supported by a wide range of software. While Bash can handle CSV files, it isn’t equipped for complex parsing. Alternatives for more intricate tasks include AWK, Sed, or using a full programming language like Python. Implementation details to consider when working with CSV in Bash include handling special characters, complex quoting, and line breaks within fields.

## See Also

- [GNU Coreutils Documentation](https://www.gnu.org/software/coreutils/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
- [Introduction to AWK](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Sed by Example](https://www.gnu.org/software/sed/manual/sed.html)

For more advanced CSV manipulation:
- [Python CSV Module Doc](https://docs.python.org/3/library/csv.html)
- [Pandas Library for Python](https://pandas.pydata.org/)
