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

## What & Why?

CSV (Comma Separated Values) is a common file format used for storing and organizing data in a table-like structure. This makes it a popular choice for exporting and importing data between different applications. Programmers often work with CSV files to extract, manipulate, and organize data for analysis or to be used in their code.

## How to:

Coding with CSV files in the Fish Shell is made easy with the use of the built-in `csv` command. This command allows you to read and write to CSV files using simple syntax and parameters.

To read data from a CSV file, you can use the `-r` flag and specify the file name as an argument. For example:

```
Fish Shell ➊ > csv -r data.csv
```

This will output the data in a table format, with each row representing a line in the CSV file. To extract specific columns from the CSV file, you can use the `-c` flag followed by the column numbers. For example:

```
Fish Shell ➊ > csv -rc 1,3 data.csv
```

This will only output the first and third columns from the CSV file.

To write data to a CSV file, you can use the `-w` flag followed by the file name. For example:

```
Fish Shell ➊ > csv -w output.csv
```

You can then enter the data, line by line, and press `ctrl + D` to finish and save the file.

## Deep Dive

CSV files were first created in the early 1970s as a way to transfer data between databases, and have since become widely used due to their simplicity and compatibility across different systems. While CSV files can easily be manipulated with programming languages like Python and R, using the built-in `csv` command in Fish Shell provides a more streamlined approach for working with CSV files.

Some alternatives to using the `csv` command in Fish Shell include using the `awk` command or using a third-party library like `libcsv`. However, these options may require more complex syntax and may not be as efficient as using the built-in `csv` command.

When working with CSV files, it's important to note that the data is not restricted to just commas as the delimiter. The `-f` flag can be used to specify a different delimiter, such as a semicolon or a tab, depending on the structure of the data.

## See Also

- [The Art of Command Line: CSV Manipulation](https://github.com/jlevy/the-art-of-command-line/blob/master/README.md#csv-manipulation)