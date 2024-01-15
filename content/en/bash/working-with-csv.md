---
title:                "Working with csv"
html_title:           "Bash recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Why
CSV (Comma Separated Values) files are a common way to store tabular data, making them widely used in data analysis, database management, and other fields. As Bash is a powerful scripting language that can manipulate files and perform data processing, knowing how to work with CSV using Bash can be a useful skill for various tasks.

## How To
To work with CSV files in Bash, we can use various built-in commands and tools. Let's explore some examples and see the output in action.

#### Reading CSV Files
To read a CSV file in Bash, we can use the `read` command and specify the delimiter (in this case, a comma) using the `-d` flag. For example, if we have a CSV file named "data.csv" with the following content:
```
John,Smith,25
Jane,Doe,30
Mark,Johnson,45
```
We can read and display each line of the file using the following code:
```Bash
while IFS=',' read -r firstName lastName age; do
    echo "Name: $firstName $lastName, Age: $age"
done < data.csv
```
The `read` command reads each line of the file and assigns the values to the specified variables. The `echo` command then prints the values in the desired format. The `while` loop continues until all lines in the file are read.

The output would be:
```
Name: John Smith, Age: 25
Name: Jane Doe, Age: 30
Name: Mark Johnson, Age: 45
```

#### Writing CSV Files
To create a new CSV file from scratch, we can use the `printf` command to specify the desired format and `>>` to append the data to the file. For example, let's create a new file named "new_data.csv" with the following content:
```
Name,Email
John, john@email
Jane, jane@email
```
We can use the following code:
```Bash
printf "%s,%s\n" "Mark" "mark@email" >> new_data.csv
```
The `%s` specifies the values to be inserted, and the `\n` adds a new line after each entry. The `>>` appends the data to the end of the file. The resulting file would look like this:
```
Name,Email
John, john@email
Jane, jane@email
Mark,mark@email
```

## Deep Dive
While Bash has built-in commands to work with CSV files, there are also external tools like `csvtool` that offer more advanced features. For example, we can use `csvtool` to convert a CSV file to a JSON file or vice versa. We can also use `awk` to filter or manipulate data in a CSV file.

It's essential to note that Bash has limitations when handling large or complex CSV files. In such cases, using a specialized programming language or tool like Python or R would be more efficient.

See Also
- [BashGuide on CSV](https://mywiki.wooledge.org/BashGuide/CSV)
- [Official Bash documentation](https://www.gnu.org/software/bash/manual/html_node/Bash-and-Files.html)
- [CSV kit tool for working with CSV files](https://csvkit.readthedocs.io/en/latest/)