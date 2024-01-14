---
title:                "Fish Shell recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

If you've ever had to work with data, chances are you've encountered a CSV file. CSV (Comma Separated Values) is a popular file format for storing tabular data, and it's commonly used in business, finance, and research. While it may seem intimidating at first, working with CSV files can actually be quite simple and efficient with the help of Fish Shell programming.

## How To

To get started, make sure you have Fish Shell installed on your system. You can install it using the command `curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher`. Once you have Fish Shell installed, follow the steps below to start working with CSV files.

1. Navigate to the directory where your CSV file is located using the `cd` command.
2. To view the content of your CSV file, use the command `cat <file_name>.csv`.
3. To filter specific columns in the CSV file, use the `cut` command along with the `-c` option. For example, `cut -c 1,3 <file_name>.csv` will display the first and third column of the CSV file.
4. You can also use the `grep` command to search for specific data in the CSV file. For example, `grep "John" <file_name>.csv` will display all rows containing the name "John".
5. To extract data from the CSV file and save it in a new file, use the `awk` command. For example, `awk -F "," '{print $1}' <file_name>.csv > new_file.csv` will extract the first column of the CSV file and save it in a new file.

Here's a sample output for the command `awk -F "," '{print $1}' <file_name>.csv`:

```Fish Shell
1
2
3
4
```

## Deep Dive

While the above commands cover the basic operations of working with CSV files in Fish Shell, there are many more advanced features and options available. These include sorting data using the `sort` command, joining multiple CSV files using the `join` command, and manipulating data using the `sed` command. Additionally, Fish Shell has a built-in CSV parser that makes it easier to work with CSV files without having to use external commands.

## See Also

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [awk: A Language for Pattern Processing](https://www.gnu.org/software/gawk/manual/html_node/index.html)
- [grep: How to Search for Text in Files](https://www.lifewire.com/how-to-use-grep-to-search-for-text-in-files-3572073)