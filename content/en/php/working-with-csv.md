---
title:                "Working with csv"
html_title:           "PHP recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma Separated Values) in PHP refers to manipulating and processing data in a format that separates values using commas. Programmers use CSV because it is a simple, lightweight, and widely used file format for storing and exchanging tabular data.

## How to:

To start working with CSV in PHP, we first need to understand how to read and write CSV files.

To read a CSV file, we use the `fopen()` function to open the file, `fgetcsv()` to read each line of the file, and `fclose()` to close the file after we're done. Here's an example:

```PHP
$file = fopen("data.csv", "r"); // open the CSV file for reading
while (($data = fgetcsv($file)) !== FALSE) { // loop through each line of the file
    print_r($data); // print the data 
}
fclose($file); // close the file
```

The `fgetcsv()` function returns an array containing the data from each line of the CSV file. We can then use `print_r()` to print the data out in a readable format.

To write to a CSV file, we use the `fopen()` function with the "w" flag to open the file for writing, `fputcsv()` to write data to the file, and `fclose()` to close the file. Here's an example:

```PHP
$file = fopen("data.csv", "w"); // open the CSV file for writing
$data = array(1, "John", "Doe"); // data to be written to the file
fputcsv($file, $data); // write data to the file
fclose($file); // close the file
```

This will create a new CSV file named "data.csv" and write the data `1, "John", "Doe"` to the first line of the file.

## Deep Dive:

CSV files have been around since the 1970s, and they have become a popular choice for storing and exchanging data due to their simplicity and compatibility with various software and programming languages.

In PHP, there are alternative ways to work with CSV such as using the `SplFileObject` class or third-party libraries like LeagueCSV. These alternatives provide additional features and functionality for working with CSV files.

It's important to note that when writing to a CSV file, we may encounter issues with characters that need to be escaped, such as commas and line breaks. To handle this, we can use the `fputcsv()` function's `delimiter` and `escape_char` parameters to specify the characters to use for delimiting and escaping data.

## See Also:

- [PHP manual on working with CSV](https://www.php.net/manual/en/ref.filesystem.php)
- [SplFileObject class documentation](https://www.php.net/manual/en/class.splfileobject.php)
- [LeagueCSV library documentation](https://csv.thephpleague.com/)