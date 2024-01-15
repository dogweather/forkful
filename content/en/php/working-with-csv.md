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

## Why

CSV (Comma Separated Values) files are commonly used to store and exchange data in a tabular format. As PHP is a popular programming language for web development, understanding how to work with CSV can be beneficial for handling data operations in various web applications.

## How To

Working with CSV files in PHP is a simple process, with built-in functions to read, write, and manipulate data. Here is a basic example of how to read a CSV file and display its contents:

```PHP
$file = fopen('data.csv', 'r'); //opens the file
while (($row = fgetcsv($file)) !== FALSE) { //reads each row of data
  echo implode(', ', $row) . "<br>"; //displays each row with a comma separator
}
fclose($file); //closes the file
```

Sample Output:

```
John Doe, 25, john@example.com
Jane Smith, 30, jane@example.com
Bob Johnson, 40, bob@example.com
```

To write data to a CSV file, we can use the `fputcsv()` function. The following example adds a new row of data to an existing CSV file:

```PHP
$data = array('Sarah Jones', 35, 'sarah@example.com'); //data to be added
$file = fopen('data.csv', 'a'); //opens the file in append mode
fputcsv($file, $data); //writes the data as a new row
fclose($file); //closes the file
```

To manipulate data in a CSV file, we can use the `array_map()` function to apply a specified function to each element of an array. Here is an example of capitalizing the first letter of each name in a CSV file:

```PHP
$file = fopen('data.csv', 'r+'); //opens the file in read and write mode
while (($row = fgetcsv($file)) !== FALSE) { //reads each row of data
  $row = array_map('ucfirst', $row); //capitalizes the first letter of each element
  fputcsv($file, $row); //writes the updated row back to the file
}
fclose($file); //closes the file
```

Sample Output:

```
John Doe, 25, john@example.com
Jane Smith, 30, jane@example.com
Bob Johnson, 40, bob@example.com
Sarah Jones, 35, sarah@example.com
```

## Deep Dive

When working with CSV files, it is important to understand the formatting and structure. Each row in a CSV file represents a single record, with each field separated by a comma (hence the name "comma separated values"). This makes CSV files easy to read and edit with a simple text editor.

However, it is important to note that CSV files do not have a standardized format, and different programs may interpret them differently. This can lead to issues with data formatting and readability, especially if the CSV file has empty or special characters.

Fortunately, PHP has built-in functions such as `mb_detect_encoding()` and `mb_convert_encoding()` to handle different character encodings in CSV files, ensuring the data is properly displayed.

Another thing to consider when working with CSV files is handling errors. The `try-catch` block can be used to catch any errors that may occur while working with the file, such as incorrect file paths or missing data.

Lastly, it is important to regularly update and backup CSV files, as they can easily be corrupted if not properly maintained.

## See Also

For more information on working with CSV files in PHP, check out the following resources:

- [PHP manual on working with CSV files](https://www.php.net/manual/en/ref.csv.php)
- [Working with CSV files in PHP](https://www.php.net/manual/en/function.fgetcsv.php) by Code Wall
- [Reading and writing CSV files in PHP](https://www.tutorialrepublic.com/php-tutorial/php-file-handling.php) by Tutorial Republic