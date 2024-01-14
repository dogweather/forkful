---
title:                "PHP recipe: Reading a text file"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Reading a text file may seem like a simple task, but it is actually a crucial skill for any PHP programmer. Whether you're looking to parse data, analyze logs, or simply read user input from a form, knowing how to read a text file is an essential skill for any programmer.

## How To
To read a text file in PHP, we use the `fopen()` function to open the file and then use the `fgets()` or `fread()` function to read the contents of the file. Let's take a look at an example:

```PHP
<?php

// Open the file in read mode
$file = fopen("textfile.txt", "r");

// Read a single line from the file
$line = fgets($file);

// Output the line
echo $line;

// Close the file
fclose($file);
```
Output:
```
This is the first line of the text file.
```

In this example, we opened a file called "textfile.txt" in read mode, read the first line using `fgets()`, and then closed the file. However, sometimes we may want to read the entire file instead of just one line. In that case, we can use the `fread()` function:

```PHP
<?php

// Open the file in read mode
$file = fopen("textfile.txt", "r");

// Read the entire file
$contents = fread($file, filesize("textfile.txt"));

// Output the contents
echo $contents;

// Close the file
fclose($file);
```
Output:
```
This is the first line of the text file.
This is the second line of the text file.
This is the third line of the text file.
```

## Deep Dive
Now that we have seen some basic examples of how to read a text file, let's dive deeper into the topic. PHP offers several functions for reading files, each with its own use case. Here are a few more functions you may find useful:

- `fgets()` - Reads a single line from the file
- `fread()` - Reads a specified number of bytes from the file
- `file()` - Reads the entire file into an array, with each element representing a line
- `file_get_contents()` - Reads the entire file into a string
- `fgetcsv()` - Reads a line and parses it as CSV data

It's important to note that when reading a file, PHP will continue reading from where it last left off. This means that if you call the `fgets()` function again, you will get the next line in the file. To reset the pointer back to the beginning of the file, you can use the `rewind()` function.

## See Also
- [PHP Manual - File Handling](https://www.php.net/manual/en/book.filesystem.php)
- [Open a File for Reading in PHP](https://www.php.net/manual/en/function.fopen.php)
- [Read a File Line by Line in PHP](https://www.php.net/manual/en/function.fgets.php)