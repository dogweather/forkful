---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file in PHP involves fetching and interpreting data from a .txt file. It's a common operation done to process, analyze or manipulate stored data, helping to persist and share data between sessions and applications.

## How To:

Here's how you read a contents of a text file using PHP:

```PHP
<?php
$filename = 'myfile.txt';
$file = fopen($filename, 'r') or die("Unable to open file!");
echo fread($file, filesize($filename));
fclose($file);
?>
```
This will print the contents of 'myfile.txt' on your HTML document. If the file doesn't exist, it prints an error message.

## Deep Dive

Back in the day, PHP didn't have great inbuilt functions for reading text files, and programmers had to rely on less efficient, more error-prone methods. Now, inbuilt functions like `fopen()`, `fread()`, and `fclose()` provide straightforward and efficient ways to do this.

Alternative ways include using `file_get_contents()` function that reads an entire file into a string. Here's an example:

```PHP
<?php
$filename = 'myfile.txt';
echo file_get_contents($filename);
?>
```
This code does the same as the first example but in a simpler way, which can be handy for smaller files. However, for larger files, `fread()` is more memory-efficient as `file_get_contents()` loads the entire file into memory.

When using `fopen()`, you typically set the mode to 'r', which signifies 'read'. There are several modes, including 'w' for write and 'a' for append. Each of these modes allows different operations on the file.

## See Also

For more detailed descriptions and other examples on file handling with PHP, see the official PHP Manual:

- [fopen()](https://www.php.net/manual/en/function.fopen.php)
- [fread()](https://www.php.net/manual/en/function.fread.php)
- [fclose()](https://www.php.net/manual/en/function.fclose.php)
- [file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP Filesystem Functions](https://www.php.net/manual/en/ref.filesystem.php)