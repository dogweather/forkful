---
title:                "Writing a text file"
html_title:           "PHP recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in PHP is a way for programmers to save data in a human-readable format. This allows the data to be easily accessed and modified, making it a convenient storage option for various types of information.

## How to:

To write a text file in PHP, we will use the `file_put_contents()` function. This function takes two parameters: the name of the file to be written, and the content to be written.

Example:

```
<?php
$filename = "example.txt";
$content = "This is an example text file written with PHP.";

file_put_contents($filename, $content);
```

This will create a file called "example.txt" in the same directory as the PHP file and write the provided content to it.

To append to an existing text file, we can use the `FILE_APPEND` flag as the third parameter of the `file_put_contents()` function. This will add the new content to the end of the existing file instead of overwriting it.

Example:

```
<?php
$filename = "example.txt";
$content = "\nThis is additional content added to the file.";

file_put_contents($filename, $content, FILE_APPEND);
```

This will add the new content on a new line after the existing content in the file.

## Deep Dive

In the early days of web development, PHP was primarily used for generating dynamic web pages. However, as the language evolved, programmers realized its potential as a scripting language for tasks beyond web development. Writing text files is one such task that PHP can easily handle.

An alternative to the `file_put_contents()` function is the `fopen()` and `fwrite()` functions. These functions require a few more lines of code, but they provide more options for handling files, such as setting permissions and reading existing files.

The `file_put_contents()` function uses the underlying `file_get_contents()` function to write the content to a file. This means that it can handle URLs as the filename parameter, making it possible to write data to a remote server.

## See Also

- [PHP file_put_contents() function](https://www.php.net/manual/en/function.file-put-contents.php)
- [PHP fopen() function](https://www.php.net/manual/en/function.fopen.php)
- [PHP fwrite() function](https://www.php.net/manual/en/function.fwrite.php)