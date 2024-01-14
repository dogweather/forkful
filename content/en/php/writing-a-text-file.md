---
title:    "PHP recipe: Writing a text file"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

Writing a text file may seem like a simple and mundane task, but it can actually be a crucial aspect of programming. Text files are a great way to store and manipulate data, allowing developers to read, write, and modify information on a system.

## How To

To write a text file in PHP, we can use the `fopen()` function to create a file pointer and specify the file name and "w" mode for writing. Then, we can use the `fwrite()` function to write the desired content into the file, followed by `fclose()` to close the file pointer.

```PHP
<?php
$file = fopen("my_file.txt", "w");
fwrite($file, "Hello world!");
fclose($file);
```

Running this code will create a new text file named `my_file.txt` and add the text "Hello world!" to it. We can also use the `file_put_contents()` function to write data to a file in a single line:

```PHP
<?php
file_put_contents("my_file.txt", "Hello world!");
```

Additionally, we can use the `file_get_contents()` function to read the contents of a text file:

```PHP
<?php
$content = file_get_contents("my_file.txt");
echo $content; // outputs "Hello world!"
```

## Deep Dive

Text files can be written in different modes depending on the desired outcome. The "w" mode, as mentioned above, overwrites the existing file if it already exists. If we want to append new content to the end of an existing file, we can use the "a" mode. Here's an example:

```PHP
<?php
$file = fopen("my_file.txt", "a");
fwrite($file, "This is a new line.");
fclose($file);
```

This will add the text "This is a new line." to the end of the file without affecting the existing content. Other useful modes include "r" for reading, "r+" for reading and writing, and "x" for creating a new file and writing to it.

We can also use the `fputs()` function instead of `fwrite()` for writing to a file. It works the same way, but its arguments are reversed, making it more similar to other programming languages.

## See Also

- [PHP File System Functions](https://www.php.net/manual/en/ref.filesystem.php)
- [PHP fopen() Function](https://www.php.net/manual/en/function.fopen.php)
- [PHP fwrite() Function](https://www.php.net/manual/en/function.fwrite.php)