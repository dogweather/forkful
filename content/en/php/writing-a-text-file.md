---
title:                "PHP recipe: Writing a text file"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is a common task in programming and can serve various purposes. Whether you need to store data, create logs or generate reports, being able to write a text file is an essential skill in PHP programming.

## How To

To write a text file in PHP, we will use the `fwrite()` function. This function takes two parameters: the file handle and the content to be written. Let's see an example of how to use it:

```PHP
<?php
$file_path = "sample.txt"; //create or open a file named "sample.txt"
$file = fopen($file_path, "w"); //use "w" for write mode
$content = "This is a sample text file."; //create a string to be written
fwrite($file, $content); //write the content to the file
fclose($file); //close the file
```

In this example, we first specify the path and name of the file we want to create or write to. Then, we use the `fopen()` function to open the file in write mode ("w"). Next, we create the content we want to write to the file and use the `fwrite()` function to write it. Finally, we use `fclose()` to close the file. This will save our content to the file and we can now access it as needed.

## Deep Dive

There are a few things to keep in mind when writing a text file in PHP. 

First, you can specify the file path and name when using `fopen()`, but if the file does not exist, it will be created. However, if the file already exists, it will be overwritten unless you use "a" mode for appending.

Second, when writing to a file, newline characters will not be automatically added. This means that if you want to write multiple lines of text, you will need to use `\n` to add line breaks.

Lastly, you can also use `file_put_contents()` to write to a file in one line of code, without the need to use `fopen()` and `fwrite()` separately.

## See Also

- Official PHP documentation for `fwrite()`: [PHP fwrite](https://www.php.net/manual/en/function.fwrite.php)
- More information on `file_put_contents()`: [PHP file_put_contents](https://www.php.net/manual/en/function.file-put-contents.php)
- Tutorial on reading and writing files in PHP: [PHP File Handling](https://www.w3schools.com/php/php_file_handling.asp)