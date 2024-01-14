---
title:                "PHP recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why Read a Text File in PHP?

Text files are a fundamental part of computing and programming. They contain plain text data that can be easily read and manipulated by both humans and machines. In PHP, there are multiple reasons why you may need to read a text file, such as reading configuration files, data persistence, or processing user input. In this blog post, we will explore the basics of reading a text file in PHP and why it can be useful in your programming journey.

## How To Read a Text File in PHP

Reading a text file in PHP can be done using the `fopen()` and `fread()` functions. The first step is to open the file using `fopen()`, which takes in two parameters: the file name and the mode (read, write, append, etc.). Once the file is opened, the `fread()` function can be used to read the contents of the file. It takes two parameters: the file handle from `fopen()` and the length of data to be read. Here is a simple example of reading a text file in PHP:

```PHP
$file = fopen("sample.txt", "r"); // Open file in read mode
$data = fread($file, filesize("sample.txt")); // Read the entire file
echo $data; // Output contents of the file
fclose($file); // Close the file
```

The output of the above code will be the contents of "sample.txt" file. Additionally, you can use `feof()` function to check if the end of the file has been reached.

## Deep Dive into Reading a Text File

There are a few things to keep in mind when reading a text file in PHP. First, it's important to use the correct mode when opening the file. If you want to read the file, make sure to use "r" mode. Also, keep in mind that `fread()` function reads the file sequentially, so the file pointer will move forward after each read. To read a specific part of the file, you can use `fseek()` function to set the file pointer to a specific location. Lastly, be mindful of memory usage when reading large files. You can use `fgets()` function to read the file line by line instead of reading the entire file at once.

## See Also

- [PHP Manual: fopen()](https://www.php.net/manual/en/function.fopen.php)
- [PHP Manual: fread()](https://www.php.net/manual/en/function.fread.php)
- [PHP Manual: feof()](https://www.php.net/manual/en/function.feof.php)
- [PHP Manual: fseek()](https://www.php.net/manual/en/function.fseek.php)
- [PHP Manual: fgets()](https://www.php.net/manual/en/function.fgets.php)

Reading a text file in PHP may seem like a simple task, but it is an essential skill for any PHP programmer. Whether you are handling user input or processing data, having the ability to read and manipulate text files will come in handy. So, next time you need to read a text file in your PHP program, remember these tips and tricks. Happy coding!