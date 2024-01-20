---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Unlocking temporary files in PHP is like setting up a 'disposable' area in your computer's memory, where data is only stored for a limited period. Programmers create temporary files for tasks requiring additional, short-term memory; this can help in situations such as bulk data manipulation or file processing where immediate impacts could be costly if mistakes occur. 

## How to: 

Let's dive into the PHP code to create a temporary file:

```PHP
$tempFile = tmpfile();

$writtenData = fwrite($tempFile, "Just a Sample Data!\nHello World!");

rewind($tempFile);

$readData = fread($tempFile, 1024);

echo $readData; 

fclose($tempFile);
```
When you run this code, it first creates a temporary file, writes some data into it, rolls back the file pointer to the start of the file, reads the data from the file, prints the data, and finally, closes the file. The output will be:

``` 
Just a Sample Data!
Hello World!
```
Please note the temporary file is automatically removed once it's closed or the script ends.

## Deep Dive 

Believe it or not, temporary files have been a part of PHP since version 4.0.2 back in August 2000. It was introduced as a solution for storing data temporarily when memory is a constraint. 

Alternatively, nowadays some developers prefer memory-based filesystems like `/dev/shm` on Linux, but the traditional `tmpfile()` function is still widely used. 

One fun fact: when you create a temp file using `tmpfile()` in PHP, under the hood it uses the system's default location for temp files. On Unix-based systems, this is often `/tmp`. 

Remember that `tmpfile()` creates a binary-safe file. This means it can also hold binary data, and it's the programmer's responsibility to handle data correctly.  

## See Also 

- PHP Official Documentation on [`tmpfile`](https://www.php.net/manual/en/function.tmpfile.php)
- An insightful blog post on [Working with Files in PHP](http://www.php.net/manual/en/ref.filesystem.php)