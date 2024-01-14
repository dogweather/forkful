---
title:                "PHP recipe: Creating a temporary file"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating a temporary file may seem like a trivial task in PHP programming, but it can actually serve a very useful purpose. Temporary files are often used for storing data during the execution of a program, and they can be automatically deleted once they are no longer needed. This can help with performance and avoid cluttering up the server with unnecessary files.

## How To

To create a temporary file in PHP, we can use the `tempnam()` function. This function takes two parameters - the directory where the temporary file should be created, and a prefix for the file name.

```PHP
$tempFile = tempnam('/tmp', 'temp_');
echo $tempFile;
```

The output of the above code would be a string containing the path to the newly created temporary file, for example: `/tmp/temp_yjs8`. This file will be automatically deleted once the script finishes execution.

We can also use the `tmpfile()` function to create a temporary file, which automatically opens the file for writing. This is useful for scenarios where we need to write data to the file before using it.

```PHP
$tempFile = tmpfile();
fwrite($tempFile, 'Hello World');
```

This code will create a temporary file and write the string "Hello World" to it. Again, the file will be automatically deleted after the script finishes running.

## Deep Dive

When creating a temporary file in PHP, we have the ability to set the permissions for the file. By default, the file will have the permissions of 0600, meaning it is readable and writable only by the owner. However, we can use the `umask()` function to change the file permissions before creating it.

```PHP
umask(0000); // this will give full permissions to the file
$tempFile = tempnam('/tmp', 'temp_');
chmod($tempFile, 0644); // change the permissions to 0644 (readable by everyone)
```

Additionally, we can use the `tempnam()` and `tmpfile()` functions in combination with other file handling functions, such as `fopen()` and `fclose()`, to perform operations on the temporary file.

## See Also

- [PHP tempnam() documentation](https://www.php.net/manual/en/function.tempnam.php)
- [PHP tmpfile() documentation](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP chmod() documentation](https://www.php.net/manual/en/function.chmod.php)