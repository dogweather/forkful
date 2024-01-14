---
title:                "PHP recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Why

As a programmer, you often encounter situations where you need to store or retrieve data from a file. This could be for storing user preferences, saving game progress, or even creating reports. In these cases, writing a text file using PHP can be extremely useful.

# How To

To begin, we must first understand the basic concept of a text file. A text file is simply a file containing ASCII characters that can be opened and read by any text editor. These files have a .txt extension and can contain text, numbers, symbols, and even special characters.

To write a text file in PHP, we use the `fopen()` function to open a file handle. The first parameter is the name of the file we want to open, and the second parameter indicates the mode in which we want to open the file. In this case, we want to write to the file, so we use the `w` mode.

```PHP
$file = fopen("example.txt", "w"); // opens a new file handle in write mode
```

Next, we use the `fwrite()` function to write data to the file. This function takes two parameters - the file handle and the data we want to write to the file. We can write a single string or use the `.` operator to concatenate multiple strings.

```PHP
fwrite($file, "Hello World!"); // writes "Hello World!" to the file
```

To ensure that the data is written to the file, we need to close the file handle using the `fclose()` function.

```PHP
fclose($file); // closes the file handle
```

Now, let's check our text file to see if our data was successfully written.

```
Hello World!
```

As you can see, our string was written to the file successfully.

# Deep Dive

There are a few things to keep in mind when writing a text file using PHP. Firstly, make sure that the file you are trying to open exists and has proper permissions for writing. If the file doesn't exist, the `fopen()` function will create a new file with the specified name.

Secondly, you can write multiple lines to a text file by using the `PHP_EOL` constant, which represents the end-of-line character on the current platform. This ensures that your lines are separated appropriately, depending on the operating system.

Lastly, you can also write data to a specific position in a file using the `fseek()` function. This function takes three parameters - the file handle, the offset (in bytes), and the position where we want to move the handle to. This is useful if you want to append data to an existing file without overwriting the existing content.

# See Also

- [PHP File Handling Tutorial](https://www.w3schools.com/php/php_file.asp)
- [PHP fwrite() Function](https://www.php.net/manual/en/function.fwrite.php)
- [PHP Constants](https://www.php.net/manual/en/language.constants.php)