---
title:    "PHP recipe: Reading a text file"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract specific information from a large text file, but didn't want to manually search through it line by line? Well, have no fear, because with a little bit of PHP programming knowledge, you can easily read through a text file and extract the information you need!

## How To

To read a text file in PHP, we will use the `fopen()` function to open the file, `fgets()` function to read each line, and `fclose()` function to close the file. Let's take a look at an example below:

```PHP
$file = fopen("example.txt", "r"); // open the file in read mode

while(!feof($file)) { // while not at the end of the file
    $line = fgets($file); // read each line
    echo $line; // print out the line
}

fclose($file); // close the file
```

In the code above, we first use the `fopen()` function to open the file "example.txt" in read mode, which means we are only allowed to read the file and not write to it. Then, we use a while loop to continue reading each line until we reach the end of the file using the `feof()` function. Inside the loop, we use the `fgets()` function to read each line and store it in the variable `$line`. Finally, we print out each line using `echo` and close the file using the `fclose()` function.

Let's say our "example.txt" file contains the following information:

```
John Smith
Jane Doe
Bob Johnson
```

The output of the above code would be:

```
John Smith
Jane Doe
Bob Johnson
```

## Deep Dive

There are a few things to keep in mind when reading a text file in PHP. First, we can specify the number of characters we want to read using the second parameter of the `fgets()` function. For example, if we only want to read the first 10 characters of each line, we can use `fgets($file, 10)`.

Next, we can use the `feof()` function to check if we have reached the end of the file, and the `feof()` function to move the file pointer to a specific position within the file.

Other useful functions for reading a text file in PHP include `fgetc()` to read a single character, `file_get_contents()` to read the entire file into a string, and `file()` to read the file into an array with each line as an element.

## See Also

For more information on working with files in PHP, check out these resources:

- [PHP File Handling Basics](https://www.w3schools.com/php/php_ref_filesystem.asp)
- [PHP Filesystem Functions](https://www.php.net/manual/en/ref.filesystem.php)
- [PHP: fopen Manual](https://www.php.net/manual/en/function.fopen.php)