---
title:                "Reading a text file"
html_title:           "PHP recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Are you looking to learn how to read a text file using PHP? Perhaps you want to manipulate the data within the file or extract specific information from it. Well, you've come to the right place! In this article, we will cover the basics of reading a text file using PHP and provide some helpful coding examples.

## How To

To begin, we need to use the `fopen()` function in PHP to open the text file. This function takes two parameters: the file name and the mode. The mode is used to specify how the file should be opened, such as "r" for reading or "w" for writing. For our purposes, we will use "r" for reading.

```
<?php
$file = fopen("textfile.txt", "r");
```

Next, we need to use the `fgets()` function to read the content of the file line by line. We can use a loop to continue reading until we reach the end of the file.

```
<?php
while(!feof($file)){
  $line = fgets($file);
  //do something with $line
}
```

We can also use the `fread()` function to read a specific number of bytes from the file, instead of reading line by line. This is useful for files that have a fixed format or for reading binary files.

Once we are finished reading the file, we need to close it using the `fclose()` function.

```
<?php
fclose($file);
```

Now, let's see an example of reading a text file and displaying its content on the webpage:

```
<?php
$file = fopen("textfile.txt", "r");
while(!feof($file)){
  $line = fgets($file);
  echo $line . "<br>";
}
fclose($file);
```

Output:
```
This is the first line of the text file.
This is the second line.
This is the third and final line.
```

## Deep Dive

When using the `fopen()` function, there are different modes you can use to open a file. Here are some of the most commonly used modes:

- "r" - opens the file for reading.
- "w" - opens the file for writing. If the file does not exist, it creates a new one. If the file already exists, it will truncate its content.
- "a" - opens the file for appending. If the file does not exist, it creates a new one. If the file already exists, it will add the new content to the end.
- "r+" - opens the file for reading and writing. The file must already exist.
- "w+" - opens the file for reading and writing. If the file does not exist, it creates a new one. If the file already exists, it will truncate its content.

You can also use the `file_get_contents()` and `file_put_contents()` functions to read and write to a file respectively. These functions handle the opening and closing of the file for you, making it a simpler solution for basic file operations.

## See Also

- [PHP documentation on file handling](https://www.php.net/manual/en/book.filesystem.php)
- [PHP fopen() function](https://www.php.net/manual/en/function.fopen.php)
- [PHP fgets() function](https://www.php.net/manual/en/function.fgets.php)
- [PHP fclose() function](https://www.php.net/manual/en/function.fclose.php)
- [PHP file_get_contents() function](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP file_put_contents() function](https://www.php.net/manual/en/function.file-put-contents.php)