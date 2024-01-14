---
title:    "PHP recipe: Reading a text file"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

"Reading a text file is a fundamental skill for any PHP programmer. It allows you to extract data from a file and use it for various purposes in your code. Whether you are working with user input, configuration files, or external data sources, knowing how to read a text file is an essential part of your programming arsenal."

## How To

To read a text file in PHP, we will use the `fopen()` and `fgets()` functions. The `fopen()` function opens a file and returns a file handle, while the `fgets()` function reads a line from the file.

```
<?php
    //open the file in read-only mode
    $handle = fopen("data.txt", "r") or die("Unable to open file!");

    //read the file line by line
    while(!feof($handle)) {
        $line = fgets($handle);
        echo $line . "<br>";
    }

    //close the file handle
    fclose($handle);
?>
```

This code will open the file "data.txt" and read its contents line by line until it reaches the end of the file. It will then print each line with a `<br>` tag to create line breaks.

Let's say the "data.txt" contains the following lines:

```
John Smith
35
New York
```

The output of our code will be:

```
John Smith
35
New York
```

## Deep Dive

There are a few things to consider when reading a text file in PHP. First, the file path must be a valid and accessible location on your server. Otherwise, the `fopen()` function will return an error.

Secondly, you can specify the mode in which the file should be opened. In our example, we used "r" for read-only mode, but there are other modes such as "w" for write and "a" for append. You can read more about the different modes and their uses in the PHP documentation.

Lastly, the `fgets()` function will read until the end of line character, which is typically the newline character "\n". This means that the line break will also be included in the output. To remove the line break, you can use the `trim()` function.

## See Also

- [PHP fopen() function documentation](https://www.php.net/manual/en/function.fopen.php)
- [PHP fgets() function documentation](https://www.php.net/manual/en/function.fgets.php)
- [PHP trim() function documentation](https://www.php.net/manual/en/function.trim.php)