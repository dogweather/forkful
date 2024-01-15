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

## Why

Before the internet became widely accessible, writing text files was the go-to method for storing and sharing information. Today, you may wonder why you would even need to write a text file when there are so many other advanced options available. However, there are still many situations where a simple, lightweight, and easy-to-read text file can come in handy.

## How To

To write a text file in PHP, you can use the `file_put_contents()` function. Here's an example:

```
<?php
// Open or create a file named "my_file.txt" in the same directory as this PHP file
$file = 'my_file.txt';

// Write some text content to the file
$content = "This is a sample text file written with PHP.";

// Write the content to the file and return the number of bytes written
$result = file_put_contents($file, $content);

// Check if the operation was successful
if ($result !== false) {
    echo "Text file successfully written.";
} else {
    echo "Unable to write the text file.";
}
```

Running this code will create a new text file named "my_file.txt" in the same directory as the PHP file. If the file already existed, it will overwrite the existing content. You can also specify a different path for the file if needed.

## Deep Dive

The `file_put_contents()` function takes three parameters: the file name, the content to be written, and an optional `flags` parameter. The default value for `flags` is `0` which means the file will be opened in write-only mode and the existing content will be overwritten. However, you can use the `FILE_APPEND` flag to append new content to the end of the file instead of overwriting it.

You can also use the `fopen()` and `fwrite()` functions for more flexibility in writing text files. `fopen()` opens a file and returns a file pointer, while `fwrite()` writes content to the open file using the file pointer. This allows you to write to a specific position in the file or even create placeholders for future content.

## See Also

Here are some useful resources for further reading on writing text files in PHP:

- [PHP file_put_contents() function](https://www.php.net/manual/en/function.file-put-contents.php)
- [PHP fopen() function](https://www.php.net/manual/en/function.fopen.php)
- [PHP fwrite() function](https://www.php.net/manual/en/function.fwrite.php)