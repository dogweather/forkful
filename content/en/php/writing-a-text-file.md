---
title:    "PHP recipe: Writing a text file"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is a crucial skill for any PHP programmer. It allows you to save and store data, making it accessible for future use in your coding projects. Whether you need to store user information, log data, or simply create a text-based report, being able to write a text file is an essential tool in your coding arsenal.

## How To

To write a text file in PHP, we will use the `fopen()` and `fwrite()` functions. The `fopen()` function opens the file, while the `fwrite()` function writes the data to the file. Let's take a look at an example:

```
<?php
$file = fopen("data.txt", "w") or die("Unable to open file!");
$txt = "This is some sample data to be written to the file.";
fwrite($file, $txt);
fclose($file);
?>
```

In this example, we first use the `fopen()` function to open a file named "data.txt" in write mode. The "w" parameter specifies that we want to write to the file. Next, we use the `fwrite()` function to write the data stored in the `$txt` variable to the file. Finally, we close the file using the `fclose()` function.

If we now open the "data.txt" file, we will see the text "This is some sample data to be written to the file." written inside.

## Deep Dive

When writing a text file, there are a few important things to keep in mind. Firstly, pay attention to the file permissions. Make sure that the file is writable by the PHP script, otherwise, the `fwrite()` function will not work and you will receive an error.

It is also important to properly format the data that you are writing to the file. Using functions like `PHP_EOL` can help ensure that your data is written on separate lines, making it easier to read and manipulate in the future.

Additionally, you can use the `file_put_contents()` function as an alternative to `fopen()` and `fwrite()`. This function will automatically open, write, and close the file for you, making it a more convenient option for writing simple text files.

## See Also

For more information on writing text files in PHP, check out the documentation on [PHP File Handling](https://www.php.net/manual/en/book.filesystem.php) and [PHP File I/O](https://www.php.net/manual/en/ref.filesystem.php). You can also explore other useful PHP functions such as `chmod()` for changing file permissions and `filesize()` for determining the size of a file.