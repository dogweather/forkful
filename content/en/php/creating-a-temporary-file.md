---
title:    "PHP recipe: Creating a temporary file"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

Creating temporary files may seem like a trivial task at first, but it is actually a very useful tool in PHP programming. Temporary files are typically used for storing data that is only needed temporarily, such as during the execution of a script. They can also be used for debugging purposes, allowing developers to inspect the contents of a file at a specific point in their code.

## How To

To create a temporary file in PHP, we can use the `tmpfile()` function. This function returns a unique file handle that we can use to read and write to the temporary file. Let's take a look at a simple example:

```PHP
<?php
// Create a temporary file
$tmpFile = tmpfile();

// Write data to the file
fwrite($tmpFile, "This is a sample temporary file.");

// Read data from the file
rewind($tmpFile);
echo fread($tmpFile, filesize($tmpFile));

// Close the file
fclose($tmpFile);
?>
```

In the above code, we first use the `tmpfile()` function to create a temporary file and assign its file handle to the `$tmpFile` variable. Then, we use the `fwrite()` function to write some sample data to the file. Next, we use the `rewind()` function to rewind the file pointer to the beginning of the file and then read the contents of the file using `fread()`. Finally, we close the file using the `fclose()` function.

Running this code will output the following:

```
This is a sample temporary file.
```

As you can see, creating a temporary file in PHP is a simple process. However, there are some things to keep in mind when creating and using temporary files in your code.

## Deep Dive

When creating a temporary file, it is important to note that the file will only exist as long as the script is running. Once the execution of the script is completed, the temporary file will be automatically deleted. This is why it is important to close the file using the `fclose()` function after you are done using it.

It is also worth mentioning that the `tmpfile()` function will create a temporary file with `rw+` permissions, meaning it can be both read from and written to. This can be changed by using the `fchmod()` function and specifying the desired permissions for the file.

Lastly, it is good practice to always check if the `tmpfile()` function was able to create a temporary file before attempting to use it. This can be done by checking the return value of the function, as it will return `false` if it is unable to create a temporary file.

## See Also

To learn more about creating and using temporary files in PHP, check out these helpful resources:

- [PHP.net - Temporary Files](https://www.php.net/manual/en/book.filesystem.php)
- [W3Schools - PHP Temporary Files](https://www.w3schools.com/php/php_file_create_temporary.asp)
- [GeeksforGeeks - PHP Temporary Files](https://www.geeksforgeeks.org/php-temporary-files/)