---
title:                "PHP recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a useful skill to have for any PHP programmer. Temporary files are often used when dealing with large amounts of data or when you need to save data that is not meant to be stored permanently. In this blog post, we will explore the benefits of creating temporary files and how it can help improve your code.

## How To

Creating a temporary file in PHP is a simple and straightforward process. Let's take a look at an example:

```PHP
// Create a temporary file
$tempFile = tmpfile();

// Write data to the file
fwrite($tempFile, "This is a temporary file.");

// Read data from the file
echo fread($tempFile, filesize($tempFile));

// Close the file
fclose($tempFile);
```

In this code, we first use the `tmpfile()` function to create a temporary file and assign it to the variable `$tempFile`. Then, we use the `fwrite()` function to write data to the file. In this case, we are writing the string "This is a temporary file." Next, we use the `fread()` function to read the data from the file and output it using `echo`. Finally, we close the file using the `fclose()` function.

You can also specify a directory where the temporary file should be created by passing the directory path as an argument to the `tmpfile()` function. For example:

```PHP
// Create a temporary file in a specific directory
$tempFile = tmpfile("/tmp");
```

## Deep Dive

Now, let's take a deeper look at creating temporary files in PHP. When we use the `tmpfile()` function, PHP automatically creates a unique file name and returns a file pointer to the newly created file. This eliminates the need for us to generate a unique file name ourselves.

Additionally, the temporary file created by `tmpfile()` is automatically deleted when the script ends or when the file is closed using `fclose()`. This ensures that we don't have to worry about deleting the file manually.

It is worth noting that the `tmpfile()` function only creates an empty file. If you want to write data to the file, you will need to use the `fwrite()` function as shown in the example above.

## See Also

- [PHP Official Documentation: tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP Official Documentation: fwrite()](https://www.php.net/manual/en/function.fwrite.php)
- [PHP Official Documentation: fclose()](https://www.php.net/manual/en/function.fclose.php)

Creating temporary files in PHP can be a useful tool in your programming arsenal. Whether you need to manage large amounts of data or just need to store temporary information, now you know how to create temporary files with ease. Happy coding!