---
title:                "Checking if a directory exists"
html_title:           "PHP recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered an error in your PHP code where the script couldn't find a specific directory? It's a common problem that can cause frustration and delay in development. In this article, we'll go over why it's important to check if a directory exists before attempting to use it in your code.

## How To

To check if a directory exists in PHP, we can use the `file_exists()` function. This function takes in a directory path as its parameter and returns a boolean value - `true` if the directory exists and `false` if it doesn't. Let's take a look at an example:

```PHP
<?php
$dir = 'images';
if (file_exists($dir)) {
  echo "The directory $dir exists.";
} else {
  echo "The directory $dir does not exist.";
}
```

In this code, we first assign the directory path to a variable called `$dir`. Then, we use a conditional statement to check if the directory exists using the `file_exists()` function. If it does, we print out a message stating that the directory exists. Otherwise, we print out a message stating that the directory does not exist.

You can also use the `is_dir()` function to check if the given path is a directory or not. This function also returns a boolean value - `true` if the path is a directory and `false` if it's not. Let's see it in action:

```PHP
<?php
$dir = 'images';
if (is_dir($dir)) {
  echo "The path $dir is a directory.";
} else {
  echo "The path $dir is not a directory.";
}
```

In this example, we're checking if the path is a directory using the `is_dir()` function. If it is, we print out a message stating that the path is a directory. Otherwise, we print out a message stating that the path is not a directory.

## Deep Dive

Under the surface, the `file_exists()` and `is_dir()` functions use the same underlying system call - `stat()`. This function returns information about the given file or directory, including its type, size, and permissions. It's this information that is used to determine if the given path is a file or directory.

It's important to note that `file_exists()` and `is_dir()` only check if the path exists at the time the function is called. If the directory is deleted after the check, the function will return a false value even though the directory existed before.

## See Also

- [PHP: file_exists() function](https://www.php.net/manual/en/function.file-exists.php)
- [PHP: is_dir() function](https://www.php.net/manual/en/function.is-dir.php)
- [PHP: stat() function](https://www.php.net/manual/en/function.stat.php)