---
title:    "PHP recipe: Checking if a directory exists"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

In the world of programming, it’s crucial to ensure the stability and functionality of your code. This includes checking if a directory exists before attempting to create or manipulate files within it. By doing so, you can avoid any potential errors and ensure that your code runs smoothly.

## How To

Checking if a directory exists in PHP is a simple process that can be done using the `is_dir()` function. This function takes in the path of the directory as a parameter and returns a boolean value of `true` if the directory exists or `false` if it doesn’t.

Let’s take a look at an example to understand this better:

```PHP
$path = "uploads/documents";

if (is_dir($path)) {
  echo "The directory $path exists!";
} else {
  echo "The directory $path doesn't exist!";
}
```

In this example, we are checking if the `uploads/documents` directory exists. If it does, the output will be `The directory uploads/documents exists!`. If it doesn’t, the output will be `The directory uploads/documents doesn't exist!`.

You can also use the `chmod()` function to set the permissions of the directory before creating or manipulating files within it.

## Deep Dive

Behind the scenes, the `is_dir()` function uses a system call to check if the directory exists. This makes it more efficient compared to manually checking for the directory’s existence using file functions. Additionally, the function also checks if the directory is readable, ensuring that you have the necessary permissions to access it.

If you want to check for the existence of a directory relative to the current directory, you can use the `getcwd()` function, which returns the current working directory. It can be used in combination with the `is_dir()` function to check for directories in specific paths.

## See Also

If you want to learn more about checking for directory existence in PHP, check out the following resources:

- [PHP Documentation on `is_dir()` function](https://www.php.net/manual/en/function.is-dir.php)
- [Code Snippets for `is_dir()` function](https://www.php.net/manual/en/function.is-dir.example.php)
- [PHPTheRightWay: File System Functions](https://phptherightway.com/pages/PHP-FileSystem-Summary.html)