---
title:                "PHP recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

One common task in PHP programming is to check if a directory exists before performing any operation on it. This can be helpful in scenarios where the directory is expected to exist, but for some reason it may have been deleted or moved.

## How To

Checking if a directory exists in PHP is a straightforward process. The `is_dir()` function can be used to check if a given path is a directory or not. Here is an example of how this function can be used:

```PHP
$directory = "/path/to/directory";
if(is_dir($directory)){
    echo "The directory exists.";
} else {
    echo "The directory does not exist.";
}
```

The `is_dir()` function returns a boolean value (`true` or `false`) depending on whether the directory exists or not. In the above example, if the directory exists, the output will be "The directory exists." If the directory does not exist, the output will be "The directory does not exist."

It is also possible to use the `file_exists()` function to check for the existence of a directory. However, this function can also check for the existence of a file, so it is not as specific as the `is_dir()` function.

```PHP
$directory = "/path/to/directory";
if(file_exists($directory) && is_dir($directory)){
    echo "The directory exists.";
} else {
    echo "The directory does not exist.";
}
```

In the above example, the `file_exists()` function is used first to check if the given path exists. If it does, then the `is_dir()` function is used to ensure that the path is a directory and not a file.

## Deep Dive

When using the `is_dir()` function, it is important to note that it only checks for the existence of the directory, not if it is readable or writable. To check for those permissions, the `is_readable()` and `is_writable()` functions can be used, respectively. 

Another thing to keep in mind is that the path passed to the `is_dir()` function must be a valid path on the server. This means that relative paths (e.g. "../directory") may not work as expected. To ensure proper functionality, it is recommended to use absolute paths.

## See Also
- [PHP is_dir() function documentation](https://www.php.net/manual/en/function.is-dir.php)
- [PHP file_exists() function documentation](https://www.php.net/manual/en/function.file-exists.php)
- [PHP is_readable() function documentation](https://www.php.net/manual/en/function.is-readable.php)
- [PHP is_writable() function documentation](https://www.php.net/manual/en/function.is-writable.php)