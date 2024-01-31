---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:57:32.050677-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists lets you confirm a folder's presence in the file system before you try to use it. Programmers do this to avoid errors when reading, writing, or navigating directories.

## How to:

In PHP, `is_dir()` checks if a directory exists:

```PHP
$directory = "/path/to/dir";

if (is_dir($directory)) {
    echo "The directory exists.";
} else {
    echo "The directory does not exist.";
}
```

Sample Output:
```
The directory exists.
```
Or, if the directory doesn't, indeed, exist:
```
The directory does not exist.
```

To suppress errors and use a more granular check, combine `is_dir()` with the `file_exists()` function:

```PHP
$directory = "/path/to/dir";

if (file_exists($directory) && is_dir($directory)) {
    echo "The directory exists and is a directory.";
} else {
    echo "The directory does not exist or is a file.";
}
```

## Deep Dive

`is_dir()` has been in PHP since version 4.0.0, allowing checks for directory existence before operations that could fail or raise errors. Not to be confused with `file_exists()`, which checks for files and directories alike, `is_dir()` is specifically for directories.

Prior to these built-in functions, programmers might have used `opendir()` and checked for a false return value to infer non-existence. This was less efficient and more error-prone.

Under the hood, `is_dir()` performs a syscall to the underlying file system, which can be costlier in terms of I/O operations, especially for remote or virtual file systems. Caching results or structuring code to minimize existence checks can optimize performance.

One alternative, especially relevant in Unix-like systems, is using `exec()` with a system command like `ls` or `test -d`, but this introduces the overhead of invoking a shell and is less portable.

## See Also

- [PHP Manual: `is_dir()`](https://www.php.net/manual/en/function.is-dir.php)
- [PHP Manual: `file_exists()`](https://www.php.net/manual/en/function.file-exists.php)
- [Filesystem best practices in PHP](https://www.php-fig.org/psr/psr-4/)
- [PHP filesystem functions](https://www.php.net/manual/en/ref.filesystem.php)
