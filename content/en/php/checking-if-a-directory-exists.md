---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

In PHP, verifying if a directory exists is exactly what it sounds like: your code checks if a particular folder is present on the server before attempting to carry out operations within it. This helps to prevent bugs and potential crashes due to missing data or file paths.

## How to:

In PHP, you can check if a directory exists with the `is_dir` function, which returns true if the directory exists. Here it is in action:

```PHP
$directory = "/path/to/directory";

if(is_dir($directory)) {
    echo "Directory exists";
} else {
    echo "Directory does not exist";
}
```

Running this script would output "Directory exists" if the path is valid.

## Deep Dive

The `is_dir` function has been around since PHP 4, and it's always been a simple way to detect directories. You could also use the `file_exists` function, which checks for files and directories alike, but `is_dir` is more specific and thus a better choice in most cases.

However, one important thing to be aware of is its behavior with symbolic links. If the function is used on a link to a directory, it will return true only if the directory actually exists at the target.

More complex checks (e.g. to also verify permissions or the existence of a file within the directory) can be performed using the `fileperms` function or by attempting `fopen`, respectively, but in general, `is_dir` is a quick and easy test that reduces the likelihood of errors in your code.

## See Also

For further reading, check out the PHP.net doсumentation on [`is_dir`](https://www.php.net/manual/en/function.is-dir) and [`file_exists`](https://www.php.net/manual/en/function.file-exists.php). Moreover, for more intricate file system operations, consider diving deeper into the [PHP filesystem functions](https://www.php.net/manual/en/book.filesystem.php).