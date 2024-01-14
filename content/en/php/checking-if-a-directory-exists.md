---
title:                "PHP recipe: Checking if a directory exists"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
When writing PHP code, it is important to ensure that all necessary directories and files exist. By checking if a directory exists, we can prevent potential errors and ensure that our code runs smoothly.

## How To

To check if a directory exists in PHP, we can use the `is_dir()` function. This function takes in a string representing the directory path and returns a boolean value indicating whether the directory exists or not.

Let's take a look at a simple example:

```PHP
$directory = '/path/to/dir';
if (is_dir($directory)) {
    echo 'The directory exists.';
} else {
    echo 'The directory does not exist.';
}
```

In this example, we first set the directory path as a variable. Then, we use the `is_dir()` function to check if the directory exists. If it does, the code within the if statement will be executed and we will see the output "The directory exists." If the directory does not exist, the code within the else statement will be executed and we will see the output "The directory does not exist."

We can also add additional logic to our code, such as creating the directory if it does not exist or displaying an error message if the directory cannot be created. Here's an example:

```PHP
$directory = '/path/to/dir';
if (!is_dir($directory)) {
    // create the directory if it does not exist
    if (!mkdir($directory)) {
        echo 'Error creating directory.';
    } else {
        echo 'Directory created.';
    }
} else {
    echo 'The directory already exists.';
}
```

In this example, we first check if the directory exists using the `is_dir()` function. If it does not exist, we then use the `mkdir()` function to create the directory. The `mkdir()` function returns a boolean value, so we can check if the directory was successfully created and display an appropriate message.

## Deep Dive
Behind the scenes, the `is_dir()` function makes use of the `opendir()` function which opens a directory handle and returns a resource. This resource is then passed to the `closedir()` function to close the directory handle.

Additionally, the `is_dir()` function also checks for symbolic links and will return `true` if a symbolic link points to a directory that exists.

It is also worth noting that the `is_dir()` function will not work for directories on remote locations, as it only checks for directories on the local file system.

## See Also
- [PHP Documentation - is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [PHP Documentation - mkdir()](https://www.php.net/manual/en/function.mkdir.php)
- [PHP Documentation - opendir()](https://www.php.net/manual/en/function.opendir.php)
- [PHP Documentation - closedir()](https://www.php.net/manual/en/function.closedir.php)