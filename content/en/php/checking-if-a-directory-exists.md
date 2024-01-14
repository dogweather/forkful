---
title:    "PHP recipe: Checking if a directory exists"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

As a programmer, it's important to have robust code that can handle unexpected scenarios. Checking if a directory exists before performing any file operations helps to prevent errors and ensure the smooth functioning of your program.

## How To

To check if a directory exists in PHP, we can use the `is_dir()` function. This function takes in a string parameter of the directory path and returns a boolean value - `true` if the directory exists and `false` if it doesn't.

Let's see an example of this in action:

```PHP
// We want to check if the "images" directory exists
$directory = "images";

// Using the "is_dir()" function to check if the directory exists
if (is_dir($directory)) {
    echo "The directory " . $directory . " exists!";
} else {
    echo "Sorry, the directory " . $directory . " does not exist.";
}
```

The code above will output `The directory images exists!` if the directory exists, or `Sorry, the directory images does not exist.` if it doesn't.

We can also perform different actions depending on the result of the `is_dir()` function. For example, if the directory already exists, we can simply continue with our code. But if it doesn't exist, we can create it using the `mkdir()` function.

```PHP
if (is_dir($directory)) {
    // Code to be executed if directory exists
    echo "The directory " . $directory . " already exists.";
} else {
    // Code to be executed if directory does not exist
    mkdir($directory);
    echo "The directory " . $directory . " has been created!";
}
```

## Deep Dive

Aside from using the `is_dir()` function, we can also use the `file_exists()` function to check if a directory exists. However, this function can also be used to check for the existence of files, so we need to specify that we are checking for a directory by using the `is_dir()` function as well.

```PHP
// Using "file_exists()" to check if directory exists
if (file_exists($directory)) {
    // Code to be executed if directory exists
    if (is_dir($directory)) {
        echo "The directory " . $directory . " exists!";
    } else {
        echo "Sorry, " . $directory . " is a file, not a directory.";
    }
} else {
    echo "Sorry, the directory " . $directory . " does not exist.";
}
```

It's important to note that both the `is_dir()` and `file_exists()` functions return a boolean value, even if the path provided does not exist at all. This means that if the directory does not exist, these functions will still return `false`.

## See Also

- [PHP Documentation on `is_dir()`](https://www.php.net/manual/en/function.is-dir.php)
- [PHP Documentation on `file_exists()`](https://www.php.net/manual/en/function.file-exists.php)