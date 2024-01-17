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

## What & Why?

Checking if a directory exists is the process of verifying if a specific directory or folder exists on the server or computer. This can be done using PHP code and is important for programmers to ensure that their code is functioning correctly and can access necessary files.

## How to:

To check if a directory exists in PHP, you can use the ```file_exists()``` function. This function takes in the path of the directory as a parameter and returns a boolean value, true if the directory exists and false if it does not.

Example code:

```
<?php

$directory = "/path/to/directory";

if(file_exists($directory)){
  echo "The directory exists!";
} else {
  echo "The directory does not exist.";
}
```

Sample output:
```
The directory exists!
```

## Deep Dive:

There are a few alternative ways to check if a directory exists in PHP such as using the ```is_dir()``` function or the ```glob()``` function. However, the ```file_exists()``` function is the most commonly used and efficient method.

Previously, checking if a directory exists was done using the ```opendir()``` function which would return a resource if the directory existed and false if it did not. However, this method is now obsolete and the ```file_exists()``` function should be used instead.

It is also important to note that the path to the directory needs to be specified correctly for the ```file_exists()``` function to work. This includes making sure the correct file path separators are used and the correct permissions are set for the directory.

## See Also:

- [PHP: file_exists() function](https://www.php.net/manual/en/function.file-exists.php)
- [Alternative directory checking methods in PHP](https://www.php.net/manual/en/function.is-dir.php)
- [Overview of file paths and permissions in PHP](https://www.php.net/manual/en/features.file-upload.php)