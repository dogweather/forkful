---
title:                "Checking if a directory exists"
html_title:           "Arduino recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists simply means verifying whether a particular folder or directory exists on a computer's file system. Programmers often need to perform this check to ensure that the necessary folders are present before executing a code that relies on them. This prevents any errors or failures that may occur due to missing directories.

## How to:
```arduino
if (SD.exists(directory)) { // Replace "directory" with the actual directory name to be checked
  Serial.println("Directory exists!")
} else {
  Serial.println("Directory does not exist.")
}
```
The above Arduino code uses the SD library to check if a directory with the specified name exists. First, the `exists()` function is called, passing the directory name as its parameter. If the directory exists, the code inside the `if` statement is executed, printing out a message to the serial monitor. If not, the code inside the `else` statement is executed, indicating that the directory does not exist.

## Deep Dive:
There are various reasons why a programmer may want to check if a directory exists. In some cases, the directory may be crucial for the proper functioning of the code, and its absence may result in unexpected errors or failures. Additionally, checking if a directory exists can also help in creating new directories if they are missing, ensuring that the code can run smoothly without any interruptions.

An alternative to using the SD library for checking directory existence would be to use the `File()` constructor and `isDirectory()` function. However, this method can be more complex and may involve handling additional error cases.

The implementation of the directory existence check may vary depending on the file system used on the device. For instance, on a Linux system, the `stat()` function can be used, while on a Windows system, the `GetFileAttributes()` function can be used.

## See Also:
- [SD Library Reference - exists()](https://www.arduino.cc/en/Reference/SDexists)
- [File() constructor and isDirectory() function](https://forum.arduino.cc/index.php?topic=668386.0)
- [Detailed explanation of directory existence check implementation](https://unix.stackexchange.com/questions/82526/check-if-directory-exists-and-create-it-if-necessary-on-sh-exit)