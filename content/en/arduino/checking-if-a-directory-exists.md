---
title:    "Arduino recipe: Checking if a directory exists"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

As an Arduino programmer, you may come across situations where you need to check if a directory exists. This is particularly useful when working with files, as it allows you to ensure that the file you are trying to access actually exists before proceeding with your code.

## How To

To check if a directory exists, we will use the Arduino `File` object and the `exists()` method. First, we need to create a `File` object and pass in the path of the directory we want to check. Then, we can use the `exists()` method to determine if the directory exists or not. Here's an example code block:

```Arduino
// Create a File object for the directory "myDirectory"
File myDirectory = SD.open("myDirectory");

// Check if the directory exists
if (myDirectory.exists()) {
  Serial.println("The directory exists!");
} else {
  Serial.println("The directory does not exists!");
}
```

If the directory "myDirectory" exists, the output will be `The directory exists!`, and if it doesn't exist, the output will be `The directory does not exist!`.

## Deep Dive

When we use the `exists()` method, it checks for both file and directory existence. If the specified path exists and it is a valid directory, then the method will return `true`. However, if the path exists but is not a valid directory, the method will return `false`.

It is important to note that the `exists()` method does not check for the validity of the path itself. So it is possible for the method to return `false` even if the path is correctly specified. Therefore, it is recommended to check the return value of the `exists()` method before proceeding with any further file operations.

## See Also

If you want to learn more about using the `exists()` method and other methods available in the `File` object, here are some useful resources:

- [Arduino Reference - File Object](https://www.arduino.cc/reference/en/libraries/sd-card-library/file-object/)
- [Arduino Code Examples - SD Library](https://www.arduino.cc/en/Reference/SD)
- [How to Read and Write Files on an SD Card with Arduino](https://www.circuitbasics.com/how-to-write-files-to-and-read-files-from-sd-cards/)