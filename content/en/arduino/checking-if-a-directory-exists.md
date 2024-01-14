---
title:                "Arduino recipe: Checking if a directory exists"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
As an Arduino programmer, you may encounter situations where you need to check if a directory exists. This can be useful for tasks such as data logging or file management, where you need to ensure that a specific directory is present before proceeding with the rest of your program.

## How To
To check if a directory exists, we will be using the `SD.exists()` function from the Arduino SD library. Let's say we want to check for the existence of a directory named "logs" on our SD card. We can do so with the following code:

```Arduino
#include <SD.h> // include the SD library
...
if (SD.exists("/logs")) { // check if "logs" directory exists
    // directory exists, do something here
} else {
    // directory does not exist, handle the error
}
```

You can replace "logs" with the name of the directory you want to check for. If the directory exists, the `SD.exists()` function will return `true` and the code within the `if` statement will be executed. Otherwise, the `else` statement will be executed.

To make sure that the `SD.exists()` function works, let's print the result to the serial monitor. We will modify our code as follows:

```Arduino
#include <SD.h> // include the SD library
...
if (SD.exists("/logs")) { // check if "logs" directory exists
    Serial.println("Directory exists!");
} else {
    Serial.println("Directory does not exist!");
}
```

If the directory exists, you will see "Directory exists!" printed to the serial monitor. If it does not exist, you will see "Directory does not exist!".

## Deep Dive
The `SD.exists()` function internally calls the `SD.open()` function, which attempts to open a file or directory. If the file or directory exists, `SD.open()` will return `true`, indicating that the file or directory exists. Otherwise, it will return `false`.

It is important to note that the `SD.exists()` function only checks for the existence of a directory or file, it does not guarantee that the directory or file is accessible. There may be other factors such as file permissions or incorrect file path that can prevent files or directories from being accessed.

## See Also
- [SD library reference](https://www.arduino.cc/en/Reference/SD)
- [Arduino and SD card tutorial](https://www.arduino.cc/en/tutorial/SDCardNotes)