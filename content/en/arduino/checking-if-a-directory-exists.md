---
title:                "Arduino recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

When programming an Arduino, it is important to know if a certain directory or folder exists. This is useful for managing file paths and preventing the program from crashing if it encounters a non-existent directory.

## How To

To check if a directory exists in Arduino, we can use the `SD.exists()` function. Let's take a look at an example:

```
#include <SD.h>

void setup() {
  Serial.begin(9600);
  // initialize SD card
  if(!SD.begin(4)) {
    Serial.println("SD card initialization failed.");
    while(true);
  }
}

void loop() {
  // check if directory "data" exists
  if(SD.exists("/data")) {
    Serial.println("Directory exists!");
  } else {
    Serial.println("Directory does not exist.");
  }
  delay(1000);
}
```

In this example, we first initialize the SD card in the `setup()` function. Then in the `loop()` function, we use the `SD.exists()` function to check if the directory named "data" exists. If it does, we print out "Directory exists!" to the serial monitor. If it doesn't, we print out "Directory does not exist."

You can modify this code to check for any directory on your SD card. The `SD.exists()` function takes in a string as its parameter, which is the path to the directory you want to check. You can also use this function to check if a specific file exists in a directory.

## Deep Dive

Under the hood, the `SD.exists()` function uses the `SD.open()` function to check for the directory. If the directory exists, it returns a `File` object and then closes it. If the directory doesn't exist, it returns a null `File` object. This is why we can use the `if` statement to check for its existence.

It is important to note that the `SD.exists()` function only works if the SD card is properly initialized and mounted. If not, it will always return false even if the directory does exist.

## See Also

- [SD Library Reference for Arduino](https://www.arduino.cc/en/Reference/SD)

- [Arduino String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)

- [Introduction to SD card modules for Arduino](https://www.instructables.com/id/Introduction-to-SD-Card-Modules-for-Arduino/)