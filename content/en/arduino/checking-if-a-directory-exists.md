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

## Why
Checking if a directory exists is an important aspect of programming, especially on the Arduino platform. It allows you to ensure that the program is accessing the correct directory and prevents errors from occurring.

## How To
```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  // initialize SD card
  if(!SD.begin(10)) {
    // if directory does not exist, create it
    SD.mkdir("myDirectory");
  }
}

void loop() {
  // do something with directory
}
```

The above code block shows how to use the `SD.mkdir()` function to check if a directory exists and create it if it doesn't. This function is part of the SD library which needs to be included in order to access the SD card on the Arduino.

Sample output:
```
Creating directory: myDirectory
```

## Deep Dive
Behind the scenes, the `SD.mkdir()` function uses the standard Arduino `mkdir()` function which is part of the `SPI.h` library. This function takes in a string as its argument, representing the name of the directory to be created.

One important thing to note is that the `mkdir()` function can only create one directory at a time. So, if you want to create multiple directories, you will need to use a loop or call the function multiple times.

If the directory already exists, the `SD.mkdir()` function will not create a new directory and will instead return a false value. This can be useful in case you need to check if a specific directory exists before trying to create it.

## See Also
- [Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [SPI Library Reference](https://www.arduino.cc/en/Reference/SPI)
- [Creating and Removing Directories on an SD card](https://create.arduino.cc/projecthub/SurtrTech/creating-and-removing-directories-on-sd-card-3664c1)