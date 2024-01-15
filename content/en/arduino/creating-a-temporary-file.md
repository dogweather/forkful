---
title:                "Creating a temporary file"
html_title:           "Arduino recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files on an Arduino board can be useful for a variety of reasons. Temporary files can be used to store data that needs to be accessed later, to track a certain process or state, or to store data temporarily before it is sent to a computer or server.

## How To

To create a temporary file on an Arduino board, we can use the `File::createTemp()` function from the `SD` library. This function takes in two parameters: the base name and the extension of the file. Here's an example code:

```Arduino
#include <SD.h>

void setup() {
  // Initialize SD card
  SD.begin();

  // Create a temporary file named "data.txt"
  File tempFile = SD.createTemp("data", ".txt");
  
  // Write data to the file
  tempFile.println("Hello, world!");

  // Close the file
  tempFile.close();
}

void loop() {
  // Other code goes here
}
```

Running this code will create a temporary file named "data.txt" on the SD card. Note that the file name will have a randomly generated number appended to it to ensure a unique file name. 

To read the data from our temporary file, we can use the `File::read()` function. Here's an example code:

```Arduino
#include <SD.h>

void setup() {
  // Initialize SD card
  SD.begin();

  // Open the temporary file
  File tempFile = SD.open("data.txt");
  
  // Read and print a line from the file
  String data = tempFile.readStringUntil('\n');
  Serial.println(data);

  // Close the file
  tempFile.close();
}

void loop() {
  // Other code goes here
}
```

This code will print "Hello, world!" to the serial monitor. We can also delete the temporary file by using the `File::remove()` function.

## Deep Dive

When we create a temporary file on an Arduino board, it is stored in the temporary folder on the SD card. This folder is automatically created when the SD card is initialized and can be accessed by using the `SD::tempDir()` function. By default, the temporary folder is named "tmp".

Temporary files have a lifetime of 24 hours on an Arduino board. After that, they are automatically deleted when the SD card is initialized. Additionally, it is recommended to not store more than 10 temporary files on the SD card to avoid memory issues.

## See Also

- [SD library reference](https://www.arduino.cc/en/reference/SD)
- [File object reference](https://www.arduino.cc/reference/en/libraries/sd/card/filereadname/)
- [Using SD cards with Arduino](https://www.arduino.cc/en/Guide/ArduinoSDCard)