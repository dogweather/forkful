---
title:                "Checking if a directory exists"
html_title:           "C recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists is all about verifying a folder's presence on your storage before you do something with it. Programmers do this to avoid errors, like trying to create a directory that's already there, or accessing one that isn't.

## How to:
Working with directories on Arduino often involves the SD library for storage on an SD card. First, ensure your Arduino is hooked up with an SD card module properly. Then, you use `SD.exists()` function to check for a directory's existence. Here's a quick example:
```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }

  if (!SD.begin(4)) { // Make sure to use the correct chip select pin
    Serial.println("Initialization failed!");
    return;
  }

  if (SD.exists("/example")) {
    Serial.println("/example directory exists.");
  } else {
    Serial.println("/example directory doesn't exist.");
  }
}

void loop() {
  // Nothing to do here
}
```
Sample output when the directory exists:
```
/example directory exists.
```
And when it doesn't:
```
/example directory doesn't exist.
```
Remember to replace `/example` with the actual path you want to check.

## Deep Dive
Way back when, checking for a directory's existence wasn't always straightforward. Systems had diverse commands. In Arduino's case, the SD library made it consistent, borrowing concepts from standard programming practices.

As for alternatives, if you're working with non-SD storage or need more control, other libraries like SdFat provide similar functionality with added features. Some advanced implementations might interact with file systems more directly, but for most users, SD.exists() is enough.

Checking a directory involves the library asking the file system to look up a special file entry that represents the directory. If it's there, fantastic. If not, you get a false. The SD library handles the low-level communication between your Arduino and the storage medium's file system, abstracting away the gritty details—so you get the info you need without fuss.

## See Also
- Arduino's SD Library Reference: [https://www.arduino.cc/en/Reference/SD](https://www.arduino.cc/en/Reference/SD)
- SdFat Library for more robust SD card interaction: [https://github.com/greiman/SdFat](https://github.com/greiman/SdFat)
