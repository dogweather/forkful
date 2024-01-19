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
Checking if a directory exists involves investigating a specific path to verify if the directory is present there. Programmers do this to prevent errors when trying to access or work with directories that may or may not be there.

## How to:

In Arduino, checking if a directory exists on an SD card can be done using the `SD` library. Here's an example:

```Arduino
#include <SD.h>

void setup(){
  Serial.begin(9600);
  if(!SD.begin(4)){
    Serial.println("Initialization failed!");
    return;
  }
  if(SD.exists("/exampleDirectory")){
    Serial.println("Directory exists.");
  } else {
    Serial.println("Directory doesn't exist.");
  }
}

void loop(){}

```
If the directory "exampleDirectory" exists on the SD card, the Serial monitor would display "Directory exists." If it doesn't, the message "Directory doesn't exist." is shown.

## Deep Dive
Checking for directories harkens back to the old days of MS-DOS. It was, and still is, an effective way to prevent file manipulation errors.

As an alternative, programmers could simply attempt to open a directory and wait for an error to indicate failure. However, checking a directory’s existence first avoids unnecessary exceptions.

When `SD.exists("/exampleDirectory")` is called in Arduino, it executes a function in the SD library that sends a command to the SD card and waits for a response indicating if the directory exists. 

## See Also:

1. [Arduino SD Library Documentation](https://www.arduino.cc/en/Reference/SD) - Official documentation for the SD library.
2. [Arduino File Manipulation](https://www.arduino.cc/en/Tutorial/FileManipulation) - Learn more about managing files and directories.
3. [Arduino SD Card Tutorial](https://www.makerguides.com/arduino-sd-card-tutorial/) - Comprehensive guide for handling SD card data with Arduino.