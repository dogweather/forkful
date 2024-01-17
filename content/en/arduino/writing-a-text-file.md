---
title:                "Writing a text file"
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file is essentially the process of creating and saving a text document on your computer or a specific device. Programmers often write text files as a way to store and organize code, data, or other types of information that can be easily accessible when needed.

## How to:

To write a text file on your Arduino, you can use the built-in SD library. Here's how:

```
#include <SD.h> // include the SD library

File myFile; // create a file object

void setup() {
  Serial.begin(9600); // initialize the serial monitor
  SD.begin(10); // initialize the SD card
  myFile = SD.open("myFile.txt", FILE_WRITE); // create or open a file named "myFile.txt" for writing
  if (myFile) { // if the file opened successfully
    myFile.println("Hello World!"); // write "Hello World!" to the file
    myFile.close(); // close the file
  }
}

void loop() {
  // do nothing
}
```

When you run this code, you should see "Hello World!" printed to the serial monitor and the text file will be created on your SD card.

## Deep Dive:

Writing text files has been a common practice in programming for many years. It allows for better organization and simplifies the process of updating or modifying code. There are alternative methods for writing and storing data, such as using EEPROM (Electrically Erasable Programmable Read-Only Memory), but text files are often preferred due to their simplicity and ease of access.

The implementation details of writing a text file on an Arduino can vary depending on the specific project and setup. The above example uses an SD card as the storage medium, but you can also write text files to other types of external memory such as a USB drive or an external hard drive. Additionally, the SD library offers various functions for reading, writing, and modifying files, giving you flexibility in how you handle your data.

## See Also:

- [Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [Writing Files to an SD Card with an Arduino](https://www.instructables.com/Writing-Files-to-an-SD-Card-with-an-Arduino/)