---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file in programming is extracting info from a .txt file. Programmers do this when they need to access external data for their projects. 

## How to:

We'll explore reading a text file with the popular Arduino platform. Thing is, Arduino is microcontroller based and has limited memory, so we can't directly read .txt files. Instead, we read data from SD cards (formatted to FAT16 or FAT32) in a txt-like format. Here's how.

First, wire up an SD card module. Then, use the SD and SPI libraries like this:

``` Arduino
#include <SD.h>
#include <SPI.h>

File myFile;

void setup(){
  Serial.begin(9600);
  // make sure that the default chip select pin is set to output, even if you don't use it:
  pinMode(4, OUTPUT);
 
  if (!SD.begin(4)) {
    Serial.println("Card failed, or not present");
    // don't do anything else:
    while (1);
  }
  // open the file
  myFile = SD.open("test.txt");
}

void loop(){}

```

Excellent, we've got the SD card ready. Now, let's read data.

```Arduino
void loop(){
  if (myFile) {
    // read from the file until there's nothing else in it:
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
   // if the file didn't open, print an error:
   Serial.println("error opening test.txt");
  }
}
```

Your output will be the content of your `test.txt` file.

## Deep Dive

Historically, microcontrollers aren't designed for file operations. Thus, the ability to read data from an SD card is a big deal. 

You could use EEPROM (Electrically Erasable Programmable Read Only Memory) instead, but it has much less storage.

In terms of implementation details, the `SD.open()` function is crucial. It opens the file for reading, and returns a file object. `myFile.available()` checks if the file has data left to read and `myFile.read()` returns the next byte or -1 if none remain.

## See Also

- [SD Card library](https://www.arduino.cc/en/Reference/SD)
- [SPI library](https://www.arduino.cc/en/reference/SPI)