---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file on Arduino means storing data as text in a file, typically on an SD card. Programmers do this to save data like sensor readings for later analysis or to log events over time.

## How to:
First, hook up an SD card reader to your Arduino. Then you’ll need the SD library. Here's a quick script:

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Start the serial communication
  Serial.begin(9600);
  
  // Check for SD card initialization
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  
  // Create/open a text file
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // If the file opened okay, write to it
  if (myFile) {
    myFile.println("Hello, world!");
    myFile.close(); // Close the file
    Serial.println("Write done.");
  } else {
    // If the file didn't open, print an error
    Serial.println("Error opening test.txt");
  }
}

void loop() {
  // Nothing here
}
```

Sample output would be "Write done." on the serial monitor, and "Hello, world!" in "test.txt" on the SD card.

## Deep Dive
Historically, Arduino's memory constraints made data logging a chore. With modern modules and SD cards, it's simpler. Alternatives like EEPROM or direct transmission to a computer are fine but have limits (EEPROM wears out, transmission needs a connection). Writing to a file is straightforward with `SD.h` but remember: the library uses quite a bit of memory, so it's better for boards with more SRAM.

## See Also
For more info, check these:
- The official SD library documentation: https://www.arduino.cc/en/Reference/SD
- Detailed SD card module hookup guide: https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial
- Arduino's File class for file operations: https://www.arduino.cc/en/Reference/File
