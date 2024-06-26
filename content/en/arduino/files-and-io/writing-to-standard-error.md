---
date: 2024-02-03 19:03:44.248510-07:00
description: "How to: Arduino doesn't natively differentiate between standard output\
  \ and standard error as conventional computing systems do. Both `Serial.print()`\
  \ and\u2026"
lastmod: '2024-03-13T22:45:00.335701-06:00'
model: gpt-4-0125-preview
summary: Arduino doesn't natively differentiate between standard output and standard
  error as conventional computing systems do.
title: Writing to standard error
weight: 25
---

## How to:
Arduino doesn't natively differentiate between standard output and standard error as conventional computing systems do. Both `Serial.print()` and `Serial.println()` methods write to the same serial output, typically viewed in the Arduino IDE Serial Monitor. However, we can emulate writing to stderr by specifically formatting error messages or directing them to an alternative output, such as a file on an SD card or over a network connection.

To emulate stderr, you can prefix error messages with a tag like "ERROR:" to differentiate them in the Serial Monitor:

```cpp
void setup() {
  Serial.begin(9600); // Initialize serial communication at 9600 baud rate
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // Emulating stderr by prefixing the error message
    Serial.println("ERROR: The function failed to execute.");
  } else {
    Serial.println("The function executed successfully.");
  }
  delay(1000); // Wait for a second before restarting the loop
}

int someFunction() {
  // A dummy function that returns -1 on error
  return -1;
}
```

Sample output in the Arduino IDE Serial Monitor might look like this:

```
ERROR: The function failed to execute.
```

For projects requiring a more sophisticated approach, including writing to different physical outputs, the use of third-party libraries or additional hardware may be necessary. For example, logging error messages to an SD card requires the `SD` library:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: SD card initialization failed!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: The function failed to execute.");
    myFile.close(); // Make sure to close the file to save the contents
  } else {
    Serial.println("ERROR: Opening error.log failed!");
  }
}

void loop() {
  // Your main code would go here
}
```

With this approach, you physically separate normal program output and error messages by directing the latter to an `error.log` file on an SD card, enabling post-mortem analyses without cluttering the primary output channel.
