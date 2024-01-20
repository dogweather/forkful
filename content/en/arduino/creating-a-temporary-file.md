---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file involves programming a file designed to store data temporarily during a program's execution. Programmers do it to hold intermediate processing data, debug issues or store data that doesn't need to persist longer.

## How To:

In Arduino, you can use the SD library to create temporary files. Let's walk through this together.

```Arduino
#include <SD.h>

File temp;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed.");
    return;
  }
  Serial.println("Initialization done.");

  temp = SD.open("temp.txt", FILE_WRITE);
  
  if (temp) {
    temp.println("This is a temporary file.");
    temp.close();
    Serial.println("Writing done.");
  } else {
    Serial.println("Error opening temp.txt.");
  }
}

void loop() {
  // nothing here
}
```

This will write the string “This is a temporary file.” to a temporary file name `temp.txt`. And if successful it will print `"Writing done."`.

## Deep Dive

The practice of creating temporary files has been around since the early days of computing - those temporary files were often stored on punch cards! When you create a temporary file in Arduino, you're tapping into functionality as old as programming itself. 

Arduino and its libraries don't provide a direct way to create and manage temporary files like some operating systems do. But with an SD card module, you can practically implement it. It's important to delete these files when you're done, as unlike many modern operating systems, Arduino won't automatically clear temp files for you.

An alternative to SD cards for temporary storage is EEPROM, but this is limited in size and has a finite number of write cycles. 

## See Also

1. Learn more about file handling with SD cards in Arduino: [Arduino SD Library](https://www.arduino.cc/en/Reference/SD)

2. To explore how temporary files are managed in more traditional operating systems: [Temporary Files in Windows](https://docs.microsoft.com/en-us/troubleshoot/windows-client/performance/temporary-windows-files)

3. Arduino's guide to using the onboard EEPROM for persistent storage: [Arduino EEPROM Library](https://www.arduino.cc/en/Reference/EEPROM)