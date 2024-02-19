---
aliases:
- /en/arduino/working-with-csv/
date: 2024-02-03 19:03:17.985468-07:00
description: "Working with CSV (Comma-Separated Values) files in Arduino involves\
  \ reading from and writing to CSV files usually stored on an SD card, enabling data\u2026"
lastmod: 2024-02-18 23:09:11.337053
model: gpt-4-0125-preview
summary: "Working with CSV (Comma-Separated Values) files in Arduino involves reading\
  \ from and writing to CSV files usually stored on an SD card, enabling data\u2026"
title: Working with CSV
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) files in Arduino involves reading from and writing to CSV files usually stored on an SD card, enabling data logging, configuration settings, and more. Programmers often handle CSVs for sensor data collection, configuration parameter storage, or interfacing with other systems, due to its simplicity and wide adoption across platforms.

## How to:
Arduino doesn't have a built-in library specifically for handling CSV files, but you can use the `SD` and `SPI` libraries for accessing files on an SD card, and then parse or generate CSV data using basic string manipulation techniques. When dealing with more complex CSV manipulation, the third-party library `ArduinoCSV` can be utilized for easier parsing and writing.

**Reading CSV Data from an SD Card:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Prints the CSV line
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // Not used in this example
}
```
*Sample Output:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Writing CSV Data to an SD Card:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // CSV header
    dataFile.println("1, 1597840923, 23.5"); // Example data row
    dataFile.close();
    Serial.println("Data written");
  } else {
    Serial.println("Error opening output.csv");
  }
}

void loop() {
  // Not used in this example
}
```
*Sample Output:*
```
Data written
```

**Using ArduinoCSV for Parsing:**
If handling complex CSV files, the `ArduinoCSV` library can significantly simplify parsing efforts. This example assumes you have already installed the `ArduinoCSV` library.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Print each field
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // Not used in this example
}
```
*Sample Output:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
In these examples, by reading from and writing to CSV files on an SD card, Arduino projects can easily collect data, store configuration settings, or exchange data with other applications in a universally accessible format.
