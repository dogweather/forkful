---
title:                "Working with CSV"
date:                  2024-01-19
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) in Arduino lets you store and manage data as text. It's cheap, easy, and universal, making it ideal for data logging, configuration files, or communicating with spreadsheets and databases.

## How to:
Here's how to save sensor data to a CSV file on an SD card:

```Arduino
#include <SD.h>
#include <SPI.h>

File myFile;
int sensorValue = analogRead(A0);  // pretend sensor value

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {  // SD card is connected to pin 4
    Serial.println("SD card failed or not present");
    return;
  }
  
  myFile = SD.open("data.csv", FILE_WRITE);
  
  if (myFile) {
    myFile.print("Time, SensorValue\n");
    unsigned long time = millis();
    myFile.print(time);
    myFile.print(", ");
    myFile.print(sensorValue);
    myFile.close();
    
    Serial.println("Data written to SD card.");
  } else {
    Serial.println("Error opening file for writing.");
  }
}

void loop() {
  // Nothing to do here
}
```

Sample CSV output in `data.csv`:
```
Time, SensorValue
12345, 678
```

## Deep Dive
CSV format can be traced back to the early days of computing. While there are fancier alternatives, like JSON or XML, CSV remains a go-to due to its simplicity and wide support across platforms. When working with Arduino, keep in mind the limited memory and opt for minimalistic CSV libraries or hand-rolled functions to parse and generate CSV data efficiently.

## See Also
- Arduino's SD library reference: https://www.arduino.cc/en/reference/SD
- Simple CSV parsing in C: https://github.com/robertgamble/simplecsv
- A tutorial on saving Arduino data to Excel: https://www.instructables.com/Save-Arduino-sensor-data-to-a-text-file/
