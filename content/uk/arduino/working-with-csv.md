---
title:                "Робота з CSV файлами"
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Що таке і чому?)
CSV – це формат для зберігання табличних даних. Програмісти використовують його для зручності обміну даними між різними програмами та пристроями.

## How to: (Як робити:)
```Arduino
#include <SD.h>
File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(10);
  myFile = SD.open("data.csv", FILE_WRITE);
  
  if (myFile) {
    myFile.println("sensor1,sensor2,sensor3"); // Writing header
    myFile.println("23,45,67"); // Writing data
    myFile.close(); // Always close the file
  } else {
    Serial.println("Error opening file");
  }
}

void loop() {
  // Reading the CSV file
  myFile = SD.open("data.csv");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
   Serial.println("Error opening file");
  }
}
```
Sample Output:
```
sensor1,sensor2,sensor3
23,45,67
```

## Deep Dive (Поглиблений аналіз):
CSV створений у 1970-х і простий у використанні. Альтернативи: JSON, XML. Істотно для CSV – правильна обробка тексту та розмежування значень комами.

## See Also (Дивіться також):
- Arduino’s SD library reference: [SD Library](https://www.arduino.cc/en/Reference/SD)
- Tutorial on processing CSV files in C++: [Working with CSV](https://www.geeksforgeeks.org/csv-file-management-using-c/)
- CSV specification by RFC 4180: [RFC 4180](https://tools.ietf.org/html/rfc4180)
