---
title:                "Робота з CSV"
aliases:
- uk/arduino/working-with-csv.md
date:                  2024-02-03T19:19:33.204642-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Робота з файлами CSV (Значення, відокремлені комою) в Arduino включає читання з файлів CSV та запис у них, зазвичай зберігаючи на SD-картці, що дозволяє вести журнал даних, налаштування конфігурацій та більше. Програмісти часто обробляють CSV для збору даних сенсорів, зберігання параметрів конфігурації або взаємодії з іншими системами через його простоту і широке використання між платформами.

## Як:
Arduino не має вбудованої бібліотеки спеціально для роботи з файлами CSV, але ви можете використовувати бібліотеки `SD` та `SPI` для доступу до файлів на SD-карті, а потім парсити або генерувати дані CSV за допомогою основних технік маніпуляції з рядками. При роботі з більш складною маніпуляцією CSV може бути використана стороння бібліотека `ArduinoCSV` для легшого парсингу і запису.

**Читання даних CSV з SD-карти:**
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
      Serial.println(dataLine); // Виводить рядок CSV
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // У цьому прикладі не використовується
}
```
*Приклад виводу:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Запис даних CSV на SD-карту:**
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
    dataFile.println("SensorID, Timestamp, Value"); // Заголовок CSV
    dataFile.println("1, 1597840923, 23.5"); // Приклад рядка даних
    dataFile.close();
    Serial.println("Data written");
  } else {
    Serial.println("Error opening output.csv");
  }
}

void loop() {
  // У цьому прикладі не використовується
}
```
*Приклад виводу:*
```
Data written
```

**Використання ArduinoCSV для парсингу:**
Якщо ви працюєте зі складними файлами CSV, бібліотека `ArduinoCSV` може значно спростити зусилля щодо парсингу. Цей приклад передбачає, що ви вже встановили бібліотеку `ArduinoCSV`.

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
          Serial.print(parser.getField(i)); // Друкує кожне поле
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
  // У цьому прикладі не використовується
}
```
*Приклад виводу:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
У цих прикладах, завдяки читанню з файлів CSV та запису в них на SD-картці, проекти Arduino можуть легко збирати дані, зберігати налаштування конфігурації або обмінюватися даними з іншими програмами у універсально доступному форматі.
