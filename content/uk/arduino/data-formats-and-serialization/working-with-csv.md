---
aliases:
- /uk/arduino/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:33.204642-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 CSV (\u0417\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0432\
  \u0456\u0434\u043E\u043A\u0440\u0435\u043C\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\
  \u043E\u044E) \u0432 Arduino \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0447\u0438\
  \u0442\u0430\u043D\u043D\u044F \u0437 \u0444\u0430\u0439\u043B\u0456\u0432 CSV \u0442\
  \u0430 \u0437\u0430\u043F\u0438\u0441 \u0443 \u043D\u0438\u0445, \u0437\u0430\u0437\
  \u0432\u0438\u0447\u0430\u0439 \u0437\u0431\u0435\u0440\u0456\u0433\u0430\u044E\u0447\
  \u0438 \u043D\u0430 SD-\u043A\u0430\u0440\u0442\u0446\u0456, \u0449\u043E \u0434\
  \u043E\u0437\u0432\u043E\u043B\u044F\u0454\u2026"
lastmod: 2024-02-18 23:09:00.841891
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\u0430\
  \u043C\u0438 CSV (\u0417\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0432\u0456\
  \u0434\u043E\u043A\u0440\u0435\u043C\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\u043E\
  \u044E) \u0432 Arduino \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0447\u0438\u0442\
  \u0430\u043D\u043D\u044F \u0437 \u0444\u0430\u0439\u043B\u0456\u0432 CSV \u0442\u0430\
  \ \u0437\u0430\u043F\u0438\u0441 \u0443 \u043D\u0438\u0445, \u0437\u0430\u0437\u0432\
  \u0438\u0447\u0430\u0439 \u0437\u0431\u0435\u0440\u0456\u0433\u0430\u044E\u0447\u0438\
  \ \u043D\u0430 SD-\u043A\u0430\u0440\u0442\u0446\u0456, \u0449\u043E \u0434\u043E\
  \u0437\u0432\u043E\u043B\u044F\u0454\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
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
