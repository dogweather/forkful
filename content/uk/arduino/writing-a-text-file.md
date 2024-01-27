---
title:                "Створення текстового файлу"
date:                  2024-01-19
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Запис текстових файлів - це процес створення та зберігання даних у форматі зрозумілому людям. Роблять це для логування даних, налаштувань та передачі інформації.

## Як робити:
```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  
  myFile = SD.open("test.txt", FILE_WRITE);
  
  if (myFile) {
    myFile.println("Hello, Ukraine!");
    myFile.close();
    Serial.println("Done writing to file.");
  } else {
    Serial.println("Error opening test.txt");
  }
}

void loop() {
  // Nothing here
}
```
Виведення:
```
Done writing to file.
```

## Поглиблений огляд:
Запис файлів на SD-карти в Arduino почався з бібліотеки SD.h. Є альтернативи, наприклад, SPIFFS або LittleFS для ESP8266/ESP32. Важливо розуміти швидкість запису і правильно вибрати бібліотеку для вашого проекту.

## Дивіться також:
- Документація SD бібліотеки: https://www.arduino.cc/en/Reference/SD.
- Модулі пам'яті для Arduino: https://create.arduino.cc/projecthub/projects/tags/memory.
- Порадник з SPIFFS для ESP8266: https://randomnerdtutorials.com/esp8266-nodemcu-spiffs.
