---
date: 2024-01-20 17:54:06.209017-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0414\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\
  \u0441\u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0439\u043C\u043E `SD`\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443, \u044F\u043A\u0443\
  \ \u0442\u0440\u0435\u0431\u0430 \u0441\u043F\u0435\u0440\u0448\u0443 \u043F\u0456\
  \u0434\u043A\u043B\u044E\u0447\u0438\u0442\u0438."
lastmod: '2024-03-13T22:44:49.797564-06:00'
model: gpt-4-1106-preview
summary: "\u0414\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\
  \u043A\u0441\u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0439\u043C\
  \u043E `SD` \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443, \u044F\
  \u043A\u0443 \u0442\u0440\u0435\u0431\u0430 \u0441\u043F\u0435\u0440\u0448\u0443\
  \ \u043F\u0456\u0434\u043A\u043B\u044E\u0447\u0438\u0442\u0438."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

## Як це зробити:
Для читання текстового файлу використовуймо `SD` бібліотеку, яку треба спершу підключити.

```Arduino
#include <SD.h>
File myFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // чекайте поки не запуститься серіалний порт
  }

  if (!SD.begin(4)) {
    Serial.println("Помилка ініціалізації SD");
    return;
  }

  myFile = SD.open("test.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Помилка читання файлу");
  }
}

void loop() {
  // тут ми залишаємо пусто, тому що читання відбувається один раз при запуску
}
```

Запуск такої програми виведе вміст файлу `test.txt` через серіалний порт.

## Поглиблений Розбір
У 2005 році Arduino Project запропонував легкий доступ до мікроконтролерів, в тому числі й для роботи з файловою системою. Сьогодні читання текстових файлів – звичайна практика, альтернативи – SPIFFS для ESP8266 або LittleFS для новіших модулів. Читання файлів потребує контролер з SD-картки чи внутрішньої пам'яті. Процес включає відкриття файлу, читання його змісту та закриття після завершення. Пам'ятайте про обмеження по пам'яті та часу доступу до пам'яті під час роботи з великими файлами.

## Дивіться Також
- Офіційна документація по `SD` бібліотеці - [Arduino SD Library](https://www.arduino.cc/en/Reference/SD)
- Порадник як підключити SD-картку до Arduino - [Connecting the SD card to Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)
- Інструкції до `SPIFFS` та `LittleFS` - [ESP8266FileSystemPlugin](https://github.com/esp8266/arduino-esp8266fs-plugin), [Arduino ESP32 Filesystem Upload Guide](https://randomnerdtutorials.com/install-esp32-filesystem-uploader-arduino-ide/)
