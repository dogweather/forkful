---
title:                "Читання текстового файлу"
date:                  2024-01-20T17:54:06.209017-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і Чому?
Читання текстового файлу — це процес збору даних з файлу, збереженого на пристрої для зберігання, як от SD-картка чи внутрішня пам'ять. Програмісти це роблять, щоби програма могла отримати вхідні дані, конфігурацію чи зразки тексту без необхідності жорсткого кодування інформації в скетчі.

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
