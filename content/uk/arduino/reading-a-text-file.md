---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що & навіщо?

Читання текстового файлу - це процес зчитування інформації з файлу у текстовому форматі. Програмісти цим займаються для обробки та аналізу даних або ж для відображення цих даних в користувацькому інтерфейсі.

## Як це робити:

Для читання текстових файлів у Arduino вам знадобиться SD картка і модуль читача SD карт. Послідовності команд можуть виглядати так:

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  
  myFile = SD.open("test.txt");

  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Error opening file!");
  }
}

void loop() {
  // nothing happens after setup
}
```

## Поглиблено:

1) Arduino почав використовувати модуль читання SD карт у 2006 році, щоб забезпечити гнучкий спосіб зберігання та обробки даних не тільки в текстовому форматі.
2) Альтернативами є модулі EEPROM або SPI Flash, але вони зазвичай мають менший обсяг зберігання.
3) Arduino використовує бібліотеку SD для роботи з SD картами, і вона повертає дані у вигляді байт. Ви можете конвертувати ці байти в текстовий формат.

## Дивіться також:

1) [Детальніше про бібліотеку SD в Arduino](https://www.arduino.cc/en/reference/SD)
2) [Додаткові приклади із використанням SD карт в Arduino](https://arduinogetstarted.com/tutorials/arduino-sd-card)
3) [Навчальний курс по Arduino на YouTube](https://www.youtube.com/watch?v=VV9QcbiZg7g)