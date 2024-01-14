---
title:                "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому
Написання текстових файлів є важливою частиною програмування Arduino, оскільки це дає можливість зберігати інформацію та використовувати її у подальших проектах.

## Як
Написання текстових файлів на Arduino можна виконати за допомогою наступного коду:

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // ініціалізація SD картки
  Serial.begin(9600);
  while (!Serial) {
    ; // чекаємо з'єднання з монітором серійного порту
  }

  Serial.print("Ініціалізація SD картки...");
  if (!SD.begin(4)) {
    Serial.println("Не вдалося ініціалізувати SD картку.");
    while (1);
  }
  Serial.println("готово!");

  // записуємо у текстовий файл
  myFile = SD.open("test.txt", FILE_WRITE);

  // пишемо у файл
  myFile.println("Привіт, це текстовий файл!");
  myFile.close();

  // читаємо з файлу
  myFile = SD.open("test.txt");
  if (myFile) {
    Serial.println("Текст з файлу:");
    
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Не вдалося відкрити файл.");
  }
}

void loop() {
  // нічого не робимо
}
```

Ви можете замінювати текст у функції `myFile.println` для записування різного виду інформації.

Ви також можете використовувати команду `Serial.write` для відображення тексту прямо на моніторі серійного порту.

## Поглиблене вивчення
При написанні текстових файлів на Arduino, важливо враховувати, що SD картка повинна бути ініціалізована перед використанням. Також варто звернути увагу на розташування SD картки та використовувати правильні команди для запису та читання файлів.

## Дивіться також
- [Офіційна документація з роботи з SD карткою на Arduino](https://www.arduino.cc/en/Reference/SD)
- [Приклад записування та читання текстових файлів на Arduino](https://randomnerdtutorials.com/arduino-data-logger-write-read-sd-card/)
- [Відео урок з використання SD картки на Arduino](https://www.youtube.com/watch?v=SAXkGeNdl6s)