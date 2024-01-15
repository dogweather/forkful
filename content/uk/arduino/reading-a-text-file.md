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

# Чому

Читання текстового файлу є важливою функцією в програмуванні на Arduino. Воно дозволяє отримати інформацію зовнішніх джерел, які можуть бути корисними для вашого проекту.

## Як це зробити

```arduino
#include <SD.h>  // підключення бібліотеки SD
File textFile;  // створення змінної для зберігання файлу

void setup() {
  Serial.begin(9600);  // ініціалізація зв'язку по Serial порту
  SD.begin(4);  // ініціалізація SD карти на піні 4

  // відкриття текстового файлу для читання
  textFile = SD.open("file.txt");
  if (textFile) {
    // читання та вивід інформації з файлу
    while (textFile.available()) {
      Serial.write(textFile.read());
    }
    // закриття файлу
    textFile.close();
  }
}

void loop() {
  // нічого не виконуємо в циклі loop
}
```

Вивід у моніторі Serial буде виглядати приблизно так:

```
Hello world!  // вміст файлу "file.txt"
```

## Поглиблене вивчення

Arduino має можливість читати текстові файли з SD карти або з комп'ютера, підключеного до входу USB. Для цього можна використовувати функції `SD.begin()` та `Serial.begin()` відповідно.

Крім того, можливо читати не тільки символи, а й цілі рядки за допомогою функцій `readString()` і `readStringUntil()`. За допомогою цих функцій можна читати певну кількість символів або читати до певного символу, наприклад, символу нового рядка.

# Дивіться також

- [Документація Arduino для функцій зчитування файлів](https://www.arduino.cc/en/Reference/FileReadWrite)
- [Приклад читання файлу з SD карти на Arduino](https://create.arduino.cc/projecthub/Aritro/reading-and-writing-text-file-for-sd-card-using-arduino-uno-0b946d)
- [Огляд функцій для роботи з текстовими файлами на Arduino](https://www.instructables.com/Arduino-Tutorial-SD-card-reading-and-writing-1/)