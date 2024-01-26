---
title:                "Перевірка наявності директорії"
html_title:           "Bash: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Перевірка наявності каталогу – це процес визначення, чи існує певна папка в системі файлів вашої SD карти, підключеної до Arduino. Це корисно, коли потрібно забезпечити збереження даних у правильне місце або коли ви не хочете перезаписувати існуючі дані.

## Як це зробити:
```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // чекаємо підключення до порта
  }

  if (!SD.begin(4)) {
    Serial.println("Помилка ініціалізації SD");
    return;
  }

  File root = SD.open("/");
  if (root.isDirectory()) {
    Serial.println("Кореневий каталог існує");
  } else {
    Serial.println("Кореневий каталог не знайдено");
  }
  
  char dirName[] = "/exampleDir";
  if (SD.exists(dirName)) {
    Serial.println("Каталог exampleDir існує");
  } else {
    Serial.println("Каталог exampleDir не знайдено");
  }
}

void loop() {
  // нічого не робимо
}
```
Вивід:
```
Кореневий каталог існує
Каталог exampleDir існує
```
або
```
Кореневий каталог існує
Каталог exampleDir не знайдено
```

## Дослідження в глибину
Arduino не має вбудованої операційної системи, тому перевірка наявності каталогу відрізняється від тієї, яку ви б зробили на комп'ютері. Функція `SD.exists()` використовується для перевірки наявності файлу чи каталогу. Бібліотека SD для Arduino працює за моделлю FAT16/32, що дозволяє взаємодіяти з SD-картами. Історично, Arduino розширило можливості мікроконтролерів за рахунок зручної інтеграції з різноманітними модулями, включно з модулями зберігання даних. Використання SD-карт додає гнучкість у зберігання файлів.

Частою альтернативою є створення каталогу, якщо він не існує – це можна зробити з допомогою `SD.mkdir(dirName)`.

## Дивіться також:
- Документація бібліотеки SD: https://www.arduino.cc/en/Reference/SD 
- Форматування SD-карт у FAT16/32: https://www.sdcard.org/downloads/formatter/
- Робота з файловою системою на Arduino: https://www.arduino.cc/en/Guide/Environment#toc8
