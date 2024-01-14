---
title:    "Arduino: Створення тимчасового файлу"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Чому

Створення тимчасового файлу є важливим елементом в програмуванні Arduino. Це дає можливість зберігати тимчасові дані, що можуть бути використані для подальшого використання чи аналізу.

##Як

Завдяки Arduino ви можете легко створити тимчасовий файл, використовуючи функцію `File` та команду `open()`. Приклад коду виглядає наступним чином:

```Arduino
#include <SD.h>

File tempFile;

void setup() {
  SD.begin(10); // Ініціалізуємо SD карту
  tempFile = SD.open("temp.txt", FILE_WRITE); // Створюємо тимчасовий файл під назвою "temp.txt"
  tempFile.println("Це тимчасові дані для подальшої роботи."); // Записуємо дані у файл
  tempFile.close(); // Закриваємо файл
}

void loop() {
 // Можна використовувати дані з тимчасового файлу тут
}
```

В результаті, у вашому SD картці з'явиться файл з назвою "temp.txt", який міститиме рядок тексту "Це тимчасові дані для подальшої роботи.".

##Глибокий занурення

Створення тимчасових файлів також може бути корисним для збереження даних з сенсорів або іншої периферії, яка потребує зберігання тимчасових даних. У разі, якщо ви бажаєте перезаписати файл, ви можете використовувати команду `SD.open("temp.txt", FILE_WRITE)` замість `SD.open("temp.txt", FILE_READ)`.

##Дивіться також

- [Функція File на Arduino офіційній документації](https://www.arduino.cc/en/Reference/File)
- [Приклад використання тимчасового файлу на Arduino](https://www.arduino.cc/en/Tutorial/FileWrite)
- [Поради та процес створення тимчасових файлів на Arduino](https://www.allaboutcircuits.com/projects/create-temporary-files-using-an-arduino/)