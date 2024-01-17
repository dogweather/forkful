---
title:                "Читання текстового файлу."
html_title:           "Arduino: Читання текстового файлу."
simple_title:         "Читання текстового файлу."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що це таке і чому це роблять програмісти?
Читання текстового файлу - це процес отримання інформації з файлу, який зберігає текстову інформацію. Це важлива частина програмування, оскільки дозволяє програмі приймати дані зовнішніх джерел, таких як користувачі або інші програми.

## Як це зробити:
Arduino може читати текстові файли за допомогою функції "SD.open()". Потім можна прочитати дані з файлу за допомогою функції "readString()". Наприклад:

```Arduino
File myFile = SD.open("example.txt"); // відкрити файл "example.txt" на карті SD
String data = myFile.readString(); // прочитати дані з файлу у змінну "data"
Serial.println(data); // вивести дані на монітор
```

Також можна використовувати "while" цикл для читання файлу по одному рядку за раз:

```Arduino
while (myFile.available()) { // поки є дані у файлі
  String line = myFile.readStringUntil('\n'); // прочитати рядок до знаку нового рядка
  Serial.println(line); // вивести рядок на монітор
}
```

## Заглиблення:
Читання текстового файлу є одним з основних способів отримання даних у програмуванні. Але можна також використовувати інші методи, такі як використання баз даних або з'єднання з Інтернетом. Реалізація читання файлів може варіюватися залежно від типу карти пам'яті та формату файлу.

## Дивись також:
- Офіційна документація з функції "SD.open()": https://www.arduino.cc/reference/en/libraries/sd-card-library/open/
- Інші способи отримання даних у Arduino: https://maker.pro/arduino/tutorial/how-to-create-and-read-a-text-file-using-arduino