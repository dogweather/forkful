---
title:                "Arduino: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

**Чому:** Програмування мікроконтролерів Arduino може бути корисним для багатьох проектів, але чому б не додати до нього можливість опрацьовувати дані з CSV файлів? Це відкриває безліч можливостей для збереження та обробки даних.

**Як це зробити:** Для початку необхідно підключити бібліотеку "SD" та "SPI". Потім створюємо змінну для файлу, який ми хочемо прочитати. Далі нам потрібно створити об'єкт для роботи з CSV файлами. Для цього використовуємо функцію "SD.open()" та "SdFile::readCsv()". Після цього можна обробляти дані, використовуючи for цикл та розбивати їх на окремі стовпці за допомогою функції "tokenizer.tokenize()". Останнє, що потрібно зробити - це закрити об'єкт файлу за допомогою функції "file.close()" та вивести зчитані дані за допомогою "Serial.println()".

```Arduino
#include <SPI.h>
#include <SD.h>

File csvfile;
SdFile file;
SdFile::readCsv csv;

void setup() {
  Serial.begin(9600);
  // Підключення бібліотек
  if(!SD.begin(10)) {
    Serial.println("Помилка читання карти SD");
    return;
  }
  // Відкриття файлу
  csvfile = SD.open("data.csv");
  if(!csvfile) {
    Serial.println("Помилка відкриття файлу");
    return;
  }
  csv.begin(&csvfile);
}

void loop() {
  // Читання рядків з файлу та обробка даних
  while(csv.readRow() != 0) {
    int id = (int)csv.getCol(0).toInt();
    int temp = (int)csv.getCol(1).toFloat();
    int hum = (int)csv.getCol(2).toInt();
    
    // Виведення даних
    Serial.print("ID: ");
    Serial.print(id);
    Serial.print(", Температура: ");
    Serial.print(temp);
    Serial.print("°C, Вологість: ");
    Serial.print(hum);
    Serial.println("%");
  }
  // Закриття файлу
  csvfile.close();
  // Затримка на 10 секунд
  delay(10000);
}
```

**Глибше:** Для більшої гнучкості можна використовувати функцію "File.seek()" для переходу до певного рядка в файлі. Також, можна розширити функціонал, використовуючи бібліотеку "StringSplitter" для розбиття рядків на окремі значення.

*See Also:*
- [Official Arduino SD library](https://www.arduino.cc/en/Reference/SD)
- [String Splitter library](https://github.com/grexter/StringSplitter)
- [CSV File Format](https://en.wikipedia.org/wiki/Comma-separated_values)