---
title:                "Arduino: Створення тимчасового файлу."
simple_title:         "Створення тимчасового файлу."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Хартия - це важлива частина програмування на Arduino, тому що вона дозволяє зберігати тимчасові дані під час виконання коду. Це особливо корисно, коли вам потрібно зберегти дані для подальшої обробки або виводу на екран.

## Як створити тимчасовий файл

Для створення тимчасового файлу в Arduino, вам потрібно використовувати клас "File" з бібліотеки SD. Ось приклад коду, де ми створюємо тимчасовий файл з ім'ям "temp.txt":

```Arduino
#include <SPI.h>
#include <SD.h>

File dataFile;

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(10)) {
    Serial.println("Unable to initialize SD card.");
    while (1);
  }
  
  dataFile = SD.open("temp.txt", FILE_WRITE);
  if (dataFile) {
    Serial.println("Temp file created.");
  } else {
    Serial.println("Error creating temp file.");
  }

}

void loop() {
  // Write data to temp file
  dataFile.println("This is a temporary file");
  dataFile.println("Created with Arduino");
  dataFile.close();
  
  // Print temp file contents to serial monitor
  dataFile = SD.open("temp.txt", FILE_READ);
  if (dataFile) {
    while (dataFile.available()) {
      Serial.write(dataFile.read());
    }
    dataFile.close();
  } else {
    Serial.println("Error opening temp file.");
  }
  
  delay(1000);
}
```

У цьому прикладі ми використовуємо функцію "open" для створення тимчасового файлу з ім'ям "temp.txt" і потім записуємо дані в нього за допомогою функції "println". Щоб закрити файл, використовується функція "close". Після цього, ми знову відкриваємо файл для зчитування і виводимо його вміст на монітор серійного порту. Ви можете змінити дані в тимчасовому файлі або створити новий кожного разу, коли запускається цикл "loop".

## Глибокий поринання

Надійна робота з тимчасовими файлами - це важлива частина програмування на Arduino. Однак, важливо пам'ятати, що мікроконтролери, такі як Arduino Uno, мають обмежену кількість оперативної пам'яті. Тому, коли ви працюєте з тимчасовими файлами, важливо враховувати розмір і кількість файлів. Також, не забувайте закривати файл після використання, щоб зберегти пам'ять.

## Дивіться також

- [Oficyna do programowania Arduino](https://www.arduino.cc/en/Reference/SD)
- [Створення тимчасового файлу в Arduino](https://create.arduino.cc/projecthub/ivsevolodov/creating-a-temporary-file-with-arduino-0ea68b)