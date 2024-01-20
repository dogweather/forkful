---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Створення тимчасового файлу - це процес, в якому програма створює файл з даними, які вона може використовувати і видаляти, коли вони вже не потрібні. Програмісти роблять це, щоб зберегти проміжний результат або використовувати багато даних без засмічення пам'яті.

## Як це зробити:
Arduino не підтримує безпосереднього створення тимчасових файлів, але ви можете використовувати SD карту або EEPROM для зберігання тимчасових даних. Ось приклад:

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  Serial.print("Initializing SD card...");

  if (!SD.begin(4)) {
    Serial.println("initialization failed!");
    return;
  }
  Serial.println("initialization done.");

  myFile = SD.open("test.txt", FILE_WRITE);
  
  if (myFile) {
    Serial.print("Writing to test.txt...");
    myFile.println("Testing data storage");
    myFile.close();
    Serial.println("done.");
  } else {
    Serial.println("error opening test.txt");
  }
}

void loop() {
  
}
 ```
Вивід в моніторі порту:
```
Initializing SD card...initialization done.
Writing to test.txt...done.
```

## Занурення в деталі
Arduino - це відкрита платформа, яка вийшла в 2005 році, для створення одноплатових комп'ютерів та мікроконтролерів. Проте Arduino не була розроблена з метою обробки великого обсягу даних або розробки складних алгоритмів. Тому вона не має вбудованої підтримки роботи з файлами як у більш потужних системах. 

Альтернатива на Arduino - використання EEPROM або використання зовнішніх пристроїв зберігання, таких як SD карти. EEPROM має обмежений цикл запису, тому використання SD-карт може бути кращим вибором для деяких прикладів.

## Додатково
Зверніться до офіційної документації Arduino, щоб дізнатися більше про [зберігання даних на SD карті](https://www.arduino.cc/en/Reference/SD), а також про [роботу з EEPROM](https://www.arduino.cc/en/Reference/EEPROM).