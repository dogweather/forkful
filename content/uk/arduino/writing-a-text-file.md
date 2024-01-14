---
title:                "Arduino: Написання текстового файлу"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому
Написання текстового файлу є важливою складовою програмування на Arduino для того, щоб зберегти і обробити дані, отримані з датчиків або від користувача.

## Як
Написання текстового файлу на Arduino просто здійснюється за допомогою використання функцій `Serial` та `SD` бібліотеки. Приклад коду та його вивід показані нижче.

```Arduino
#include <SD.h>

File dataFile;

void setup() {
  // ініціалізація Serial порту
  Serial.begin(9600);

  // ініціалізація SD карти
  if (!SD.begin(10)) {
    Serial.println("Помилка ініціалізації SD карти.");
    while (1);
  }

  // відкриття файлу для запису даних
  dataFile = SD.open("dane.txt", FILE_WRITE);
}

void loop() {
  // зчитування даних з датчика або від користувача
  int data = analogRead(A0);

  // запис даних у файл
  dataFile.println(data);

  // вивід даних в Serial порт
  Serial.println("Записано: " + String(data));

  delay(1000);
}

```

Вивід:

```
Записано: 123
Записано: 234
Записано: 345
Записано: 456
...
```

## Глибше вдивлення
У випадках, коли необхідно записати більш складні дані, наприклад текстові повідомлення або числа з плаваючою крапкою, можна використати функцію `sprintf` для форматування даних перед записом у файл. Наприклад:

```Arduino
// приклад з тектовим повідомленням та числом з плаваючою крапкою
char message[] = "Привіт, це значення напруги: %.2f V";
float voltage = 5.75;

// форматування повідомлення з вставкою значення напруги
sprintf(message, message, voltage);

// запис повідомлення у файл
dataFile.println(message);
```

## Дивіться також
- [SD бібліотека Arduino](https://www.arduino.cc/en/reference/SD)
- [sprintf() функція Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/sprintf/)