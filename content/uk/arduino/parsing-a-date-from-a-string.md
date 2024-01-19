---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Парсинг дати з рядка - це процес вилучення інформації про дату з текстового рядка. Програмісти роблять це, щоб взаємодіяти з датами в коді - виконувати оператори порівняння, арифметичні операції тощо.

## Як це робиться:

```Arduino
// Припустимо, у нас є рядок у форматі «dd.mm.yyyy»
String str_date = "31.12.2020";

// Розділяємо стрічку на частини
int day = str_date.substring(0, 2).toInt();
int month = str_date.substring(3, 5).toInt();
int year = str_date.substring(6, 10).toInt();

// Виводимо значення
Serial.print("День: ");
Serial.println(day);
Serial.print("Місяць: ");
Serial.println(month);
Serial.print("Рік: ");
Serial.println(year);
```

## Занурення:

**Історичний контекст**: У минулому, коли ресурси були обмежені, парсинг дат був важливим аспектом оптимізації коду. 

**Альтернативи**: Замість цього, ви можете використовувати бібліотеки, що забезпечують можливість парсингу дати, такі як TimeLib у випадку Arduino. 

**Деталі реалізації**: Пам'ятайте, що Arduino не має вбудованих функцій для роботи з датами. Парсинг дати і часу з рядка - це простий спосіб обійти цю проблему.

## Дивіться також:

1. Офіційна документація Arduino: https://www.arduino.cc/reference/en/
2. TimeLib, бібліотека для роботи з датами і часом на Arduino: https://www.arduinolibraries.info/libraries/time
3. Порядок роботи з рядками на Arduino: https://create.arduino.cc/projecthub/iot_lover/string-handling-by-arduino-string-character-array-93b68f