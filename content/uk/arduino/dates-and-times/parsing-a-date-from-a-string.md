---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:34.493444-07:00
description: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437\
  \ \u0440\u044F\u0434\u043A\u0430 \u0432 Arduino \u043F\u043E\u043B\u044F\u0433\u0430\
  \u0454 \u0443 \u0432\u0438\u0442\u044F\u0433\u0443\u0432\u0430\u043D\u043D\u0456\
  \ \u0442\u0430 \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\
  \u0456 \u043A\u043E\u043C\u043F\u043E\u043D\u0435\u043D\u0442\u0456\u0432 \u0434\
  \u0430\u0442\u0438 (\u0440\u0456\u043A, \u043C\u0456\u0441\u044F\u0446\u044C, \u0434\
  \u0435\u043D\u044C) \u0437 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u043E\u0433\
  \u043E \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ \u0443 \u0444\u043E\u0440\u043C\u0430\u0442, \u044F\u043A\u0438\u0439 \u043C\u043E\
  \u0436\u043D\u0430\u2026"
lastmod: '2024-03-13T22:44:49.734106-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430 \u0432 Arduino \u043F\u043E\u043B\u044F\u0433\u0430\u0454\
  \ \u0443 \u0432\u0438\u0442\u044F\u0433\u0443\u0432\u0430\u043D\u043D\u0456 \u0442\
  \u0430 \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u0456\
  \ \u043A\u043E\u043C\u043F\u043E\u043D\u0435\u043D\u0442\u0456\u0432 \u0434\u0430\
  \u0442\u0438 (\u0440\u0456\u043A, \u043C\u0456\u0441\u044F\u0446\u044C, \u0434\u0435\
  \u043D\u044C) \u0437 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u043E\u0433\u043E\
  \ \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ \u0443 \u0444\u043E\u0440\u043C\u0430\u0442, \u044F\u043A\u0438\u0439 \u043C\u043E\
  \u0436\u043D\u0430\u2026"
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## Що і чому?

Розбір дати з рядка в Arduino полягає у витягуванні та перетворенні компонентів дати (рік, місяць, день) з текстового представлення у формат, який можна використовувати для ведення часу, порівнянь або маніпуляцій в скетчах. Програмісти часто виконують це завдання для взаємодії з компонентами, такими як годинники в реальному часі, логери, або для обробки вхідних даних з веб-API та інтерфейсів користувача, де дати можуть бути представлені в зрозумілому форматі.

## Як це зробити:

Прямий підхід без сторонньої бібліотеки:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Приклад рядка дати у форматі РРРР-ММ-ДД
  String dateString = "2023-04-01";

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Ініціалізація об'єкта DateTime з розібраними компонентами
  DateTime parsedDate(year, month, day);
  
  Serial.print("Розібрана Дата: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Приклад виводу:
```
Розібрана Дата: 2023/4/1
```

Використання сторонньої бібліотеки (*ArduinoJson* для більш складних сценаріїв розбору, наприклад отримання дати з відповіді JSON):

Спочатку встановіть бібліотеку ArduinoJson через Менеджер бібліотек Arduino.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Симуляція відповіді JSON
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Витягування рядка дати
  const char* date = doc["date"];

  // Розбір дати з рядка як і раніше
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Розібрана Дата з JSON: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Приклад виводу:
```
Розібрана Дата з JSON: 2023/7/19
```
