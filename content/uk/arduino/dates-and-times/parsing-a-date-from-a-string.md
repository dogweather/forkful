---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:34.493444-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041F\u0440\u044F\u043C\u0438\u0439 \u043F\u0456\u0434\u0445\u0456\u0434 \u0431\
  \u0435\u0437 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u044C\u043E\u0457 \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438."
lastmod: '2024-03-13T22:44:49.734106-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0440\u044F\u043C\u0438\u0439 \u043F\u0456\u0434\u0445\u0456\u0434\
  \ \u0431\u0435\u0437 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u044C\u043E\u0457\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438."
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
weight: 30
---

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
