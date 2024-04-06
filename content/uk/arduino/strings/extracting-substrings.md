---
date: 2024-01-20 17:45:23.756554-07:00
description: "How to / \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: \u041E\u0431\u0433\u043E\u0440\u0442\u0430\u043D\u043D\u044F \u043A\u043E\
  \u0434\u0443 `data.substring(7, 14)` \u0442\u0430 `data.substring(7)` \u0437\u0430\
  \u0431\u0435\u0437\u043F\u0435\u0447\u0443\u044E\u0442\u044C \u0432\u0438\u0442\u044F\
  \u0433\u0443\u0432\u0430\u043D\u043D\u044F \u043F\u0456\u0434\u0440\u044F\u0434\u043A\
  \u0456\u0432."
lastmod: '2024-04-05T21:53:49.858528-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0431\u0433\u043E\u0440\u0442\u0430\u043D\u043D\u044F \u043A\u043E\
  \u0434\u0443 `data.substring(7, 14)` \u0442\u0430 `data.substring(7)` \u0437\u0430\
  \u0431\u0435\u0437\u043F\u0435\u0447\u0443\u044E\u0442\u044C \u0432\u0438\u0442\u044F\
  \u0433\u0443\u0432\u0430\u043D\u043D\u044F \u043F\u0456\u0434\u0440\u044F\u0434\u043A\
  \u0456\u0432."
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

## How to / Як це зробити:
```Arduino
String data = "Привіт, Arduino!";

// Витяг 7 символів, починаючи з 8-ї позиції
String greeting = data.substring(7, 14);

// Витягнути до кінця рядка, починаючи з 8-ї позиції
String arduinoPart = data.substring(7);

void setup() {
  Serial.begin(9600);
  
  Serial.println(greeting);   // Output: Arduino
  Serial.println(arduinoPart); // Output: Arduino!
}

void loop() {
  // тут коду не потрібно
}
```
Обгортання коду `data.substring(7, 14)` та `data.substring(7)` забезпечують витягування підрядків.

## Deep Dive / Занурення:
Історично, робота з рядками завжди була ключовою частиною програмування, так як обробка текстової інформації є дуже частою задачею. Функція `substring` існує у багатьох мовах програмування, і в Arduino вона дозволяє легко маніпулювати `String` об'єктами. 

Альтернативи в Arduino — це використання char масивів і функцій як `strncpy` з бібліотеки `<cstring>`, але це вимагає більш глибокого розуміння C++ і управління пам'яттю.

Внутрішні механізми використання `substring` на Arduino схожі на інші мови: метод працює шляхом копіювання обраної частини рядка у новий об'єкт `String`. Важливо знати, що створення багатьох `String` може призвести до фрагментації пам'яті, особливо на пристроях з обмеженим ресурсом пам’яті як Arduino.

## See Also / Дивіться також:
- [String Object in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino String Manipulation](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator)
