---
title:                "Виділення підрядків"
date:                  2024-01-20T17:45:23.756554-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? / Що й Навіщо?

Витягування підрядків - це процес виділення частин з більшого рядка. Програмісти роблять це для аналізу даних, валідації введення чи формування нових рядків.

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
