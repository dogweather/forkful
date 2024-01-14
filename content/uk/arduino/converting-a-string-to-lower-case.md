---
title:    "Arduino: Перетворення рядка у нижній регістр"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Відображення тексту в нижньому регістрі є корисною функцією у багатьох додатках, таких як перевірка орфографії або порівняння рядків. Використання Arduino у цих додатках може додати функціональності та зробити їх управління більш зручним.

## Як

Для перетворення строки в нижній регістр у мові програмування Arduino можна використовувати функцію `toLowerCase()`. Наприклад, для перетворення рядка `HELLO WORLD` в нижній регістр, потрібно використати такий код:

```Arduino
String upperCaseString = "HELLO WORLD";
String lowerCaseString = upperCaseString.toLowerCase();
Serial.println(lowerCaseString); // Output: hello world
```

## Глибоке занурення

Коли рядок перетворюється у нижній регістр, всі символи, які не є літерами англійського алфавіту, будуть залишені без змін. Також, використання функції `toLowerCase()` може зберігати оригінальний рядок і повертати новий рядок зі зміненим регістром, тому це не впливатиме на оригінальну змінну.

## Дивіться також

- [Мануал з офіційного сайту Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/tolowercase/)
- [Приклад застосування функції toLowerCase()](https://www.tutorialspoint.com/arduino/arduino_string_tolowercase.htm)