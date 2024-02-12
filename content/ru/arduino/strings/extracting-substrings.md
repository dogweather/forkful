---
title:                "Извлечение подстрок"
aliases:
- /ru/arduino/extracting-substrings.md
date:                  2024-01-28T23:57:51.658921-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Извлечение подстрок означает выделение определённых частей строки - подобно тому как если бы вы взяли кусочек пирога. Программисты делают это для изоляции данных, очистки ввода или манипуляции текстом, например, для анализа сообщений от датчиков.

## Как это сделать:

Строки в Arduino могут быть разделены на части с помощью `substring()`:

```arduino
void setup() {
  Serial.begin(9600);
  String phrase = "Hello, Arduino World!";
  String greeting = phrase.substring(0, 5);
  String location = phrase.substring(7, 19);
  
  Serial.println(greeting); // Выводит "Hello"
  Serial.println(location); // Выводит "Arduino World"
}

void loop() {
  // Здесь ничего не повторяется.
}
```

Вывод в Serial Monitor:
```
Hello
Arduino World
```

## Подробнее

До появления Arduino программисты использовали массивы символов и функции вроде `strncpy` в C. Эти инструменты не являются просто историческими реликвиями, они все еще используются для работы на низком уровне. Функция `substring()` в Arduino, на самом деле, является обёрткой, которая упрощает работу со строковыми объектами. Но будьте внимательны, использование `String` может привести к фрагментации памяти. Если стабильность критически важна, особенно в долгосрочных или сложных программах, рассмотрите старые методы с массивами символов `char`.

Альтернативы `substring()` включают в себя прямую манипуляцию с массивами символов или функции вроде `strtok()`. Эти методы могут быть более эффективными, но могут потребовать больше кода для управления.

Под капотом, `substring()` создаёт новый объект String, содержащий символы от начального индекса до индекса, предшествующего конечному, который можно опустить, если вы хотите получить все до конца.

## Смотрите также:

- Справочник по строкам Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Управление памятью в Arduino: https://learn.arduino.cc/programming/variables-and-data-types/memory-management
- Метод substr `std::string` в C++ для сравнения: http://www.cplusplus.com/reference/string/string/substr/
