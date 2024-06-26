---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:51.658921-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0421\u0442\u0440\u043E\u043A\u0438 \u0432 Arduino \u043C\u043E\u0433\
  \u0443\u0442 \u0431\u044B\u0442\u044C \u0440\u0430\u0437\u0434\u0435\u043B\u0435\
  \u043D\u044B \u043D\u0430 \u0447\u0430\u0441\u0442\u0438 \u0441 \u043F\u043E\u043C\
  \u043E\u0449\u044C\u044E `substring()`."
lastmod: '2024-03-13T22:44:45.509544-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0442\u0440\u043E\u043A\u0438 \u0432 Arduino \u043C\u043E\u0433\u0443\
  \u0442 \u0431\u044B\u0442\u044C \u0440\u0430\u0437\u0434\u0435\u043B\u0435\u043D\
  \u044B \u043D\u0430 \u0447\u0430\u0441\u0442\u0438 \u0441 \u043F\u043E\u043C\u043E\
  \u0449\u044C\u044E `substring()`."
title: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\u0434\
  \u0441\u0442\u0440\u043E\u043A"
weight: 6
---

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
