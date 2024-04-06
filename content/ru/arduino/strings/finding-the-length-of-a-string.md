---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:58.562033-07:00
description: "\u041A\u0430\u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C: \u041F\
  \u0440\u0438\u043C\u0435\u0440 \u0432\u044B\u0432\u043E\u0434\u0430."
lastmod: '2024-04-05T21:53:45.918803-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u041F\u043E\u0438\u0441\u043A \u0434\u043B\u0438\u043D\u044B \u0441\u0442\
  \u0440\u043E\u043A\u0438"
weight: 7
---

## Как сделать:
```Arduino
void setup() {
  Serial.begin(9600); // Начало серийной связи
  String myString = "Hello, Arduino!"; // Ваша строка здесь
  int stringLength = myString.length(); // Определение длины строки
  Serial.print("Длина строки составляет: ");
  Serial.println(stringLength); // Выводит длину
}

void loop() {
  // Здесь делать нечего.
}
```
Пример вывода:
```
Длина строки составляет: 15
```

## Погружение в детали
В прошлом С-программисты использовали функцию `strlen()` из `<string.h>`, считая символы до нуль-терминатора. В мире Arduino класс `String` упрощает жизнь благодаря встроенному методу `length()`. Но помните, использование объектов `String` может со временем фрагментировать ограниченную память устройства. Альтернатива? Использовать массивы char (C-строки), которые более дружественны к памяти, но их сложнее обрабатывать.

Для крупных проектов всегда учитывайте управление памятью. С методом `length()` дополнительные вычисления не нужны — объект `String` отслеживает свой размер. Внутренне `length()` — это быстрый поиск, а не подсчет символов. Это эффективно! Но если у вас мало памяти, вернитесь к основам с массивами char и ручными расчетами длины, как в старые добрые времена с `strlen()`.

## Смотрите также
- Справочник по `String` в Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Функция `strlen()` для C-строк в Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/
- Обсуждение `String` против массива char в Arduino: https://forum.arduino.cc/t/string-vs-char-array/678207
