---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:58.562033-07:00
description: "\u041E\u043F\u0440\u0435\u0434\u0435\u043B\u0435\u043D\u0438\u0435 \u0434\
  \u043B\u0438\u043D\u044B \u0441\u0442\u0440\u043E\u043A\u0438 \u043E\u0437\u043D\
  \u0430\u0447\u0430\u0435\u0442 \u0432\u044B\u0447\u0438\u0441\u043B\u0435\u043D\u0438\
  \u0435 \u043A\u043E\u043B\u0438\u0447\u0435\u0441\u0442\u0432\u0430 \u0441\u0438\
  \u043C\u0432\u043E\u043B\u043E\u0432, \u043A\u043E\u0442\u043E\u0440\u044B\u0435\
  \ \u043E\u043D\u0430 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u0442. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\
  \u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043F\u0440\u043E\u0432\u0435\
  \u0440\u043A\u0438 \u0432\u0445\u043E\u0434\u043D\u044B\u0445 \u0434\u0430\u043D\
  \u043D\u044B\u0445, \u043F\u0435\u0440\u0435\u0431\u043E\u0440\u0430\u2026"
lastmod: '2024-03-13T22:44:45.513457-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u043F\u0440\u0435\u0434\u0435\u043B\u0435\u043D\u0438\u0435 \u0434\
  \u043B\u0438\u043D\u044B \u0441\u0442\u0440\u043E\u043A\u0438 \u043E\u0437\u043D\
  \u0430\u0447\u0430\u0435\u0442 \u0432\u044B\u0447\u0438\u0441\u043B\u0435\u043D\u0438\
  \u0435 \u043A\u043E\u043B\u0438\u0447\u0435\u0441\u0442\u0432\u0430 \u0441\u0438\
  \u043C\u0432\u043E\u043B\u043E\u0432, \u043A\u043E\u0442\u043E\u0440\u044B\u0435\
  \ \u043E\u043D\u0430 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u0442. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\
  \u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043F\u0440\u043E\u0432\u0435\
  \u0440\u043A\u0438 \u0432\u0445\u043E\u0434\u043D\u044B\u0445 \u0434\u0430\u043D\
  \u043D\u044B\u0445, \u043F\u0435\u0440\u0435\u0431\u043E\u0440\u0430\u2026"
title: "\u041F\u043E\u0438\u0441\u043A \u0434\u043B\u0438\u043D\u044B \u0441\u0442\
  \u0440\u043E\u043A\u0438"
---

{{< edit_this_page >}}

## Что и Зачем?
Определение длины строки означает вычисление количества символов, которые она содержит. Программисты делают это для проверки входных данных, перебора символов, выравнивания текста или динамического управления данными.

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
