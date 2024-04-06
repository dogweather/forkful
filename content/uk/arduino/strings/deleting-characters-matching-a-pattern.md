---
date: 2024-01-20 17:41:37.310879-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0424\u0443\u043D\u043A\u0446\u0456\u044F \u0432\u0438\u0434\u0430\u043B\u0435\
  \u043D\u043D\u044F \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0437\u0443\
  \u0441\u0442\u0440\u0456\u0447\u0430\u0454\u0442\u044C\u0441\u044F \u043C\u0430\u0439\
  \u0436\u0435 \u0432 \u043A\u043E\u0436\u043D\u0456\u0439 \u043C\u043E\u0432\u0456\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\u044F\
  . \u0423 \u043C\u043E\u0432\u0456 \u0421/\u0421++, \u043D\u0430 \u043E\u0441\u043D\
  \u043E\u0432\u0456 \u044F\u043A\u043E\u0457 \u043F\u043E\u0431\u0443\u0434\u043E\
  \u0432\u0430\u043D\u043E Arduino, \u0434\u043B\u044F \u0446\u044C\u043E\u0433\u043E\
  \u2026"
lastmod: '2024-04-05T22:51:02.702243-06:00'
model: gpt-4-1106-preview
summary: "\u0424\u0443\u043D\u043A\u0446\u0456\u044F \u0432\u0438\u0434\u0430\u043B\
  \u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0437\
  \u0443\u0441\u0442\u0440\u0456\u0447\u0430\u0454\u0442\u044C\u0441\u044F \u043C\u0430\
  \u0439\u0436\u0435 \u0432 \u043A\u043E\u0436\u043D\u0456\u0439 \u043C\u043E\u0432\
  \u0456 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\u044F\
  ."
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

## Як це зробити:
```Arduino
String text = "Hello, World! 123";
String pattern = "123";

void setup() {
  Serial.begin(9600);
  Serial.println("Before: " + text);
  text.remove(text.indexOf(pattern), pattern.length());
  Serial.println("After: " + text);
}

void loop() {
  // nothing to loop over
}
```
Вивід буде наступним:
```
Before: Hello, World! 123
After: Hello, World!
```

## Поглиблений Розділ
Функція видалення символів зустрічається майже в кожній мові програмування. У мові С/С++, на основі якої побудовано Arduino, для цього використовують `string` або `char` масиви. 

Говорячи про Arduino, метод `remove()` є частиною класу `String`. Під капотом, він змінює `buffer` об'єкту `String`, виключаючи з нього визначений шматок. Пошук по масиву символів і видалення вручну також доступний для більш низькорівневих операцій.

Альтернативи методу `remove()` включають функції як `strtok()` та `strstr()` в С, які можна використовувати для більш складних задач парсингу і маніпуляції строками.

## Дивіться Також
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [C++ String Manipulation](http://www.cplusplus.com/reference/string/string/)
- [C Character String Processing](https://en.cppreference.com/w/c/string/byte)
