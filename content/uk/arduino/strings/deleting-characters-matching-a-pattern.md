---
date: 2024-01-20 17:41:37.310879-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:49.694558-06:00'
model: gpt-4-1106-preview
summary: .
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
