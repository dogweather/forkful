---
title:                "Видалення символів за візерунком"
date:                  2024-01-20T17:41:37.310879-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що і Чому?
У кодуванні видалення символів за патерном означає вирізання певних елементів з рядка. Це роблять, аби очистити дані, форматувати текст чи видалити непотрібні частини.

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