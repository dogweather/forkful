---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Що це таке і навіщо?
Конкатенація рядків це процес об'єднання двох або більше рядків у один. Програмісти використовують це для зосередження інформації в одному рядку та її подальшого використання.

# Як це робиться:
Отже, давайте подивимось на простий приклад коду, який демонструє, як ми можемо об'єднати рядки у Arduino.

```Arduino
String stringOne = "Hello";
String stringTwo = " World";
String stringThree = stringOne + stringTwo;
Serial.println(stringThree);
```
Внаслідок виконання цього коду, наш вихід буде: `Hello World`

# Глибше занурення:
Метод конкатенації рядків у Arduino базується на бібліотеці C++. Гісторично, ця функція була розроблена ще в перших версіях C++, але її доопрацювали й у Arduino для полегшення роботи зі строками. Конкатенування використовує швидку та ефективну оперативну пам'ять для об'єднання рядків.

Альтернативою для об'єднання рядків може бути використання методу `concat()`:

```Arduino
String stringOne = "Hello";
String stringTwo = " World";
stringOne.concat(stringTwo);
Serial.println(stringOne);
```

Вихід обох кодів буде ідентичним.

# Дивитись також:
Чудовим ресурсом для глибшого розуміння конкатенації рядків у Arduino є офіційна документація Arduino:
- Офіційна документація по методу +: https://www.arduino.cc/en/Reference.StringPlusOperator
- Офіційна документація по методу concat(): https://www.arduino.cc/en/Tutorial/StringConcat