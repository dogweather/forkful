---
aliases:
- /uk/arduino/converting-a-string-to-lower-case/
date: 2024-01-20 17:37:47.281819-07:00
description: "Converting a string to lower case means changing all uppercase letters\
  \ in the text to their lowercase equivalents. Programmers often do this for\u2026"
lastmod: 2024-02-18 23:09:00.779841
model: gpt-4-1106-preview
summary: "Converting a string to lower case means changing all uppercase letters in\
  \ the text to their lowercase equivalents. Programmers often do this for\u2026"
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
---

{{< edit_this_page >}}

## What & Why? (## Що і Чому?)
Converting a string to lower case means changing all uppercase letters in the text to their lowercase equivalents. Programmers often do this for consistency, especially when comparing or processing text data.

## How to: (## Як зробити:)
```arduino
String original = "Hello, Ukraine!";
String lowercase = original.toLowerCase();

Serial.begin(9600);
Serial.println(lowercase); // prints "hello, ukraine!"
```

## Deep Dive (## Поглиблений аналіз)
Lowercasing strings has been essential in computing since its early days when data storage and comparison needed standardization. In Arduino, the `String` object's `toLowerCase()` method makes this task easy. Alternatives, like using ASCII manipulation, are cumbersome and rarely used.

When you use `toLowerCase()`, it internally loops through each character, checking if it's uppercase. If so, it calculates the lowercase version based on ASCII values. It's simple and efficient for Arduino projects where memory and processing power need careful management.

Remember, lowercasing only applies to alphabetic characters; numbers and symbols are unaffected.

## See Also (## Дивіться також):
- Arduino's official `String` reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/tolowercase/
- C++ `transform` and `tolower`: a more complex but versatile method, for interested programmers: https://en.cppreference.com/w/cpp/algorithm/transform
- Unicode consideration for non-ASCII characters (advanced topic): https://unicode.org/reports/tr21/tr21-5.html
