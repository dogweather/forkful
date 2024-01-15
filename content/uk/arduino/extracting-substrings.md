---
title:                "Видобування підрядків."
html_title:           "Arduino: Видобування підрядків."
simple_title:         "Видобування підрядків."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Чому 
Є багато ситуацій, коли нам потрібно виділити певний фрагмент тексту з рядка даних. Наприклад, можливо нам потрібно отримати ключову фразу з повідомлення що надійшло на нашому пристрої. У цьому випадку, використання функції вилучення підрядка дуже корисне та зручне.

## Як це зробити
Для вилучення підрядка в Arduino, ми будемо використовувати функцію `substring()`. Ця функція приймає два аргументи: початкову та кінцеву позиції підрядка, який ми хочемо вилучити. Наприклад, якщо ми хочемо вилучити перші 5 символів з рядка, ми можемо використати наступний код:
```Arduino
String str = "Привіт, це Arduino!";
String result = str.substring(0, 5);
Serial.println(result);
```
Вихідний код цього прикладу виведе`Привіт` на моніторі порту серійних даних.

## Глибше в деталі
Функція `substring()` також може приймати лише один аргумент - початкову позицію підрядка, а кінцева позиція буде останнім символом рядка. Також ми можемо використовувати від'ємні числа для вказівки позиції від кінця рядка. Наприклад, якщо ми хочемо вилучити останні 7 символів з рядка, ми можемо використати `str.substring(-7)`.

## Дивись також
- [Документація про функцію `substring()` для Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Приклади використання функції `substring()` з різними аргументами](https://www.geeksforgeeks.org/arduino-string-substring/?ref=lbp)
- [Відео на YouTube, яке показує, як використовувати `substring()` на Arduino](https://www.youtube.com/watch?v=tufOg1AwdTI)