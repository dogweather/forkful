---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:35.729941-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Arduino, \u044F\u043A\u0435 \u0432 \u043E\u0441\u043D\u043E\u0432\u043D\u043E\u043C\
  \u0443 \u0432\u0456\u0434\u043E\u043C\u0435 \u0432\u0437\u0430\u0454\u043C\u043E\
  \u0434\u0456\u0454\u044E \u0437 \u0430\u043F\u0430\u0440\u0430\u0442\u043D\u0438\
  \u043C \u0437\u0430\u0431\u0435\u0437\u043F\u0435\u0447\u0435\u043D\u043D\u044F\u043C\
  , \u0442\u0430\u043A\u043E\u0436 \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0431\
  \u0430\u0437\u043E\u0432\u0456 \u043C\u043E\u0436\u043B\u0438\u0432\u043E\u0441\u0442\
  \u0456 \u043C\u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u043D\u043D\u044F\
  \ \u0440\u044F\u0434\u043A\u0430\u043C\u0438 \u0447\u0435\u0440\u0435\u0437 \u0441\
  \u0432\u0456\u0439\u2026"
lastmod: '2024-03-13T22:44:49.692791-06:00'
model: gpt-4-0125-preview
summary: "Arduino, \u044F\u043A\u0435 \u0432 \u043E\u0441\u043D\u043E\u0432\u043D\u043E\
  \u043C\u0443 \u0432\u0456\u0434\u043E\u043C\u0435 \u0432\u0437\u0430\u0454\u043C\
  \u043E\u0434\u0456\u0454\u044E \u0437 \u0430\u043F\u0430\u0440\u0430\u0442\u043D\
  \u0438\u043C \u0437\u0430\u0431\u0435\u0437\u043F\u0435\u0447\u0435\u043D\u043D\u044F\
  \u043C, \u0442\u0430\u043A\u043E\u0436 \u0432\u043A\u043B\u044E\u0447\u0430\u0454\
  \ \u0431\u0430\u0437\u043E\u0432\u0456 \u043C\u043E\u0436\u043B\u0438\u0432\u043E\
  \u0441\u0442\u0456 \u043C\u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u043D\
  \u043D\u044F \u0440\u044F\u0434\u043A\u0430\u043C\u0438 \u0447\u0435\u0440\u0435\
  \u0437 \u0441\u0432\u0456\u0439 \u043E\u0431'\u0454\u043A\u0442 `String`."
title: "\u0417\u0440\u043E\u0431\u0438\u0442\u0438 \u043F\u0435\u0440\u0448\u0443\
  \ \u043B\u0456\u0442\u0435\u0440\u0443 \u0440\u044F\u0434\u043A\u0430 \u0432\u0435\
  \u043B\u0438\u043A\u043E\u044E"
weight: 2
---

## Як це зробити:
Arduino, яке в основному відоме взаємодією з апаратним забезпеченням, також включає базові можливості маніпулювання рядками через свій об'єкт `String`. Однак, воно не має прямої функції `capitalize`, яка бачена в мовах вищого рівня. Таким чином, ми реалізуємо приведення до великої літери, перебираючи рядок і застосовуючи перетворення регістру.

Ось простий приклад без використання сторонніх бібліотек:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // Повертаємо порожній рядок, якщо вхідний рядок порожній
  }
  input.toLowerCase(); // Спочатку перетворюємо весь рядок в нижній регістр
  input.setCharAt(0, input.charAt(0) - 32); // Приводимо перший символ до великої літери
  
  // Приводимо до великої літери символи, які йдуть після пробілу
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // Вивід: "Hello Arduino World"
}

void loop() {
  // Пустий цикл
}
```

Цей фрагмент коду визначає функцію `capitalizeString`, яка спочатку перетворює весь рядок в нижній регістр, щоб стандартизувати його випадок. Потім вона приводить до великої літери перший символ та будь-який символ, що слідує за пробілом, ефективно приводячи кожне слово у вхідному рядку до великих літер. Зауважте, що ця первісна реалізація передбачає кодування символів ASCII і може потребувати коригувань для повної підтримки Unicode.

Наразі, не існує широко прийнятих сторонніх бібліотек, спеціально для маніпуляції рядками в екосистемі Arduino, головним чином через її акцент на взаємодії з апаратним забезпеченням та ефективності. Однак, наданий приклад є простим способом досягти приведення рядка до великих літер в програмному середовищі Arduino.
