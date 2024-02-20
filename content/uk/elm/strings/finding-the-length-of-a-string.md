---
date: 2024-01-20 17:47:17.268131-07:00
description: ''
lastmod: 2024-02-19 22:05:08.155815
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## What & Why?
## Що та Чому?

(1) Дізнатися довжину рядка в Elm - це просто з'ясувати, скільки символів у ньому міститься. (2) Програмісти роблять це, аби впорядковувати дані, забезпечувати валідацію і обмежувати ввід.

## How to:
## Як зробити:

```Elm
import String

-- Функція для отримання довжини рядка
stringLength : String -> Int
stringLength str =
    String.length str

-- Приклад використання
exampleLength : Int
exampleLength =
    stringLength "Привіт, світ!"

-- Вивід: 13
```

## Deep Dive
## Глибше Занурення

(1) Функція `String.length` існує в Elm від самого початку і є стандартним способом з'ясувати довжину рядка. (2) Хоч Elm обмежує нестандартні способи взаємодії зи строками, у інших мовах можуть використовувати цикли чи рекурсію для обчислення довжини. (3) Elm використовує реалізацію Unicode, тож `String.length` вертає кількість іксодних одиниць, що може бути несподіванкою, якщо там є емоджі або інші багатокодові символи.

## See Also
## Дивіться також

- Elm String package documentation: [https://package.elm-lang.org/packages/elm/core/latest/String#length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- Unicode standard explanation: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)
