---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо це потрібно?

Знаходження довжини рядка - це визначення кількості символів у цьому рядку. Це здійснюється програмістами для обробки або маніпуляцій з даними на основі довжини рядка.

## Як це робиться:

У мові програмування Elm для знаходження довжини рядка використовується вбудована функція `String.length`. Розглянемо приклад:

```Elm
import String

main = 
    let
        str = "Привіт, світе!"
    in
    String.length str
```

Код вище повертає довжину рядка "Привіт, світе!", яка становить 14.

## Занурення глибше

Разом з введенням Elm в 2012 році, `String.length` була однією з базових функцій, доступних в цій чистій функціональній мові програмування для створення безпечних веб-додатків. Альтернативи, такі як написання власного рекурсивного алгоритму для підрахунку символів у рядку, були б неоптимальні та більш складні для розуміння.

Щодо деталей реалізації, `String.length` повертає кількість Unicode символів у рядку. Важливо пам'ятати, що ця функція повертає довжину в Unicode символах, а не в байтах.

## Див. також

Перегляньте ці корисні ресурси для глибшого розуміння:

- Офіційна документація Elm про `String.length`: [https://package.elm-lang.org/packages/elm/core/latest/String#length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- База знань Elm про рядки: [https://elmprogramming.com/strings.html](https://elmprogramming.com/strings.html)