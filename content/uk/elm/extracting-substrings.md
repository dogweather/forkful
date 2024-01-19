---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і чому?

Витягування підрядка - це процес вибірки ділянки тексту з більшого рядка. Programmers often use it for processing text data, extracting specific parts for analysis or manipulation.

## Як:

Давайте подивимося на приклад. В Elm є функція slice, яка використовується для витягування підрядків.

```Elm
import String

main =
    String.slice 0 5 "Привіт світ!"
```

Цей код виведе "Привіт", яке є першими п’ятьма символами в рядку "Привіт світ!".

## Поглиблений огляд

Перші реалізації витягування підрядків в модерних мовах програмування з'явилися в 70-х роках, і вони залишаються поширеними в більшості мов включно з Elm.

Альтернативою може бути користування регулярними виразами для витягування підрядків, але в Elm можна легко виконати це за допомогою `String.slice`.

Функція `String.slice` працює за принципом зрізів масивів. Вона бере два індекси: початковий та кінцевий, і повертає новий рядок, що складається з символів оригінального рядка між цими двома індексами.

## Див. також

1. [Official Elm Documentation for String](https://package.elm-lang.org/packages/elm/core/latest/String#slice): офіційне керівництво Elm для рядків, в тому числі `String.slice`.
2. [Practical Elm Tutorial on Extracting Substrings](https://elmprogramming.com/strings.html): керівництво по Elm із практичними прикладами використання `String.slice`.
3. [Regular Expressions in Elm](https://package.elm-lang.org/packages/elm/regex/latest/): якщо ви зацікавлені в користуванні регулярними виразами для витягування підрядків.