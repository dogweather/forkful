---
title:                "Elm: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому
Програмування у мові Elm - це швидший та ефективніший спосіб створювати веб-додатки. Настільки ефективний, що навіть звичайна операція, така як перетворення рядка у верхній регістр, може бути зроблена з легкістю.

## Як зробити
```elm
import String

capitalizedString = String.toUpper "elm programming" 

-- Вивід: ELM PROGRAMMING
```

Ми можемо використовувати модуль String у мові Elm для перетворення рядка у верхній регістр за допомогою функції `String.toUpper`. Просто передайте функції рядок, який потрібно перетворити, і отримаєте результат без зайвих зусиль. 

## Глибока занурення
Але як саме ця функція працює? Під капотом, модуль String у мові Elm використовує стандартний алгоритм Unicode для перетворення рядка у верхній регістр. Це означає, що функція може обробляти не тільки латинські символи, але й будь-які символи у будь-якій мові. Таким чином, ви можете використовувати цю функцію на будь-якій мові, не хвилюючись про правильне розташування символів у рядку.

## Дивись також
- [Документація Elm по модулю String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Офіційний сайт Elm](https://elm-lang.org/)
- [Вступ до програмування у мові Elm](https://dev.to/kspeakman/getting-started-with-elm-510n)
- [Приклади завдань на мові Elm](https://rosettacode.org/wiki/Category:Elm)