---
title:                "Пошук та заміна тексту"
html_title:           "Elm: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому
Перебирання тексту і заміна його може бути корисним для виправлення помилок або зміни форматування у вашому коді.

## Як це зробити
```elm
import String

replacedText : String
replacedText =
    String.replace "hello" "hi" "Hello world!"

main =
    replacedText --outputs "Hi world!"
```

## Глибше в деталі
Функція String.replace використовується для заміни певного тексту у рядку на інший текст. Вона приймає три аргументи - перший - текст, який необхідно замінити, другий - текст-замінник і третій - рядок, який буде проходити перетворення.

## Дивіться також
1. [Функція String.replace в офіційній документації Elm](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
2. [Підхід Функціонального програмування на Elixir](https://blog.engineyard.com/2015/elixir-and-functional-programming)
3. [Використання регулярних виразів для пошуку і заміни тексту на Elm](https://dev.to/mndrix/regular-expressions-in-elm-2d5j)