---
title:                "Перетворення рядка у нижній регістр"
html_title:           "Elm: Перетворення рядка у нижній регістр"
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Навіщо нам знадобиться конвертувати рядок в нижній регістр? Національні українські літери використовуються в українській мові, тому конвертування до нижнього регістру може бути корисним, коли ми хочемо забезпечити правильне відображення тексту в нашій програмі.

## Як це зробити

```Elm
import String

stringToLower : String -> String
stringToLower input =
  String.toLower input

stringToLower "ВИХіДУЩиск" -- виведе "вихідущиск"
```

## Поглиблене дослідження

У Elm є вбудована функція `String.toLower`, яка дозволяє нам легко конвертувати рядок до нижнього регістру. Але варто зауважити, що ця функція застосовується лише до латинських літер. Для конвертування українських літер до нижнього регістру, може бути доречніше використовувати додаткову бібліотеку або створити власну функцію для цього.

## Дивіться також

- [Документація Elm про `String.toLower`](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Бібліотека для конвертування українських літер до нижнього регістру](https://package.elm-lang.org/packages/elm-community/string-extra/latest/String-LowerCase-Ukrainian)
- [Блог про Elm](https://medium.com/@jreispt/why-use-elm-part-1-96365cc614b3) (англійською)