---
title:                "Великі літери в рядку"
html_title:           "Elm: Великі літери в рядку"
simple_title:         "Великі літери в рядку"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що й навіщо?
Перетворення рядка в верхній регістр — це процес заміни всіх маленьких літер в рядку на відповідні великі літери. Програмісти роблять це, щоб поліпшити гнучкість програми — наприклад, при порівнянні рядків без урахування регістра.

## Як це робиться:
Щоб перетворити рядок на великі літери в Elm, ви можете скористатись функцією `String.toUpper`. Ось її приклад використання:

```Elm
import String exposing(toUpper)

main =
    let
        originalStr = "привіт, світ!"
    in
        originalStr 
        |> toUpper 
        |> Html.text
       
```

При виконанні цього коду вашій програмі буде виведено на екран: "ПРИВІТ, СВІТ!".

## Зануримося глибше
Цей метод походить зі старих мов програмування, де не було підтримки Unicode. В Elm, `String.toUpper` працює з Unicode символами, оберігаючи їхні властивості. Як альтернативний механізм, можна використовувати список кодів символів, але це більш складний і менш надійний метод.

## Також дивіться
- [Elm: API для рядків](https://package.elm-lang.org/packages/elm/core/latest/String)