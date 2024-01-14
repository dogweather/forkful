---
title:                "Elm: Розбір html"
simple_title:         "Розбір html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/parsing-html.md"
---

{{< edit_this_page >}}

##Чому

Html-парсінг - це важлива навичка для будь-якого програміста Elm. Вона дозволяє нам ефективно отримувати дані з веб-сайтів та використовувати їх у нашому коді.

##Як

```Elm
import Html exposing (text)
import Html.Parser exposing (..)

main =
    let
        html = "<div>Hello <em>world</em></div>"
        parsed = parse html
    in
        text parsed
```
Вивід: ``` "Hello world" ```

У цьому прикладі ми імпортуємо необхідні модулі, створюємо рядок з HTML та застосовуємо функцію parse, щоб отримати рядок з об'єднаним текстом з усіх елементів.

##Глибина аналізу

Розбір HTML полягає в тому, щоб отримати дані з HTML документа та зробити їх доступними для подальшого використання в нашому Elm-коді. Elm має вбудовану бібліотеку для розбору HTML, яка дуже проста та легка у використанні. Ми можемо використовувати функції, такі як parse і extract, щоб отримати необхідні дані з тегів.

##Дивись також

- [Офіційна документація Elm для розбирання HTML](https://package.elm-lang.org/packages/elm/parser/latest/Html-Parser)
- [Стаття на тему розбирання HTML у Elm](https://dev.to/rvjuly/parsing-html-in-elm-3p63)
- [Демо Elm додаток для розбирання HTML](https://ellie-app.com/4xKKH3t84hSa1/)