---
title:    "Elm: Читання аргументів командного рядка"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Найпростіший спосіб для взаємодії з програмами на вашому комп'ютері - використання командного рядка. Прочитання аргументів командного рядка є важливою навичкою для розробника, яка дозволяє передавати вхідні дані у вашу програму під час її виконання.

## Як

```Elm
import Platform exposing (worker)
import Task

type alias Args =
    { program : String
    , positional : List String
    , named : Dict.Dict String String
    }

type alias Message =
    Args -> Task.Task x ()

getArgs : ( Args -> msg ) -> Cmd msg
getArgs toMsg =
    Json.Decode.list Json.Decode.string
        |> Task.perform (toMsg << fromPlatformArgs)
        |> Platform.worker

fromPlatformArgs : List String -> Args
fromPlatformArgs args =
    let ( program, positional, named ) =
        args
            |> List.unzip
    in
        Args program positional (List.filterMap namedArgs args)

namedArgs : String -> Maybe ( String, String )
namedArgs arg =
    case String.split "=" arg of
        [ name, value ] ->
            Just ( name, value )

        _ ->
            Nothing


```


Використовуйте функцію `getArgs` з параметром, який приймає тип даних `Msg`, отриманий з декодування аргументів командного рядка. Функція `fromPlatformArgs` допомагає розпакувати аргументи на окремі частини, які можна буде використовувати в вашій програмі.

## Глибокий погляд

Застосовувані функції `Json.Decode.list` та `Json.Decode.string` допомагають декодувати аргументи з формату JSON у зрозумілі типи даних для Elm. Зверніть увагу, що треба буде самостійно обробляти значення зірочки `*`, які означають, що тип даних невідомий.

## Дивіться також

- [Документація Elm по `Platform.worker`](https://package.elm-lang.org/packages/elm/browser/latest/Platform#worker)
- [Більше інформації про роботу з командним рядком в Elm](https://guide.elm-lang.org/interop/cmdline.html)
- [Приклади коду з читанням аргументів командного рядка](https://github.com/elm-lang/elm-lang.org/tree/master/src/examples/cmdline)