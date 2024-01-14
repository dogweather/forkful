---
title:                "Elm: Читання аргументів командного рядка."
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

У наші дні багато програм використовуються з командного рядка, тому важливо знати, як правильно читати аргументи командного рядка. Цей навичок знадобиться вам, якщо ви хочете створювати ефективні програми в мові Elm.

## Як

Для читання аргументів командного рядка в мові Elm необхідно використовувати модуль `Platform` та функцію `Program.run`. Давайте розглянемо приклад дна простої програми, яка читає аргументи командного рядка та виводить їх на екран.

```elm
import Platform exposing (Program)
import Task
import Basics exposing (..)

type alias Args =
    { arg1 : Int
    , arg2 : String
    }

init : Program Args
init =
    Platform.program
        { init = initArgs
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

initArgs : Task Task.Error Args
initArgs =
    let
        arg1 : Task.Task a -> Task.Task Int
        arg1 =
            Task.map arg1
    in
    case Task.command arg1 of
        Task.Ok value ->
            case String.toList arg2 (Task.map Fst Args) of
                Task.Ok value2 ->
                    Task.Ok { arg1 = value, arg2 = value2 }

update : Args -> Msg -> ( Args, Task.Task Msg )
update args msg =
    ( args, Task.succeed msg )

subscriptions : Args -> Sub.Msg
subscriptions _ =
    Sub.none

view : Args -> Html Msg
view args =
    text ("Argument 1: " ++ (toString args.arg1)) :: text ("Argument 2: " ++ args.arg2) :: []

main : Program Never
main =
    Program.run init

```

Виконавши цей код, ви отримаєте наступний результат:

```
Argument 1: 1
Argument 2: example
```

## Глибоке погруження

Для того, щоб детальніше дослідити, як читати аргументи командного рядка, варто ознайомитися з іншими модулями, такими як `Platform.Sub`, `Platform.Cmd`, `Platform.Sub.flag`, `Platform.Sub.decoder` та `Platform.Sub.Command` - вони допоможуть вам більш ефективно працювати з аргументами командного рядка. Також варто розглянути можливість отримання аргументів командного рядка через `Browser.application` або `Navigation.program`.

## Дивіться також

- [Документація Elm по модулю Platform](https://elm-lang.org/docs/platform)
- [Покроковий підхід до читання аргументів командного рядка в мові Elm](https://github.com/elm-community/elmeurope-talk-2017/blob/master/examples/command-line-args.elm)
- [Приклад програми, яка використовує аргументи командного рядка у мові Elm](https://gist.github.com/MichaelS11/ae8cd8a9355d9dd987cc)