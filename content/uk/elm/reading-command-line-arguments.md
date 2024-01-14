---
title:                "Elm: Зчитування аргументів командного рядка"
simple_title:         "Зчитування аргументів командного рядка"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Якщо ви прагнете розвинути свої навички програмування та ефективно використовувати мову програмування Elm, то читання аргументів командного рядка є важливим навичкою. Це дозволить вам більш гнучко та зручно управляти вашими програмами, особливо коли використовуєте їх в консольному середовищі.

## Як

Для читання аргументів командного рядка в Elm, ви можете використовувати модуль `Platform.Cmd`. Нижче наведено приклад коду та результату, який демонструє як отримати аргументи командного рядка та вивести їх у консоль:

```Elm
import Platform.Cmd exposing (getArgs)

main =
  Platform.program
    { init = (model, Cmd.none)
    , update = \msg model -> (model, Cmd.none)
    , view = \model -> Html.text ""
    , subscriptions = \_ -> Sub.none
    , onUrlRequest = \link -> Link.load link
    , onUrlChange = \_ -> Url.navigateTo "" -- перехід на пусту сторінку
    }

onMessage : Message -> Model -> ( Model, Cmd Msg )
onMessage msg model =
  case msg of
    GotArgs args ->
      ( { model | args = args } -- оновити модель з отриманими аргументами
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ getArgs GotArgs -- викличемо функцію з отриманими аргументами при підписці на зміни
    ]

type Msg
  = GotArgs (List String)

-- Результат:
$ elm make Main.elm --output=elm.js
$ node elm.js hello world
["hello","world"]
```

## Глибша розгляд

Можна зазначити, що для отримання аргументів командного рядка не обов'язково використовувати модуль `Platform.Cmd`, так як цей модуль працює лише у консольному середовищі, але як приклад з читання та обробки аргументів цей підхід підходить найкраще.

## Дивіться також

- [Офіційна документація Elm](https://guide.elm-lang.org/)
- [Практичний підручник Elm](https://www.elm-tutorial.org/)
- [Приклади реальних проектів на Elm](https://github.com/elm/projects)
- [Плагіни для Elm у вашому редакторі коду](https://elm-lang.org/docs/editors)