---
date: 2024-01-20 15:14:26.320874-07:00
description: "How to: (\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) Elm\
  \ \u043C\u0430\u0454 \u0447\u0456\u0442\u043A\u0443 \u0430\u0440\u0445\u0456\u0442\
  \u0435\u043A\u0442\u0443\u0440\u0443, \u044F\u043A\u0430 \u0440\u043E\u0437\u0434\
  \u0456\u043B\u044F\u0454 \u043B\u043E\u0433\u0456\u043A\u0443 \u043E\u0442\u0440\
  \u0438\u043C\u0430\u043D\u043D\u044F \u0447\u0430\u0441\u0443 \u0442\u0430 \u0432\
  \u0456\u0434\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u043D\u044F \u0439\u043E\u0433\
  \u043E \u0443 \u0432'\u044E. \u0406\u0441\u0442\u043E\u0440\u0438\u0447\u043D\u043E\
  , Elm \u0437\u0430\u0437\u043D\u0430\u0432 \u0432\u0435\u043B\u0438\u043A\u0438\u0445\
  \ \u0437\u043C\u0456\u043D \u0443\u2026"
lastmod: '2024-04-05T22:51:02.270504-06:00'
model: unknown
summary: "(\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) Elm \u043C\u0430\
  \u0454 \u0447\u0456\u0442\u043A\u0443 \u0430\u0440\u0445\u0456\u0442\u0435\u043A\
  \u0442\u0443\u0440\u0443, \u044F\u043A\u0430 \u0440\u043E\u0437\u0434\u0456\u043B\
  \u044F\u0454 \u043B\u043E\u0433\u0456\u043A\u0443 \u043E\u0442\u0440\u0438\u043C\
  \u0430\u043D\u043D\u044F \u0447\u0430\u0441\u0443 \u0442\u0430 \u0432\u0456\u0434\
  \u043E\u0431\u0440\u0430\u0436\u0435\u043D\u043D\u044F \u0439\u043E\u0433\u043E\
  \ \u0443 \u0432'\u044E."
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
weight: 29
---

## How to: (Як зробити:)
```Elm
import Browser
import Html exposing (Html, text)
import Task

type Msg = GotTime Posix

type alias Model = Posix

init : () -> (Model, Cmd Msg)
init _ =
    (Posix.fromMillis 0, Task.perform GotTime Time.now)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotTime newTime ->
            (newTime, Cmd.none)

view : Model -> Html Msg
view model =
    text (String.fromInt (Posix.toMillis model))

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
```
Sample output: 
```
1617632023765
```

## Deep Dive (Поглиблений Огляд)
Elm має чітку архітектуру, яка розділяє логіку отримання часу та відображення його у в'ю. Історично, Elm зазнав великих змін у версіях 0.17 та 0.18, спростивши роботу зі сторонніми бібліотеками JS та забезпечивши сильнішу систему типів. Для отримання часу Elm використовує власний тип Posix, який репрезентує точку часу в UTC. Альтернативою може бути передача часу з сервера для синхронізації, особливо важливо це, коли точність критична і можуть бути проблеми з часовими зонами.

## See Also (Детальніше)
- Elm Time package documentation: https://package.elm-lang.org/packages/elm/time/latest/
- Handling time zones in Elm: https://medium.com/elm-shorts/handling-time-zones-in-elm-e0e1872e7d2b
- Elm architecture tutorial: https://guide.elm-lang.org/architecture/
