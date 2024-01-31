---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:14:26.320874-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Отримання поточної дати – це зчитування системного часу девайсу, щоб знати дату та час зараз. Програмісти роблять це для логування, таймштампів, або функціоналу, що базується на даті.

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
