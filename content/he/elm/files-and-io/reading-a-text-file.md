---
date: 2024-01-20 17:54:42.768494-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Elm \u05D0\u05D9\u05E0\
  \u05D5 \u05DE\u05D0\u05E4\u05E9\u05E8 \u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\u05D9\
  \u05E8\u05D4 \u05DC\u05E7\u05D1\u05E6\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05DE\u05D4\
  \u05D3\u05E4\u05D3\u05E4\u05DF \u05E2\u05E7\u05D1 \u05DE\u05D2\u05D1\u05DC\u05D5\
  \u05EA \u05D0\u05D1\u05D8\u05D7\u05D4. \u05E2\u05DD \u05D6\u05D0\u05EA, \u05D0\u05E4\
  \u05E9\u05E8 \u05DC\u05D8\u05E2\u05D5\u05DF \u05D8\u05E7\u05E1\u05D8 \u05D3\u05E8\
  \u05DA \u05D8\u05E2\u05D9\u05E0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D0\u05D5 \u05D1\
  \u05E7\u05E9\u05EA HTTP. #."
lastmod: '2024-03-13T22:44:39.230941-06:00'
model: gpt-4-1106-preview
summary: "Elm \u05D0\u05D9\u05E0\u05D5 \u05DE\u05D0\u05E4\u05E9\u05E8 \u05D2\u05D9\
  \u05E9\u05D4 \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05E7\u05D1\u05E6\u05D9 \u05D8\
  \u05E7\u05E1\u05D8 \u05DE\u05D4\u05D3\u05E4\u05D3\u05E4\u05DF \u05E2\u05E7\u05D1\
  \ \u05DE\u05D2\u05D1\u05DC\u05D5\u05EA \u05D0\u05D1\u05D8\u05D7\u05D4."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

## איך לעשות:
Elm אינו מאפשר גישה ישירה לקבצי טקסט מהדפדפן עקב מגבלות אבטחה. עם זאת, אפשר לטעון טקסט דרך טעינת קובץ או בקשת HTTP.

### דוגמה לטעינת טקסט באמצעות HTTP:
```Elm
module Main exposing (..)

import Http
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)

type Msg
    = Fetch
    | ReceiveText (Result Http.Error String)

type alias Model =
    { content : String
    , error : Maybe String
    }

init : Model
init =
    { content = ""
    , error = Nothing
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Fetch ->
            ( model
            , Http.get
                { url = "https://example.com/data.txt"
                , expect = Http.expectString ReceiveText
                }
            )

        ReceiveText (Ok str) ->
            ( { model | content = str, error = Nothing }
            , Cmd.none
            )

        ReceiveText (Err error) ->
            ( { model | error = Just <| "Failed to load text: " ++ Http.errorToString error }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Fetch ] [ text "Load Text" ]
        , div [] [ text model.content ]
        , case model.error of
            Just err ->
                div [] [ text err ]

            Nothing ->
                text ""
        ]

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
```

### פלט לדוגמה:
כאשר המשתמש ילחץ על "Load Text", הטקסט מהקישור יוצג באפליקציה.

## הבט עמוק יותר:
בעבר, עם JavaScript, קריאה ישירה לקובץ הייתה יותר פשוטה. Elm ממזער גישה לקובצים כחלק מהפילוסופיה למודל אבטחה יציב ומנותק. הגישה המועדפת היא דרך בקשות HTTP או על ידי שילוב עם JavaScript דרך Ports. קוראים לקבצי טקסט באמצעות Ports דורשת קוד JS צדדי שמשתלם עם הפלטפורמה של Elm. הדבר מאפשר לנו למנף יכולות JS מבלי לוותר על הבטיחות והיציבות של אפליקציית Elm. אולטרנטיבות נוספות כוללות הטמעה ב-server side או שימוש ב-API נפרד שנותן שירות של טעינת קבצים.

## ראו בנוסף:
- [Elm Guide on HTTP](https://guide.elm-lang.org/effects/http.html)
- [Elm Ports](https://guide.elm-lang.org/interop/ports.html)

זהו כל המידע שתצטרך לקריאת קבצי טקסט בElm. תכתוב קוד טוב ועדכני!
