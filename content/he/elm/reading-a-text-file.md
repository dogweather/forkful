---
title:                "קריאת קובץ טקסט"
aliases:
- he/elm/reading-a-text-file.md
date:                  2024-01-20T17:54:42.768494-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט ב-Elm היא ליקט נתונים מקובץ מאוחסן. מתכנתים עושים זאת כדי לטעון, לעבד או להציג מידע חיצוני באפליקציית ה-Elm שלהם.

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
