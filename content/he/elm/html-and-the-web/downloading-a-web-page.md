---
aliases:
- /he/elm/downloading-a-web-page/
date: 2024-01-20 17:44:22.853126-07:00
description: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05E2\u05DE\u05D5\u05D3 \u05D5\
  \u05D5\u05D1 \u05D6\u05D5 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\
  \u05E0\u05D7\u05E0\u05D5 \u05DE\u05D1\u05E7\u05E9\u05D9\u05DD \u05D5\u05E7\u05D5\
  \u05DC\u05D8\u05D9\u05DD \u05EA\u05D5\u05DB\u05DF \u05DE\u05E9\u05E8\u05EA \u05D1\
  \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05E7\u05D1\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E2\
  \u05D1\u05D3 \u05DE\u05D9\u05D3\u05E2 \u05D0\u05D5 \u05DC\u05D0\u05D7\u05E1\u05DF\
  \ \u05D0\u05D5\u05EA\u05D5 \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05DE\u05D0\u05D5\
  \u05D7\u05E8 \u05D9\u05D5\u05EA\u05E8."
lastmod: 2024-02-18 23:08:52.752164
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05E2\u05DE\u05D5\u05D3 \u05D5\u05D5\
  \u05D1 \u05D6\u05D5 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\
  \u05D7\u05E0\u05D5 \u05DE\u05D1\u05E7\u05E9\u05D9\u05DD \u05D5\u05E7\u05D5\u05DC\
  \u05D8\u05D9\u05DD \u05EA\u05D5\u05DB\u05DF \u05DE\u05E9\u05E8\u05EA \u05D1\u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9\
  \ \u05DC\u05E7\u05D1\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E2\u05D1\
  \u05D3 \u05DE\u05D9\u05D3\u05E2 \u05D0\u05D5 \u05DC\u05D0\u05D7\u05E1\u05DF \u05D0\
  \u05D5\u05EA\u05D5 \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05DE\u05D0\u05D5\u05D7\
  \u05E8 \u05D9\u05D5\u05EA\u05E8."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
להוריד עמוד ווב זו תהליך שבו אנחנו מבקשים וקולטים תוכן משרת באינטרנט. תכניתנים עושים את זה כדי לקבל נתונים, לעבד מידע או לאחסן אותו לשימוש מאוחר יותר.

## איך לעשות:
ב-Elm, אנו משתמשים ב-http package כדי להוריד נתונים מהאינטרנט:

```Elm
module Main exposing (main)

import Html exposing (Html, text)
import Http
import Json.Decode exposing (string)

type Msg
    = GotText (Result Http.Error String)

type alias Model =
    { content : String }

init : Model
init =
    { content = "" }

update : Msg -> Model -> Model
update msg model =
    case msg of
        GotText (Ok newText) ->
            { model | content = newText }

        GotText (Err _) ->
            model

subscribe : Model -> Sub Msg
subscribe model =
    Http.get
        { url = "https://example.com"
        , expect = Http.expectString GotText
        }
        |> Sub.task

view : Model -> Html Msg
view model =
    text model.content

main : Program () Model Msg
main =
    Html.program
        { init = (init, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscribe
        }
```

הדוגמא מתארת איך לשלוח בקשה לשרת ולקבל תוכן כטקסט.

## צלילה עמוקה
Elm מספקת פרדיגמה נקייה לניהול תוכנת ווב על ידי שימוש ב-Architecture שלה: Model, Update ו-View. בעבר, בשפות אחרות כמו JavaScript, תהליך זה היה מעט מסורבל יותר. Elm מאוד ממוקדת בטיפול בתוצאות אסינכרוניות, כמו הורדות, בצורה מסודרת ועקבית. תחליף אפשרי ל-http ב-Elm הוא עבודה עם WebSockets למהירות גבוהה יותר ותקשורת דו-כיוונית.

## ראה גם
- [Elm HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [Elm Guide on HTTP](https://guide.elm-lang.org/effects/http.html)
- [JSON Decode documentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- [Elm Architecture Tutorial](https://guide.elm-lang.org/architecture/)
