---
title:                "הורדת דף אינטרנט"
aliases:
- /he/elm/downloading-a-web-page/
date:                  2024-01-20T17:44:22.853126-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/downloading-a-web-page.md"
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
