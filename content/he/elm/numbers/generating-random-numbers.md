---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:51:15.919952-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05D8\u05D1\
  \u05E2 \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9 \u05D4\
  \u05D8\u05D4\u05D5\u05E8 \u05E9\u05DC Elm \u05D0\u05D5\u05DE\u05E8 \u05E9\u05D0\u05D9\
  \ \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D9\u05D9\u05E6\u05E8 \u05DE\u05E1\u05E4\u05E8\
  \u05D9\u05DD \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D9\u05E9\u05D9\u05E8\
  \u05D5\u05EA \u05DB\u05E4\u05D9 \u05E9\u05D0\u05D5\u05DC\u05D9 \u05EA\u05E2\u05E9\
  \u05D5 \u05D1\u05E9\u05E4\u05D5\u05EA \u05E4\u05E7\u05D5\u05D3\u05EA\u05D9\u05D5\
  \u05EA. \u05D1\u05DE\u05E7\u05D5\u05DD \u05D6\u05D0\u05EA, \u05D0\u05EA\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05DE\u05D5\u05D3\u05D5\u05DC `Random`\u2026"
lastmod: '2024-03-13T22:44:39.195009-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D8\u05D1\u05E2 \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\
  \u05E0\u05DC\u05D9 \u05D4\u05D8\u05D4\u05D5\u05E8 \u05E9\u05DC Elm \u05D0\u05D5\u05DE\
  \u05E8 \u05E9\u05D0\u05D9 \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D9\u05D9\u05E6\u05E8\
  \ \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD\
  \ \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DB\u05E4\u05D9 \u05E9\u05D0\u05D5\u05DC\
  \u05D9 \u05EA\u05E2\u05E9\u05D5 \u05D1\u05E9\u05E4\u05D5\u05EA \u05E4\u05E7\u05D5\
  \u05D3\u05EA\u05D9\u05D5\u05EA."
title: "\u05D9\u05D9\u05E6\u05D5\u05E8 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD"
weight: 12
---

## איך לעשות:
הטבע הפונקציונלי הטהור של Elm אומר שאי אפשר לייצר מספרים אקראיים ישירות כפי שאולי תעשו בשפות פקודתיות. במקום זאת, אתם משתמשים במודול `Random` בשילוב עם פקודות. הנה דוגמה בסיסית שמייצרת מספר שלם אקראי בין 1 ל-100.

ראשית, התקינו את מודול `Random` עם `elm install elm/random`. לאחר מכן ייבאו אותו לקובץ Elm שלכם, יחד עם המודולים הנדרשים של HTML ואירועים, כך:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

כדי שדוגמה זו תהיה עצמאית, תוכלו להוסיף זירוז קוד זה:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

לאחר מכן, הגדירו **פקודה** לייצר מספר אקראי. זה כולל הקמת טיפוס `Msg` לטיפול במספר האקראי לאחר יצירתו, `Model` לאחסון שלו, ופונקציית עדכון לקשר הכל יחד.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

כדי להפעיל יצירת מספר, אתם שולחים הודעת `Generate`, לדוגמה, דרך כפתור בתצוגה שלכם:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("מספר אקראי: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "צור" ]
        ]
```

כאשר לוחצים על הכפתור "צור", יוצג מספר אקראי בין 1 ל-100.

גישה פשטנית זו יכולה להתאים ולהתרחב, תוך שימוש בפונקציות נוספות במודול `Random` כדי לייצר מספרים עשרוניים אקראיים, רשימות, או אפילו מבני נתונים מורכבים מבוססי סוגים מותאמים אישית, ומספקת מגרש משחק נרחב להוספת אקראיות ליישומי Elm שלכם.

המדריך של Elm נכנס לפרטים רבים יותר. יש בו גם [דוגמה לגלגול קוביה בעלת שש פאות](https://guide.elm-lang.org/effects/random).
