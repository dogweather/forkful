---
date: 2024-01-20 15:14:26.220140-07:00
description: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA \u05D6\u05D4\
  \ \u05DC\u05E9\u05DC\u05D5\u05E3 \u05D0\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D5\u05D4\u05E9\u05E2\u05D4 \u05DB\u05E8\u05D2\u05E2. \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4 \u05DC\u05EA\u05D9\u05E2\u05D5\u05D3, \u05EA\u05D9\u05D6\u05DE\u05D5\u05DF\
  \ \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05D5\u05E9\u05DC\u05DC \u05E4\u05D9\u05E6\
  '\u05E8\u05D9\u05DD \u05E9\u05EA\u05DC\u05D5\u05D9\u05D9\u05DD \u05D1\u05D6\u05DE\
  \u05DF \u05D0\u05DE\u05D9\u05EA\u05D9."
lastmod: '2024-03-13T22:44:39.219617-06:00'
model: unknown
summary: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9 \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA \u05D6\u05D4 \u05DC\
  \u05E9\u05DC\u05D5\u05E3 \u05D0\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\
  \u05D4\u05E9\u05E2\u05D4 \u05DB\u05E8\u05D2\u05E2. \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DC\u05EA\u05D9\u05E2\u05D5\u05D3, \u05EA\u05D9\u05D6\u05DE\u05D5\u05DF \u05E4\
  \u05E2\u05D5\u05DC\u05D5\u05EA \u05D5\u05E9\u05DC\u05DC \u05E4\u05D9\u05E6'\u05E8\
  \u05D9\u05DD \u05E9\u05EA\u05DC\u05D5\u05D9\u05D9\u05DD \u05D1\u05D6\u05DE\u05DF\
  \ \u05D0\u05DE\u05D9\u05EA\u05D9."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי בתכנות זה לשלוף את התאריך והשעה כרגע. תוכניתנים עושים את זה לתיעוד, תיזמון פעולות ושלל פיצ'רים שתלויים בזמן אמיתי.

## איך לעשות:
ב-Elm, לקבל את התאריך הנוכחי זה קצת שונה משפות אחרות כי אתה צריך לעבוד עם מערכת ההודעות של הסביבה.
```Elm
import Browser
import Task
import Time exposing (Posix)

type Msg = GotTime Posix

type alias Model = Maybe Posix

init : () -> (Model, Cmd Msg)
init () =
    (Nothing, Task.perform GotTime Time.now)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotTime posixTime ->
            (Just posixTime, Cmd.none)

-- זה יפעיל את 'init', וכאשר התאריך הנוכחי יתקבל, 'update' יעבד את המידע.
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

view : Model -> Html msg
view model =
    case model of
        Nothing -> 
            text "Fetching current date and time..."
        Just posixTime ->
            text <| "Current date and time: " ++ (Time.posixToHuman posixTime)
```
הפלט יהיה כמו `"Current date and time: Sat, 24 Jun 2023 12:45:27 GMT"` אחרי שהזמן יתעדכן.

## עיון מעמיק
לפני Elm 0.19, היה יותר קל לקבל את התאריך הנוכחי בזמנים סינכרוניים. אבל בגרסה הזו, מבנה הקוד הפך להיות מבוסס יותר על הודעות כדי לקדם את היבטי תכנות הפונקציונלי של Elm ולתת ניהול טוב יותר של תהליכים א-סינכרוניים. יש אלטרנטיבות כמו שימוש ב`Time.every` לקבלת עדכוני זמן באופן מחזורי, אבל לקבלת התאריך הנוכחי בזמן פתיחת האפליקציה, הדוגמה למעלה היא הנכונה. מודל ה`Posix` מתייחס לפורמט זמן אוניברסלי שמקל על עבודה עם זמנים בצורה מתמטית ובמעבר בין אזורי זמן.

## ראה גם
- [Elm Time documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Browser documentation](https://package.elm-lang.org/packages/elm/browser/latest/)
