---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:14:26.220140-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/getting-the-current-date.md"
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
