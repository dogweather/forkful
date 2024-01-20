---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
בקשת HTTP היא דרך לשלוח ולקבל מידע משרת באמצעות הפרוטוקול HTTP. תכניתאים משתמשים בזה לגשת למידע ממאגרים מרוחקים, לתקשר עם ממשקים תוכנה ציבוריים (APIs), ולביצוע פעולות אחרות שדורשות שיחה עם שרת.

## כיצד ל:
כאן יש דוגמא לשליחת בקשת HTTP באמצעות ביבליותקה המובנת `Http` של Elm, והצגת התוצאות.

```Elm
module Main exposing (..)

import Http
import Json.Decode as Decode

type alias Result =
    { name : String }

decoder : Decode.Decoder Result
decoder =
    Decode.map Result (Decode.field "name" Decode.string)

sendRequest : Cmd msg
sendRequest =
    Http.request
        { method = "GET"
        , url = "https://api.example.com"
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson Msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }

main =
    sendRequest
```
התכנית עוברת על חלקים שונים של בקשת HTTP, כולל המתודה (במקרה זה, "GET"), ה- URL של השרת, כותרות (אם יש) והגוף של הבקשה.

## צלילה עמוקה
שליחת בקשת HTTP היא חלק אינטגרלי מן האינטרנט כפי שאנו מכירים אותו כיום. היא הופקה כחלק מתקן HTTP שנוצר ב- 1991.

על אף שכמה שפות תכנות אחרות מציעות שיטות דומות לעיבוד בקשות HTTP, Elm ניהיליסט הגנטלי המציגת גישה יחודית באופן שהיא טסטית וחסונה במיוחד לשגיאות ריצה. 

תכנים זה משתמש בממיר JSON תוך שימוש בגישה מבנית ומסוג של Elm לטיפול בנתונים החוזרים מהבקשה.

## ראה גם
למידה נוספת על שליחת בקשת HTTP באמצעות Elm:

- [The official Elm guide on HTTP requests](https://guide.elm-lang.org/)
- [HTTP in Elm - video tutorial](https://www.youtube.com/watch?v=t2YWnZSFsg0)