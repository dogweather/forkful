---
date: 2024-01-20 17:57:15.501564-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Elm \u05DC\u05D0\
  \ \u05EA\u05D5\u05DE\u05DA \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05D1\u05E7\u05E8\
  \u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\u05DD \u05DE\
  \u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 \u05D1\u05D3\u05D5\
  \u05DE\u05D4 \u05DC\u05E9\u05E4\u05D5\u05EA \u05DB\u05DE\u05D5 Python \u05D0\u05D5\
  \ Node.js, \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\u05D9\u05D0 \u05E2\u05D5\
  \u05E1\u05E7\u05EA \u05D1\u05E2\u05D9\u05E7\u05E8 \u05D1\u05E4\u05D9\u05EA\u05D5\
  \u05D7 \u05E9\u05DC Front-End \u05DC\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8.\
  \ \u05E2\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.227986-06:00'
model: gpt-4-1106-preview
summary: "Elm \u05DC\u05D0 \u05EA\u05D5\u05DE\u05DA \u05D9\u05E9\u05D9\u05E8\u05D5\
  \u05EA \u05D1\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\
  \u05D8\u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\
  \u05D4 \u05D1\u05D3\u05D5\u05DE\u05D4 \u05DC\u05E9\u05E4\u05D5\u05EA \u05DB\u05DE\
  \u05D5 Python \u05D0\u05D5 Node.js, \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\
  \u05D9\u05D0 \u05E2\u05D5\u05E1\u05E7\u05EA \u05D1\u05E2\u05D9\u05E7\u05E8 \u05D1\
  \u05E4\u05D9\u05EA\u05D5\u05D7 \u05E9\u05DC Front-End \u05DC\u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

## איך לעשות:
Elm לא תומך ישירות בקריאת ארגומנטים משורת הפקודה בדומה לשפות כמו Python או Node.js, מכיוון שהיא עוסקת בעיקר בפיתוח של Front-End לאינטרנט. עם זאת, אם אתה רוצה להשתמש ב-Elm עבור סקריפטינג על המחשב האישי שלך, תצטרך לעטוף אותו ב-JavaScript.

נניח שיש לנו קובץ `Main.elm` ואנו רוצים לעבד ארגומנטים משורת הפקודה, נוכל להשתמש בקובץ עטיפה של Node.js כזה:

```javascript
const { Elm } = require('./Main.elm'); // המרת המודול של Elm ל-Node.js
let app = Elm.Main.init({
  flags: process.argv // מעביר ארגומנטים משורת הפקודה
});

app.ports.output.subscribe(function(data) {
  console.log(data);
});
```

הנה קוד ה-Elm שמתאים לדוגמה שלעיל:
```elm
port module Main exposing (..)

import Json.Decode as Decode

port output : String -> Cmd msg

main : Program () String msg
main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( "", Cmd.none )
        , subscriptions = \_ -> Sub.none
        }

init : flags -> ( String, Cmd msg )
init flags =
    ( processArgs flags, Cmd.none )

processArgs : flags -> String
processArgs flags =
    case Decode.decodeValue Decode.string flags of
        Ok args ->
            args

        Err _ ->
            "No valid args found."
```

## עמק הדעת
ב-Elm, המודל המקובל ליצירת קוד שפועל ביצירתיות עם המידע הנתון, הוא לשלוח את הנתונים מהסביבה החיצונית כגון Node.js או דפדפן כדגלים (flags) לפרוגרמה. זה מאפשר לנו להביא נתונים להפעלת התוכנית בצורה מבוקרת. אפשרות אחרת היא שימוש בסביבות שרת כמו `elm-server-side-renderer` שיכולות לקחת ארגומנטים משורת הפקודה.

בעבר, שפות תכנות אחרות כמו C או Java הפעילו את הכוח לקריאה ישירה משורת הפקודה, אבל Elm לקחה גישה שונה עם דגש על Front-End ופיתוח אפליקציות אינטרנט. עם זאת, קהילת המפתחים יצרה דרכים עקיפות לאפשר שילובים מסוג זה.

## כמו כן
- [Elm Ports](https://guide.elm-lang.org/interop/ports.html) - מידע על כיצד להתקשר עם JavaScript.
- [Node.js Documentation](https://nodejs.org/api/process.html#process_process_argv) - מידע על `process.argv` ב-Node.js.
