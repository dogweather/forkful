---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:57:15.501564-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה זה התהליך שבו התוכנה שלך מקבלת נתונים משורת הפקודה. תכניתאים עושים את זה כדי לאפשר גמישות והתאמה עצמית של הפעלת התוכנית ללא צורך בשינוי קוד.

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
