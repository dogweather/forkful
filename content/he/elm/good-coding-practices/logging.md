---
date: 2024-01-26 01:03:56.080099-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D0\u05E8\u05DB\
  \u05D9\u05D8\u05E7\u05D8\u05D5\u05E8\u05EA \u05E9\u05E4\u05EA Elm \u05D0\u05D9\u05E0\
  \u05D4 \u05EA\u05D5\u05DE\u05DB\u05EA \u05D1\u05D0\u05E4\u05E7\u05D8\u05D9\u05DD\
  \ \u05DC\u05E6\u05D3\u05D3\u05D9\u05D9\u05DD \u05DB\u05DE\u05D5 \u05E8\u05D9\u05E9\
  \u05D5\u05DD \u05DC\u05D5\u05D2 \u05DB\u05D1\u05E8\u05D9\u05E8\u05EA \u05DE\u05D7\
  \u05D3\u05DC - \u05D0\u05EA\u05DD \u05DE\u05EA\u05DE\u05D5\u05D3\u05D3\u05D9\u05DD\
  \ \u05E2\u05DD \u05D6\u05D4 \u05D3\u05E8\u05DA \u05E4\u05E7\u05D5\u05D3\u05D5\u05EA\
  , \u05D0\u05E9\u05E8 \u05D4\u05DD \u05D7\u05DC\u05E7 \u05DE\u05D0\u05E8\u05DB\u05D9\
  \u05D8\u05E7\u05D8\u05D5\u05E8\u05EA \u05D4\u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\
  \u05D4 \u05E9\u05DC\u05DB\u05DD.\u2026"
lastmod: '2024-03-13T22:44:39.213317-06:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05E8\u05DB\u05D9\u05D8\u05E7\u05D8\u05D5\u05E8\u05EA \u05E9\u05E4\
  \u05EA Elm \u05D0\u05D9\u05E0\u05D4 \u05EA\u05D5\u05DE\u05DB\u05EA \u05D1\u05D0\u05E4\
  \u05E7\u05D8\u05D9\u05DD \u05DC\u05E6\u05D3\u05D3\u05D9\u05D9\u05DD \u05DB\u05DE\
  \u05D5 \u05E8\u05D9\u05E9\u05D5\u05DD \u05DC\u05D5\u05D2 \u05DB\u05D1\u05E8\u05D9\
  \u05E8\u05EA \u05DE\u05D7\u05D3\u05DC - \u05D0\u05EA\u05DD \u05DE\u05EA\u05DE\u05D5\
  \u05D3\u05D3\u05D9\u05DD \u05E2\u05DD \u05D6\u05D4 \u05D3\u05E8\u05DA \u05E4\u05E7\
  \u05D5\u05D3\u05D5\u05EA, \u05D0\u05E9\u05E8 \u05D4\u05DD \u05D7\u05DC\u05E7 \u05DE\
  \u05D0\u05E8\u05DB\u05D9\u05D8\u05E7\u05D8\u05D5\u05E8\u05EA \u05D4\u05D0\u05E4\u05DC\
  \u05D9\u05E7\u05E6\u05D9\u05D4 \u05E9\u05DC\u05DB\u05DD."
title: "\u05DC\u05D5\u05D2\u05D9\u05DD"
weight: 17
---

## איך לעשות:
ארכיטקטורת שפת Elm אינה תומכת באפקטים לצדדיים כמו רישום לוג כברירת מחדל - אתם מתמודדים עם זה דרך פקודות, אשר הם חלק מארכיטקטורת האפליקציה שלכם. לצרכי למידה, בואו נבדוק איך אתם יכולים לחקות רישום לוג על ידי שליחת הודעות ל-JavaScript דרך ports.

ראשית, תגדירו מודול port:

```Elm
port module Logger exposing (..)

-- להגדיר port ששולח לוגים ל-JavaScript
port log : String -> Cmd msg
```

ב-`Main.elm` שלכם, תשתמשו ב-port `log` כדי לשלוח הודעת לוג:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- עדכונים מסוימים לדגם שלכם כאן
            ( updatedModel, log "אירוע AnEvent התרחש." )

        AnotherEvent ->
            -- עדכוני דגם אחרים כאן
            ( anotherUpdatedModel, log "אירוע AnotherEvent התרחש." )
```

בצד ה-JavaScript, תירשמו ל-port `log` כדי להתמודד עם הודעות הלוג הנכנסות:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

דוגמה לפלט בקונסול ה-JavaScript תהיה אז:

```
אירוע AnEvent התרחש.
אירוע AnotherEvent התרחש.
```

## הבנה עמוקה יותר
מסורתית, בשפות כמו Python או Java, רישום לוג מתבצע באמצעות שימוש בספריית לוגים, המספקת ממשק API פשוט לרישום הודעות ברמות שונות כגון debug, info, warning, error, ו-critical.

Elm, עם דגשה על טהרות ובלתי משתנות, אינה מספקת רישום ישיר כזה, כיוון שכל פעולה של קלט/פלט או אפקט לצדדי מנוהלת באופן ברור דרך ארכיטקטורת Elm.

כאשר אתם זקוקים לתכונות לוג מלאות ב-Elm, בדרך כלל מסתמכים על כלים חיצוניים של JavaScript. Ports, כפי שהוצג לעיל, הם הגשר לכלים אלה. המודול Debug הוא עוד אופציה, אבל הוא מיועד לשימוש בפיתוח בלבד ולא לרישום בסביבת פרודקשן.

בנוסף ל-ports, מתכנתים לעיתים קרובות משתמשים בהודעות המהדר של Elm ובאפשרויות דיבאגינג בזמן ריצה, כמו `Debug.log`, אשר אתם יכולים להכניס לקוד שלכם לעקוב אחר ערכים. זה מכסה על ביטוי ומתעד את הפלט שלו לקונסול כך:

```Elm
view model =
    Debug.log "Model Debug" model
    -- קוד התצוגה שלכם פה
```

זה עם זאת גם אינו מיועד לפרודקשן. כלים כמו elm-logger מספקים כמה רמות עיטוף מעל ports לצורך רישום לוג, אם כי גם אלה מיועדים יותר לפיתוח מאשר לפרודקשן.

## ראה גם
- Elm ports: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- דיון ב-Elm על לוגינג: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- elm-logger package: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
