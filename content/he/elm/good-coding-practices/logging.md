---
aliases:
- /he/elm/logging/
date: 2024-01-26 01:03:56.080099-07:00
description: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05DC\u05D5\u05D2 \u05D4\u05D5\u05D0\
  \ \u05DC\u05DE\u05E2\u05E9\u05D4 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC\
  \ \u05EA\u05D9\u05E2\u05D5\u05D3 \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD \u05D5\
  \u05E4\u05DC\u05D8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA\
  \ \u05EA\u05D5\u05DB\u05E0\u05D4 \u05D1\u05E2\u05EA \u05E8\u05D9\u05E6\u05EA\u05D4\
  , \u05D7\u05E9\u05D1\u05D5 \u05E2\u05DC \u05D6\u05D4 \u05DB\u05E2\u05DC \u05D9\u05D5\
  \u05DE\u05DF \u05E9\u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05E8\
  \u05D9\u05E9\u05D5\u05DD \u05DC\u05D5\u05D2 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\
  \u05D5\u05D1 \u05D0\u05D7\u05E8 \u05DE\u05D4\u2026"
lastmod: 2024-02-18 23:08:52.760186
model: gpt-4-1106-preview
summary: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05DC\u05D5\u05D2 \u05D4\u05D5\u05D0 \u05DC\
  \u05DE\u05E2\u05E9\u05D4 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05EA\
  \u05D9\u05E2\u05D5\u05D3 \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD \u05D5\u05E4\
  \u05DC\u05D8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05EA\
  \u05D5\u05DB\u05E0\u05D4 \u05D1\u05E2\u05EA \u05E8\u05D9\u05E6\u05EA\u05D4, \u05D7\
  \u05E9\u05D1\u05D5 \u05E2\u05DC \u05D6\u05D4 \u05DB\u05E2\u05DC \u05D9\u05D5\u05DE\
  \u05DF \u05E9\u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05E8\u05D9\
  \u05E9\u05D5\u05DD \u05DC\u05D5\u05D2 \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\
  \u05D1 \u05D0\u05D7\u05E8 \u05DE\u05D4\u2026"
title: "\u05DC\u05D5\u05D2\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
רישום לוג הוא למעשה התהליך של תיעוד אירועים ופלט נתונים מתוך תוכנה בעת ריצתה, חשבו על זה כעל יומן של התוכנה. מתכנתים משתמשים ברישום לוג כדי לעקוב אחר מה שקורה מתחת למכסה - זה חיוני לאיתור באגים, ניטור התנהגות המערכת בזמן אמת וניתוח פעילות עבר לשיפור ביצועים או לבדיקות תקינות.

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
