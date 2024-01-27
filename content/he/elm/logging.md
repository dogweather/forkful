---
title:                "לוגים"
date:                  2024-01-26T01:03:56.080099-07:00
model:                 gpt-4-1106-preview
simple_title:         "לוגים"
programming_language: "Elm"
category:             "Elm"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/logging.md"
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