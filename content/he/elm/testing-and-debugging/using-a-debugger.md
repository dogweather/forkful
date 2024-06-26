---
date: 2024-01-26 03:50:13.791180-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC-Elm \u05D0\
  \u05D9\u05DF \u05DE\u05E0\u05EA\u05D7 \u05D1\u05D0\u05D2\u05D9\u05DD \u05DE\u05D5\
  \u05D1\u05E0\u05D4 \u05D1\u05DE\u05D5\u05D1\u05DF \u05D4\u05DE\u05E1\u05D5\u05E8\
  \u05EA\u05D9 \u05DB\u05DE\u05D5, \u05DC\u05D3\u05D5\u05D2\u05DE\u05D4, \u05D1-JavaScript\
  \ \u05E2\u05DD \u05DB\u05DC\u05D9 \u05D4\u05E4\u05D9\u05EA\u05D5\u05D7 \u05D1\u05D3\
  \u05E4\u05D3\u05E4\u05DF. \u05E2\u05DD \u05D6\u05D0\u05EA, \u05E7\u05D4\u05D9\u05DC\
  \u05EA Elm \u05D1\u05E0\u05EA\u05D4 \u05DB\u05DC\u05D9\u05DD \u05DC\u05DE\u05D9\u05DC\
  \u05D5\u05D9 \u05D4\u05E4\u05E2\u05E8 \u05D4\u05D6\u05D4. \u05D4\u05E0\u05D4 \u05D0\
  \u05D9\u05DA\u2026"
lastmod: '2024-03-13T22:44:39.209593-06:00'
model: gpt-4-0125-preview
summary: "\u05DC-Elm \u05D0\u05D9\u05DF \u05DE\u05E0\u05EA\u05D7 \u05D1\u05D0\u05D2\
  \u05D9\u05DD \u05DE\u05D5\u05D1\u05E0\u05D4 \u05D1\u05DE\u05D5\u05D1\u05DF \u05D4\
  \u05DE\u05E1\u05D5\u05E8\u05EA\u05D9 \u05DB\u05DE\u05D5, \u05DC\u05D3\u05D5\u05D2\
  \u05DE\u05D4, \u05D1-JavaScript \u05E2\u05DD \u05DB\u05DC\u05D9 \u05D4\u05E4\u05D9\
  \u05EA\u05D5\u05D7 \u05D1\u05D3\u05E4\u05D3\u05E4\u05DF."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
weight: 35
---

## איך לעשות:
ל-Elm אין מנתח באגים מובנה במובן המסורתי כמו, לדוגמה, ב-JavaScript עם כלי הפיתוח בדפדפן. עם זאת, קהילת Elm בנתה כלים למילוי הפער הזה. הנה איך תוכלו להשתמש ב-`elm-debug-transformer` לניפוי באגים באפליקציה שלכם ב-Elm:

```Elm
-- התקנת elm-debug-transformer (חבילת Node)

1. npm install -g elm-debug-transformer

-- שימוש ב-elm-debug-transformer כדי להתחיל את האפליקציה שלכם

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

ברגע ש-`elm-debug-transformer` רץ, הוא יוצר חיבור WebSocket ללוגים. תוכלו לראות מידע לניפוי באגים בקונסול של הדפדפן שלכם, שם תוכלו לבדוק את מבני הנתונים של התוכנית שלכם בנקודות נתונות באפליקציה.

ב-Elm 0.19 ואילך, פונקציות במודול `Debug` כמו `Debug.log` ו-`Debug.todo` יכולות לעזור לכם לעקוב אחר ערכים ולסמן במפורש חלקים בקוד שטרם הושלמו. הנה איך להשתמש ב-Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Incrementing" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Decrementing" { model | count = model.count - 1 }, Cmd.none )
```

תראו הודעות "Incrementing" או "Decrementing" בקונסול של הדפדפן לצד המצב החדש של ה`model`.

## צלילה עמוקה
אוון צ'פליקי, יוצר Elm, שאף ליצור שפה שבה תקלות נפוצות יהיו בלתי אפשריות או קלות לתפיסה. מטרה זו היא הסיבה שליבת Elm אינה כוללת פונקציות ניפוי באגים מסורתיות. ניתוח סטטי והסקת סוגים ב-Elm תורמים רבות להפחתת שגיאות בזמן ריצה, מה שמפחית את הצורך בניפוי באגים מתקדם בזמן ריצה. אלטרנטיבות היסטוריות כללו שימוש ב-`elm-reactor` שהופיע והציע ניפוי בזמן עבור עקיבה אחורה - דרך לחזור ולהשחזר פעולות באפליקציה שלך.

כיום, כלים כמו `elm-debug-transformer` והשימוש במודול `Debug` של Elm עוזרים לגשר על הפער. על אף שמודול `Debug` מיועד לשימוש בעת הפיתוח בלבד ויש להסירו לפני בניות לפרודקשן, הוא כלי חשוב לזיהוי ולתיעוד של שינויי מצב.

זכרו שטכניקות ניפוי באגים מסורתיות ב-JavaScript, כמו נקודות עצירה או ביצוע צעד אחר צעד, אינן ישימות ישירות ב-Elm בשל מבנה התוכנית והעיבוד של עדכוני המצב על ידי זמן הריצה של Elm. Elm מעודדת אותך למבנה את התוכנית שלך בצורה שבה זרימת הנתונים ברורה ונעה לפי סוגים קפדניים והבטחות של אי שינויות, שמפחיתות את המקרים שבהם ניפוי באגים נדרש.

## ראה גם
- המדריך הרשמי של Elm על טיפול בחריגים בזמן ריצה: https://guide.elm-lang.org/error_handling/
- מאגר ה-GitHub של `elm-debug-transformer`: https://github.com/kraklin/elm-debug-transformer
- דיון בפורום של Elm על אסטרטגיות לניפוי באגים: https://discourse.elm-lang.org/c/show-and-tell/debugging
- תיעוד המודול `Debug` של Elm: https://package.elm-lang.org/packages/elm/core/latest/Debug
