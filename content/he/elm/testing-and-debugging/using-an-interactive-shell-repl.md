---
date: 2024-01-26 04:14:01.812445-07:00
description: "\u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D7\
  \u05D9\u05E9\u05D5\u05D1-\u05D4\u05D3\u05E4\u05E1\u05D4 (REPL) \u05D4\u05D9\u05D0\
  \ \u05E1\u05D1\u05D9\u05D1\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA \u05D5\u05E4\u05E9\u05D5\u05D8\
  \u05D4 \u05E9\u05DE\u05E7\u05D1\u05DC\u05EA \u05E7\u05DC\u05D8\u05D9\u05DD \u05D9\
  \u05D7\u05D9\u05D3\u05D9\u05DD \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9, \u05DE\
  \u05E2\u05E8\u05D9\u05DB\u05D4 \u05D0\u05D5\u05EA\u05DD, \u05D5\u05DE\u05D7\u05D6\
  \u05D9\u05E8\u05D4 \u05D0\u05EA \u05D4\u05EA\u05D5\u05E6\u05D0\u05D4 \u05DC\u05DE\
  \u05E9\u05EA\u05DE\u05E9. \u05EA\u05DB\u05E0\u05EA\u05D9 Elm \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.205198-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D7\u05D9\
  \u05E9\u05D5\u05D1-\u05D4\u05D3\u05E4\u05E1\u05D4 (REPL) \u05D4\u05D9\u05D0 \u05E1\
  \u05D1\u05D9\u05D1\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA \u05D5\u05E4\u05E9\u05D5\u05D8\u05D4\
  \ \u05E9\u05DE\u05E7\u05D1\u05DC\u05EA \u05E7\u05DC\u05D8\u05D9\u05DD \u05D9\u05D7\
  \u05D9\u05D3\u05D9\u05DD \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9, \u05DE\u05E2\
  \u05E8\u05D9\u05DB\u05D4 \u05D0\u05D5\u05EA\u05DD, \u05D5\u05DE\u05D7\u05D6\u05D9\
  \u05E8\u05D4 \u05D0\u05EA \u05D4\u05EA\u05D5\u05E6\u05D0\u05D4 \u05DC\u05DE\u05E9\
  \u05EA\u05DE\u05E9."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך לעשות:
Elm אינו מגיע עם REPL משולב. עם זאת, אתה יכול להשתמש ב-`elm repl` משורת הפקודה שלך כדי להתחיל סשן Elm לאחר התקנת Elm.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

בסשן זה, לאחר ייבוא פונקציות הרשימה, כפלנו את המספרים ברשימה וקיבלנו את התוצאה מיידית.

## צלילה עמוקה
REPL של Elm עשוי להיראות מוגבל לעומת אלה של שפות אחרות כמו Python או JavaScript, מכיוון ש-Elm היא שפה מהודרת המתמקדת בייצור אפליקציות רשת. באופן היסטורי, Elm התמקדה באפליקציות מלאות יותר מאשר בתסריטים או באינטראקציות עם מעטפת פקודה.

חלופות ל-REPL של Elm כוללות את `elm-live` ועורכים מקוונים כמו Ellie, שם אתה יכול לראות שינויים בקוד משתקפים בזמן אמת בדפדפן.

בקשר ליישום, REPL של Elm מהדר קטעי קוד של Elm ל-JavaScript ברקע, מאפשר לך להריץ את Elm באופן אינטראקטיבי. זה שונה מ-REPL של שפות מתורגמות, שלא דורשות את שלב ההידור הזה. בנוסף, REPL של Elm מצומצם כדי לשמור על השפה הליבה קלה וממוקדת.

## ראה גם
- המדריך הרשמי של Elm על אינטראקטיביות: https://guide.elm-lang.org/interop/
- Ellie, מגרש משחקים מקוון של Elm: https://ellie-app.com/new
- `elm-live`, שרת פיתוח גמיש ל-Elm: https://www.elm-live.com/
