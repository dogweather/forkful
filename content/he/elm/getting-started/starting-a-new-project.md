---
date: 2024-01-20 18:03:34.642730-07:00
description: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\
  \ \u05D7\u05D3\u05E9 \u05D1-Elm \u05D4\u05D9\u05D0 \u05D4\u05E7\u05DE\u05EA \u05E1\
  \u05D1\u05D9\u05D1\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05DC\u05DB\u05EA\
  \u05D9\u05D1\u05EA \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D4. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05D0\u05E4\u05DC\u05D9\u05E7\
  \u05E6\u05D9\u05D5\u05EA \u05D5\u05D5\u05D1 \u05E2\u05D3\u05D9\u05E0\u05D5\u05EA\
  \ \u05D1\u05E9\u05E4\u05D4 \u05D8\u05D4\u05D5\u05E8\u05D4 \u05D5\u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05EA."
lastmod: '2024-03-13T22:44:39.203586-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9 \u05D1-Elm \u05D4\u05D9\u05D0 \u05D4\u05E7\u05DE\u05EA \u05E1\u05D1\
  \u05D9\u05D1\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05DC\u05DB\u05EA\u05D9\
  \u05D1\u05EA \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D4."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
weight: 1
---

## איך לעשות:
כדי להתחיל פרויקט חדש ב-Elm, בצע את הפקודות הבאות:

```Elm
-- התקן את ארגז הכלים של Elm (אם טרם הותקן)
npm install -g elm

-- צור פרויקט חדש
elm init

-- זה ייווצר קבצים וספריות בסיסיים
-- src/
-- elm.json
```

אחרי הפקודה `elm init` ייציר קבצים עם מבנה התיקיות הרגיל לפרויקט Elm. כתוב ב-`src/Main.elm` את הקוד הבא:

```Elm
module Main exposing (main)

import Html exposing (text)

-- התוכנית הראשונית שלך: מדפיסה "שלום עולם!"
main =
  text "שלום עולם!"
```

כדי להיכנס לפריסה במצב פיתוח ולראות את התוצאה על הדפדפן, הרץ:
```Elm
elm reactor
```
גש ל- `http://localhost:8000` ובחר את הקובץ `src/Main.elm` לתצוגה.

## צלילה עמוקה:
Elm הוא שפת תכנות פונקציונלית לפיתוח אפליקציות ווב. הוא נוצר על ידי Evan Czaplicki כחלק מפרויקט התיזה שלו ב-2012. הפונקציונליות של חווית המשתמש והביצועים ב-Elm עדיפים על רוב הטכנולוגיות האחרות כמו JavaScript.

Elm משתמש בארכיטקטורת The Elm Architecture, שהיא דפוס לבניית אפליקציות בצורה נקייה ומבנית. היא דומה במידת מה ל-Redux בעולם של React, אך היא כלולה באופן טבעי ב-Elm.

על אף שישנן חלופות נפוצות כמו React, Vue, או Angular ב-JavaScript, Elm מספקת יתרון בטיחותי בזכות מערכת הטיפוסים החזקה והקומפילציה ל-JavaScript, שמבטלים קטגוריה שלמה של באגים בזמן פיתוח.

## גם ראה:
- [Elm Guide](https://guide.elm-lang.org/) - המדריך הרשמי של Elm למתחילים.
- [Elm Package Catalog](https://package.elm-lang.org/) - קטלוג החבילות הרשמי למציאת חבילות וישומים נתמכים.
- [Elm Discourse](https://discourse.elm-lang.org/) - פורום הדיונים של Elm, מקום טוב לפניות מהקהילה.
