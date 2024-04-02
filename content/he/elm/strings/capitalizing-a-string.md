---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:25.344704-07:00
description: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05E8\u05D0\u05E9\u05D9\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05D4\u05EA\u05D5 \u05D4\
  \u05E8\u05D0\u05E9\u05D5\u05E0\u05D9 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05E0\u05EA\u05D5\u05E0\u05D4 \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\
  \u05D9\u05E9\u05D9\u05D5\u05EA, \u05EA\u05D5\u05DA \u05E9\u05DE\u05D9\u05E8\u05D4\
  \ \u05E2\u05DC \u05D9\u05EA\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA, \u05DC\u05E2\u05D9\
  \u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DC\u05E6\u05D5\u05E8\
  \u05DA \u05E2\u05D9\u05E6\u05D5\u05D1 \u05DE\u05EA\u05D5\u05E7\u05E0\u05DF\u2026"
lastmod: '2024-03-13T22:44:39.173863-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05E8\u05D0\u05E9\u05D9\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05D4\u05EA\u05D5 \u05D4\u05E8\
  \u05D0\u05E9\u05D5\u05E0\u05D9 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E0\
  \u05EA\u05D5\u05E0\u05D4 \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D9\
  \u05E9\u05D9\u05D5\u05EA, \u05EA\u05D5\u05DA \u05E9\u05DE\u05D9\u05E8\u05D4 \u05E2\
  \u05DC \u05D9\u05EA\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05D0\u05D5\
  \u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA, \u05DC\u05E2\u05D9\u05EA\
  \u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DC\u05E6\u05D5\u05E8\u05DA\
  \ \u05E2\u05D9\u05E6\u05D5\u05D1 \u05DE\u05EA\u05D5\u05E7\u05E0\u05DF\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

## מה ולמה?

הפיכת מחרוזת לראשית גדולה כוללת המרה של התו הראשוני במחרוזת נתונה לאותיות רישיות, תוך שמירה על יתר התווים באותיות קטנות, לעיתים קרובות לצורך עיצוב מתוקנן או קריאות. מתכנתים ביצעו לעיתים קרובות משימה זו כדי להבטיח שהנתונים מוצגים באופן עקבי, במיוחד בממשקי משתמש או בעת עיבוד והצגת קלט משתמש.

## איך לעשות:

ב-Elm, אין פונקציה מובנית במיוחד להפיכת מחרוזות לראשית גדולה. עם זאת, אתה יכול להשיג זאת בקלות על ידי שימוש בפונקציות המודול `String` המובנות כגון `toUpper`, `toLower`, `left`, ו-`dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- דוגמה לשימוש
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- פלט: "Hello World"
```

לסיטואציות מורכבות יותר או אם אתה מעדיף להשתמש בספרייה המספקת דרך ישירה להפיכת מחרוזות לראשית גדולה, ייתכן שתשקול חבילה של צד שלישי כגון `elm-community/string-extra`. עם זאת, לפי העדכון האחרון שלי, האקוסיסטם של Elm מעודד טיפול במשימות כאלו באמצעות פונקציות מובנות כדי לשמור על השפה והפרויקטים רזים.

```elm
import String.Extra as StringExtra

-- במקרה שיש פונקציה `capitalize` בספריית צד שלישי
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- דוגמה לשימוש עם פונקציה היפותטית של ספרייה
main =
    "this is elm" |> capitalizeWithLibrary
    -- פלט היפותטי: "This is elm"
```

תמיד בדוק את מאגר החבילות של Elm עבור הספריות העדכניות והמועדפות ביותר למניפולציה של מחרוזות אם אתה מחפש פונקציונליות נוספת מעבר לספרייה הסטנדרטית.
