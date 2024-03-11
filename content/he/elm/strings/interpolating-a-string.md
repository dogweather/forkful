---
date: 2024-01-20 17:50:42.691577-07:00
description: "\u05D1\u05E2\u05D5\u05DC\u05DD \u05D4\u05EA\u05DB\u05E0\u05D5\u05EA\
  , \u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8\
  \ \u05D4\u05D8\u05DE\u05E2\u05D4 \u05E9\u05DC \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05D0\u05D5 \u05E2\u05E8\u05DB\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05D8\u05E7\
  \u05E1\u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05E9\u05E8\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\u05D9\u05D9\u05DD\
  \ \u05E2\u05DD \u05D8\u05E7\u05E1\u05D8 \u05E7\u05D1\u05D5\u05E2, \u05DB\u05DE\u05D5\
  \ \u05DC\u05D4\u05D3\u05E4\u05D9\u05E1\u2026"
lastmod: '2024-03-11T00:14:12.622497-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E2\u05D5\u05DC\u05DD \u05D4\u05EA\u05DB\u05E0\u05D5\u05EA, \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05D4\
  \u05D8\u05DE\u05E2\u05D4 \u05E9\u05DC \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05D0\
  \u05D5 \u05E2\u05E8\u05DB\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05D8\u05E7\u05E1\
  \u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05E9\u05E8 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\u05D9\u05D9\u05DD \u05E2\
  \u05DD \u05D8\u05E7\u05E1\u05D8 \u05E7\u05D1\u05D5\u05E2, \u05DB\u05DE\u05D5 \u05DC\
  \u05D4\u05D3\u05E4\u05D9\u05E1\u2026"
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)

בעולם התכנות, אינטרפולציה של מחרוזות זה פשוט הטמעה של משתנים או ערכים בתוך טקסט. תכניתנים עושים את זה כדי לקשר נתונים דינמיים עם טקסט קבוע, כמו להדפיס מסרים מותאמים אישית.

## How to (איך לעשות:)

ב-Elm, אתם משלבים אינטרפולציה בעזרת פונקציות. נגיד שכן, יש לכם שם של משתמש ורצים לברך אותו:

```Elm
greetUser : String -> String
greetUser userName =
    "שלום, " ++ userName ++ "!"
```

הרצת הפונקציה עם `"אלי"` תחזיר:

```Elm
"שלום, אלי!"
```

פשוט, נכון?

## Deep Dive (עומק התהום:)

ברוב שפות התכנות, אינטרפולציה היא לעשות את הנתונים לחלק מהמחרוזת בזמן ריצה. ב-Elm, אין לכם מנגנון אינטרפולציה מובנה כמו ב-JavaScript או Python; תצטרכו להשתמש ב-`++` לצירוף מחרוזות. לפעמים זה נראה מסורבל, אבל זה מחזיק אתכם במסלול הטיפוס הבטוח של Elm, ומונע בעיות. אם מחרוזת רבה יותר מדי, שקלו להשתמש ב-`String.concat` או ב-`List.map` עם פונקציה אונומית.

## See Also (ראו גם:)

- התיעוד הרשמי של Elm לקידוד מחרוזות: https://package.elm-lang.org/packages/elm/core/latest/String
- מדריך למיזוג מחרוזות ב-Elm: https://elmprogramming.com/model-update-view.html
- פורום Elm דיסקוס: https://discourse.elm-lang.org/ - אם יש לכם שאלות, זה מקום מעולה לשאול!
