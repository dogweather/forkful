---
date: 2024-01-20 17:38:52.826337-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D0\u05DC\
  \u05DD \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DE\u05D0\u05D5\u05D3. \u05EA\u05E9\
  \u05EA\u05DE\u05E9\u05D5 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `String.toLower`\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05DB\u05DC \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05E9\u05D1\u05E8\u05E6\u05D5\u05E0\u05DB\u05DD."
lastmod: '2024-03-13T22:44:39.180332-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D0\u05DC\u05DD \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DE\u05D0\
  \u05D5\u05D3."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## איך לעשות:
באלם זה פשוט מאוד. תשתמשו בפונקציה `String.toLower` כדי להמיר כל מחרוזת שברצונכם.

```Elm
import String

lowercaseString : String -> String
lowercaseString str =
  String.toLower str

-- דוגמא לשימוש:
result : String
result =
  lowercaseString "HELLO, WORLD!"

-- תוצאה:
-- "hello, world!"
```

## עיון מעמיק
המרה לאותיות קטנות היא משימה סטנדרטית בתכנות, ואפשר היה למצוא אותה כבר בשפות עתיקות כמו לימוד שפת מ"א. ישנם חלופות כמו השימוש בביטויים רגולריים או פונקציות עזר מותאמות אישית, אבל באלם, 'String.toLower' היא פונקציה פשוטה וישירה שעושה את העבודה. מאחורי הקלעים, הפונקציה חייבת לכבד כללים של השפה, כמו אותיות גדולות בשפות שונות וסימנים מיוחדים.

## ראו גם
- תיעוד הפונקציה `String.toLower` באלם: [https://package.elm-lang.org/packages/elm/core/latest/String#toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- מדריך לעבודה עם טקסט ומחרוזות בסטנדרטים שונים.
