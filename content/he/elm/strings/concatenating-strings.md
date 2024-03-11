---
date: 2024-01-20 17:34:44.125198-07:00
description: "\u05D4\u05D3\u05D1\u05E7\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05DE\u05D7\
  \u05D1\u05E8\u05D9\u05DD \u05E9\u05EA\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05DC\u05D0\u05D7\u05EA. \u05EA\u05DB\
  \u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05E9\u05E4\u05D8\u05D9\
  \u05DD, \u05DC\u05E7\u05E8\u05D9\u05D0\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05D5\u05D7, \u05D0\u05D5 \u05DC\u05D4\
  \u05E6\u05D9\u05D2 \u05D8\u05E7\u05E1\u05D8 \u05D3\u05D9\u05E0\u05DE\u05D9."
lastmod: '2024-03-11T00:14:12.632641-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D3\u05D1\u05E7\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05DE\u05D7\
  \u05D1\u05E8\u05D9\u05DD \u05E9\u05EA\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05DC\u05D0\u05D7\u05EA. \u05EA\u05DB\
  \u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05E9\u05E4\u05D8\u05D9\
  \u05DD, \u05DC\u05E7\u05E8\u05D9\u05D0\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05D5\u05D7, \u05D0\u05D5 \u05DC\u05D4\
  \u05E6\u05D9\u05D2 \u05D8\u05E7\u05E1\u05D8 \u05D3\u05D9\u05E0\u05DE\u05D9."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הדבקת מחרוזות היא תהליך שבו מחברים שתי מחרוזות או יותר לאחת. תכנותים עושים זאת כדי ליצור משפטים, לקריאת נתונים בפורמט נוח, או להציג טקסט דינמי.

## איך לעשות:
```Elm
-- יצירת שתי מחרוזות
firstName : String
firstName = "משה"

lastName : String
lastName = "כהן"

-- הדבקת מחרוזות עם פונקציה ++
fullName : String
fullName = firstName ++ " " ++ lastName

-- דוגמה לפלט
-- "משה כהן"
```

## צלילה עמוקה
בעברית בתיכנות, ההדבקה יכולה להיות מורכבת עקב כיוון הכתיבה מימין לשמאל. ב-Elm, כמו ברוב השפות, ההדבקה נעשית בכיוון שמאל לימין, מה שעלול לדרוש תשומת לב מיוחדת בסביבה דו-לשונית. חלופה ל++ היא השימוש ב-fmt או ב-interpolate של ספריות חיצוניות עבור פורמט מורכב יותר של מחרוזות. ביצוע ההדבקה פנימית מתבצע בצורה יעילה כדי למזער ביצועים לא יעילים של איחוד מחרוזות.

## ראה גם
- [Elm String documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [String concatenation in functional programming](https://en.wikipedia.org/wiki/Functional_programming)
