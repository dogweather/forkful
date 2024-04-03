---
date: 2024-01-20 17:34:44.125198-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.188165-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

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
