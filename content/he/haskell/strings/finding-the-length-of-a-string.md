---
date: 2024-01-20 17:48:00.098169-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D0\u05D5\u05E8\
  \u05DA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA, \u05D1\u05D9\u05DE\u05D9\u05DD\
  \ \u05E9\u05DC \u05E9\u05E4\u05D5\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05DE\u05D5\
  \u05D3\u05E8\u05E0\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 Haskell, \u05D4\u05D5\u05D0\
  \ \u05E4\u05E9\u05D5\u05D8 \u05DC\u05DE\u05D3\u05D9. \u05D0\u05D1\u05DC \u05D1\u05E9\
  \u05E4\u05D5\u05EA \u05D9\u05E9\u05E0\u05D5\u05EA \u05D9\u05D5\u05EA\u05E8 \u05D0\
  \u05D5 \u05D1\u05E1\u05D1\u05D9\u05D1\u05D5\u05EA \u05E0\u05DE\u05D5\u05DB\u05D5\
  \u05EA \u05D9\u05D5\u05EA\u05E8, \u05DB\u05DE\u05D5 C, \u05D6\u05D4 \u05D4\u05D9\
  \u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05D3\u05E8\u05D5\u05E9\u2026"
lastmod: '2024-04-05T22:50:53.566975-06:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05D5\u05E8\u05DA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA, \u05D1\
  \u05D9\u05DE\u05D9\u05DD \u05E9\u05DC \u05E9\u05E4\u05D5\u05EA \u05EA\u05DB\u05E0\
  \u05D5\u05EA \u05DE\u05D5\u05D3\u05E8\u05E0\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5\
  \ Haskell, \u05D4\u05D5\u05D0 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05DE\u05D3\u05D9."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## איך לעשות:
```haskell
main :: IO ()
main = do
    let myString = "שלום עולם"
    print $ length myString
```

פלט דוגמא: 

```haskell
10
```

מערכת הספירה כוללת גם רווחים ותווי בקרה, ולא רק אותיות וספרות.

## עיון מעמיק
אורך המחרוזת, בימים של שפות תכנות מודרניות כמו Haskell, הוא פשוט למדי. אבל בשפות ישנות יותר או בסביבות נמוכות יותר, כמו C, זה היה יכול לדרוש סריקה לאורך כל המחרוזת כדי למצוא את סוף המחרוזת (נקודת העצירה הייתה התו '\0'). ב-Haskell, `length` היא פונקציה סטנדרטית בספריה Prelude שמחזירה את אורך כל רשימה, כולל מחרוזת שהיא רשימת תווים. פרטי מימוש - `length` מבצעת ספירה רקורסיבית של התווים במחרוזת. החסרון הוא שלפעולה יש סיבוכיות ליניארית - O(n).

בנוסף לפונקציית `length`, יש גם פונקציות אחרות שיכולות למדוד גדלים של נתחים מहמחרוזת או לבצע פעולות יעילות יותר עם סטרקטורות נתונים אחרות, כמו סידרה בתוך ספריה Data.Sequence, שיכולים לתת גישה לאורך בזמן קבוע – O(1).

## ראה גם
- [Haskell Documentation for Prelude](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html)
- [Haskell Wiki Book](https://en.wikibooks.org/wiki/Haskell)
- [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
