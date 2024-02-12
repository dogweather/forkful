---
title:                "מציאת אורך מחרוזת"
aliases:
- he/haskell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:00.098169-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

מציאת אורך של מחרוזת היא פעולה שמחזירה את מספר התווים שבה. תוכניתנים עושים זאת כדי לבדוק גודל, לאמת קלט, או לבצע חיתוך והרחבה של נתונים.

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
