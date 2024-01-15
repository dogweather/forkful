---
title:                "ניצון מחרוזת בתוכנות מחשב"
html_title:           "Haskell: ניצון מחרוזת בתוכנות מחשב"
simple_title:         "ניצון מחרוזת בתוכנות מחשב"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

מה הנוכחות של הפונקציה capitalize בשפת Haskell ולמה כדאי לנו להשתמש בה? פשוט - אין דבר כזה נסיבות "קריאות" יותר משאר השמות בקוד, כך שכולם יכולים לפגוש תווים ריקים, טעויות כתיב ועוד. עם capitalize, כל מה שעליך לעשות הוא להזין מחרוזת והפונקציה תחזיר לך את התווים במחרוזת הראשונים כתווים גדולים.

## איך להשתמש

תחילה, נטען את הפונקציה capitalize על ידי לחיצה על Ctrl + L במצב הפיתוח שלנו.

```Haskell
import Data.Char (toUpper)
```

את המחרוזת שברצונך להקפיטליזציה ניתן לכתוב לפני הפונקציה בתוך סוגריים, כך:

```Haskell
capitalize "hello world"
```

והפלט יהיה:

```Haskell
"Hello world"
```

כעת ננסה להקפיטליזציה של מחרוזת המכילה טעויות כתיב:

```Haskell
capitalize "tHiS sTrInG hAs SoMe ErRoRs"
```

והתוצאה היא:

```Haskell
"This string has some errors"
```

## Deep Dive

הפונקציה capitalize היא חלק מספר פונקציות מובנות של המודול Data.Char, המכיל מספר פונקציות המתייחסות לתווים ולתחביר שלהם במחרוזות. במקרה של capitalize, הפונקציה משנה את תחילת המחרוזת לתווים גדולים, ויתכן גם ליישם בעזרת loop או פעולות נוספות על המחרוזת המקורית.

## ראה גם

- [מודול Data.Char ב-Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [פונקציות נוספות לטיפול במחרוזות ב-Haskell](https://www.haskell.org/tutorial/strings.html)