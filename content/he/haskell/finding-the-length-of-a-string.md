---
title:                "מציאת אורך של מחרוזת"
html_title:           "Haskell: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה

כאשר אתם כותבים תוכניות ב-Haskell, לפעמים תצטרכו לדעת את אורך המחרוזת שלכם. אולם, זהו אחד הרכיבים הבסיסיים ביותר של שפת התכנות הזו, ולכן כדאי לדעת לאבחן אותו.

## איך לעשות זאת

הנה דוגמה קצרה על איך למצוא את אורך המחרוזת ב-Haskell, וגם את הפלט שתקבלו:

```Haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

length "Hello!" -- יחזיר 6
```

כמו שאתם יכולים לראות, ישנם שני חלקים לפונקציה זו. החלק הראשון מגדיר את טיפוס הנתונים שהפונקציה מצפה לקבל, שהוא רשימה מסוג a. החלק השני מדגיש את הפונקציה עצמה, שעוברת על הרשימה באמצעות חזרה על כל פריט ומחזירה את האורך הסופי של הרשימה.

## למציאת נתונים רחבים יותר

עכשיו שאתם יודעים איך למצוא את אורך המחרוזת באמצעות פונקציות מובנות ב-Haskell, אתם רוצים לדעת מה לעשות נתונים רחבים יותר. אתם עשויים להשתמש בפונקציות נוספות שזמינות בשפת התכנות הזו, כגון "repeat" שיכול לאפשר לכם ליצור רשימה חדשה של אותו גודל פעמיים, או "take" שיכול לקחת פריטים ראשונים מכל רשימה.

## ראו גם

* [התיעוד הרשמי של Haskell](https://www.haskell.org/documentation/)
* [מדריך ללמידת Haskell](https://learn.hfm.io/)
* [ערוץ הוובינרים של Haskell](https://www.youtube.com/user/HaskellWebinars)