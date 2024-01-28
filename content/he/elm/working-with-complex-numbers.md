---
title:                "עבודה עם מספרים מרוכבים"
date:                  2024-01-26T04:40:00.467144-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מרוכבים הם שילוב של מספרים ממשיים ודמיוניים, כמו `a + bi` כאשר `i` הוא השורש הריבועי של ‎-1‎. הם מהווים מפתח בתחומים כמו הנדסה ופיזיקה לפתרון בעיות שמספרים רגילים אינם יכולים לפתור.

## איך לעשות:
ב-Elm אין תמיכה מובנית במספרים מרוכבים, ולכן תיצורו את סוג הנתונים והפונקציות שלכם. הנה התקנה מהירה:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- דוגמא לשימוש:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum הוא { real = 4.0, imaginary = -2.0 }
```

## צלילה לעומק
בהיסטוריה, המספרים המרוכבים לא תמיד היו מקובלים. הם הפכו למשני משחק במאה ה-16 על מנת לפתור משוואות ממעלה שלישית. חלופות בשפות אחרות כמו Python מציעות תמיכה מובנית במספרים מרוכבים עם פעולות מוכנות ישירות מהקופסא. Elm דורשת גישה עשה זאת בעצמך כפי שראיתם. אך אפשר לעשות את זה מתוחכם ככל שנדרש, בונים כפל, חילוק, ופעולות נוספות, מטפלים בבעיות ביצועים.

## ראו גם
- התיעוד הרשמי של Elm: https://package.elm-lang.org/ ליצירת סוגי נתונים מותאמים אישית ולשליטה ביסודות Elm.
- חובבי היסטוריה של המתמטיקה יכולים לבדוק את "An Imaginary Tale" מאת Paul J. Nahin למסע של מספרים מרוכבים ברחבי הזמן.
- לצלול לאתגרי תכנות מוכווני מתמטיקה ב-Project Euler (https://projecteuler.net) כדי להפעיל את קסמי המספרים המרוכבים שלכם.
