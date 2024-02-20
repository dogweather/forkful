---
date: 2024-01-26 04:40:00.467144-07:00
description: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD \u05D4\u05DD \u05E9\u05D9\u05DC\u05D5\u05D1 \u05E9\u05DC \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05DE\u05DE\u05E9\u05D9\u05D9\u05DD \u05D5\u05D3\u05DE\u05D9\
  \u05D5\u05E0\u05D9\u05D9\u05DD, \u05DB\u05DE\u05D5 `a + bi` \u05DB\u05D0\u05E9\u05E8\
  \ `i` \u05D4\u05D5\u05D0 \u05D4\u05E9\u05D5\u05E8\u05E9 \u05D4\u05E8\u05D9\u05D1\
  \u05D5\u05E2\u05D9 \u05E9\u05DC \u200E-1\u200E. \u05D4\u05DD \u05DE\u05D4\u05D5\u05D5\
  \u05D9\u05DD \u05DE\u05E4\u05EA\u05D7 \u05D1\u05EA\u05D7\u05D5\u05DE\u05D9\u05DD\
  \ \u05DB\u05DE\u05D5 \u05D4\u05E0\u05D3\u05E1\u05D4 \u05D5\u05E4\u05D9\u05D6\u05D9\
  \u05E7\u05D4 \u05DC\u05E4\u05EA\u05E8\u05D5\u05DF \u05D1\u05E2\u05D9\u05D5\u05EA\
  \u2026"
lastmod: 2024-02-19 22:04:58.415660
model: gpt-4-0125-preview
summary: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD \u05D4\u05DD \u05E9\u05D9\u05DC\u05D5\u05D1 \u05E9\u05DC \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05DE\u05DE\u05E9\u05D9\u05D9\u05DD \u05D5\u05D3\u05DE\u05D9\
  \u05D5\u05E0\u05D9\u05D9\u05DD, \u05DB\u05DE\u05D5 `a + bi` \u05DB\u05D0\u05E9\u05E8\
  \ `i` \u05D4\u05D5\u05D0 \u05D4\u05E9\u05D5\u05E8\u05E9 \u05D4\u05E8\u05D9\u05D1\
  \u05D5\u05E2\u05D9 \u05E9\u05DC \u200E-1\u200E. \u05D4\u05DD \u05DE\u05D4\u05D5\u05D5\
  \u05D9\u05DD \u05DE\u05E4\u05EA\u05D7 \u05D1\u05EA\u05D7\u05D5\u05DE\u05D9\u05DD\
  \ \u05DB\u05DE\u05D5 \u05D4\u05E0\u05D3\u05E1\u05D4 \u05D5\u05E4\u05D9\u05D6\u05D9\
  \u05E7\u05D4 \u05DC\u05E4\u05EA\u05E8\u05D5\u05DF \u05D1\u05E2\u05D9\u05D5\u05EA\
  \u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
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
