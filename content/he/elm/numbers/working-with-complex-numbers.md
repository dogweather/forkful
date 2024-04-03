---
date: 2024-01-26 04:40:00.467144-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Elm \u05D0\
  \u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA\
  \ \u05D1\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD, \u05D5\u05DC\u05DB\u05DF \u05EA\u05D9\u05E6\u05D5\u05E8\u05D5 \u05D0\u05EA\
  \ \u05E1\u05D5\u05D2 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\u05D4\u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05E9\u05DC\u05DB\u05DD. \u05D4\u05E0\
  \u05D4 \u05D4\u05EA\u05E7\u05E0\u05D4 \u05DE\u05D4\u05D9\u05E8\u05D4."
lastmod: '2024-03-13T22:44:39.191684-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Elm \u05D0\u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\
  \u05D1\u05E0\u05D9\u05EA \u05D1\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\
  \u05D5\u05DB\u05D1\u05D9\u05DD, \u05D5\u05DC\u05DB\u05DF \u05EA\u05D9\u05E6\u05D5\
  \u05E8\u05D5 \u05D0\u05EA \u05E1\u05D5\u05D2 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05D5\u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05E9\u05DC\
  \u05DB\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

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
