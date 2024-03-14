---
date: 2024-01-26 04:46:27.894296-07:00
description: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD, \u05D4\u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\u05DD \u05DE\u05D7\u05DC\u05E7\
  \ \u05DE\u05DE\u05E9\u05D9 \u05D5\u05D7\u05DC\u05E7 \u05D3\u05DE\u05D9\u05D5\u05E0\
  \u05D9 (\u05DB\u05DE\u05D5 3+4i), \u05D4\u05DD \u05E2\u05DE\u05D5\u05D3 \u05D4\u05E9\
  \u05D3\u05E8\u05D4 \u05D1\u05D4\u05E0\u05D3\u05E1\u05D4 \u05D5\u05D1\u05E4\u05D9\
  \u05D6\u05D9\u05E7\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05D1\u05D3\u05D9\u05DD \u05D0\u05D9\u05EA\u05DD \u05D1\u05E1\u05D9\u05DE\u05D5\
  \u05DC\u05E6\u05D9\u05D5\u05EA, \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5\u05EA\
  \u05D5\u05EA \u05D5\u05D1\u05E4\u05EA\u05E8\u05D5\u05DF \u05DE\u05E9\u05D5\u05D5\
  \u05D0\u05D5\u05EA \u05E9\u05DC\u05D0\u2026"
lastmod: '2024-03-13T22:44:40.190102-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD, \u05D4\u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\u05DD \u05DE\u05D7\u05DC\u05E7\
  \ \u05DE\u05DE\u05E9\u05D9 \u05D5\u05D7\u05DC\u05E7 \u05D3\u05DE\u05D9\u05D5\u05E0\
  \u05D9 (\u05DB\u05DE\u05D5 3+4i), \u05D4\u05DD \u05E2\u05DE\u05D5\u05D3 \u05D4\u05E9\
  \u05D3\u05E8\u05D4 \u05D1\u05D4\u05E0\u05D3\u05E1\u05D4 \u05D5\u05D1\u05E4\u05D9\
  \u05D6\u05D9\u05E7\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05D1\u05D3\u05D9\u05DD \u05D0\u05D9\u05EA\u05DD \u05D1\u05E1\u05D9\u05DE\u05D5\
  \u05DC\u05E6\u05D9\u05D5\u05EA, \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5\u05EA\
  \u05D5\u05EA \u05D5\u05D1\u05E4\u05EA\u05E8\u05D5\u05DF \u05DE\u05E9\u05D5\u05D5\
  \u05D0\u05D5\u05EA \u05E9\u05DC\u05D0\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מרוכבים, המורכבים מחלק ממשי וחלק דמיוני (כמו 3+4i), הם עמוד השדרה בהנדסה ובפיזיקה. מתכנתים עובדים איתם בסימולציות, עיבוד אותות ובפתרון משוואות שלא "מתחברות" עם מספרים ממשיים בלבד.

## איך לעשות:
Ruby מקל על הטיפול במספרים מרוכבים. ניתן ליצור ולתפעל אותם באמצעות המחלקה Complex:

```ruby
require 'complex'

# יצירת מספרים מרוכבים
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# פעולות בסיסיות
sum = c1 + c2               # => (5.0+9.0i)
difference = c1 - c2        # => (1.0-1.0i)
product = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# הצמדה, גודל ושלב
conjugate = c1.conjugate    # => (3.0-4.0i)
magnitude = c1.abs          # => 5.0
phase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 רדיאנים

# שיטות ספציפיות למספרים מרוכבים
polar = c1.polar            # => [5.0, 0.9272952180016122]
rectangular = c1.rect       # => [3.0, 4.0]
```

## עיון מעמיק
מספרים מרוכבים אינם חדשים - הם קיימים מהמאה ה-16, ופותרים משוואות ללא פתרונות ממשיים. מלבד המתמטיקה, מבחינה חישובית, מחלקת ה-Complex של Ruby עושה את העבודה הכבדה, בתמיכה במודול Math לפונקציות טריגונומטריות וטרנסצנדנטליות.

שפות תכנות קודמות דרשו טיפול ידני של החלקים הממשיים והדמיוניים. חלק מהן, כמו Fortran ו-C++, משתמשות בספריות מיוחדות לחישובים מרוכבים.

הגישה של Ruby משלבת תמיכה במספרים מרוכבים בתחביר שלה, וחוסכת ממך את הצורך להמציא את הגלגל מחדש. בכללי, מחלקת ה-Complex טופלת את המתמטיקה, בעוד Ruby דואגת לאינטראקציות בין האובייקטים.

## ראה גם
- מסמכי Ruby על Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- התיאור של MathWorld על מספרים מרוכבים: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- הקדמה חזותית למספרים מרוכבים והסיבה לשימושיות שלהם: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
