---
date: 2024-01-20 17:39:13.652217-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D4\u05D9\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05E9\u05E0\u05D4 \u05D0\u05EA\
  \ \u05DB\u05DC \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05DC\u05D2\u05E8\u05E1\u05EA\u05DD \u05D4\u05E7\u05D8\u05E0\u05D4\
  . \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA\
  \ \u05EA\u05D5\u05D5\u05D9\u05DD \u05D0\u05D7\u05D9\u05D3\u05D5\u05EA, \u05DC\u05DE\
  \u05E9\u05DC \u05D1\u05E2\u05EA \u05E2\u05D9\u05D1\u05D5\u05D3\u2026"
lastmod: '2024-03-13T22:44:40.179975-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D4\u05D9\u05D0\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05E9\u05E0\u05D4 \u05D0\u05EA \u05DB\
  \u05DC \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05DC\u05D2\u05E8\u05E1\u05EA\u05DD \u05D4\u05E7\u05D8\u05E0\u05D4. \u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\
  \u05D4\u05D1\u05D8\u05D9\u05D7 \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA \u05EA\
  \u05D5\u05D5\u05D9\u05DD \u05D0\u05D7\u05D9\u05D3\u05D5\u05EA, \u05DC\u05DE\u05E9\
  \u05DC \u05D1\u05E2\u05EA \u05E2\u05D9\u05D1\u05D5\u05D3\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא פעולה שמשנה את כל התווים במחרוזת לגרסתם הקטנה. תוכניתנים עושים זאת לעיתים כדי להבטיח השוואות תווים אחידות, למשל בעת עיבוד קלט משתמש או עבודה עם נתוני טקסט.

## איך לעשות:
ברובי, תוכל להמיר מחרוזת לאותיות קטנות בעזרת המתודה `.downcase`. קח לדוגמא:

```ruby
original_string = "HeLLo WOrLD!"
lowercase_string = original_string.downcase

puts lowercase_string  # יציג "hello world!"
```

חלופה היא שימוש במתודה `.downcase!`, שמשנה את המחרוזת המקורית ישירות:

```ruby
mutable_string = "HeLLo WOrLD!"
mutable_string.downcase!

puts mutable_string  # יציג גם "hello world!"
```

## עיון נוסף:
לפעולה של המרה לאותיות קטנות יש היסטוריה עשירה בתחום התוכנה. זה נעשה כדי להקל על השוואות תווים וחיפושים קייס-אינסנסיטיב. בשפות תכנות אחרות, יש פונקציות דומות, כמו `.toLowerCase()` ב-JavaScript.

ב-Ruby, שימוש ב-`.downcase` מעניק יתרון במיוחד עם יוניקוד (Unicode), כי הוא טופל באופן חכם להשוות אותיות בלי להתחשב בגודלן. אבל תיזהר: שימוש ב-`.downcase!` יכול להיות מסוכן כי הוא ישנה את המחרוזת המקורית שאתה עובד איתה. רק תשתמש בזה אם אתה בטוח שזו התנהלות הרצויה.

## לקרוא גם:
- [Ruby-Doc המידע הרשמי על downcase](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase)
- [עמוד ויקיפדיה על Case Sensitivity](https://en.wikipedia.org/wiki/Case_sensitivity)
- [עוד על Unicode Case Mapping ברובי](https://ruby-doc.org/core-3.1.2/String.html#method-i-unicode_normalize)
