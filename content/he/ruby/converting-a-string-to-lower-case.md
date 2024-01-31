---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:39:13.652217-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/converting-a-string-to-lower-case.md"
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
