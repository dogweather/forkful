---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:50:01.079700-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
גילוי מספרים אקראיים הוא התהליך שבו יוצרים מספר שאינו ניתן לחזוי מראש. תכניתנים עושים שימוש בזה לשלל סיבות, כולל בדיקות תוכנה, משחקים, ואבטחת מידע.

## איך עושים את זה?
ברובי, יש לנו כמה דרכים פשוטות ליצור מספרים אקראיים:

```ruby
# יצירת מספר אקראי בין 0 ל-1
rand

# יצירת מספר שלם אקראי בין 0 ל-10
rand(11)

# יצירת מספר שלם אקראי בטווח של 20 עד 30
rand(20..30)

# בשימוש במחלקה Random
r = Random.new
r.rand(100) # מספר אקראי בין 0 ל-99
```

דוגמה לפלט:
```
0.4370615772814186
7
23
45
```

## צלילה לעומק
היום אנחנו משתמשים בחומרה ואלגוריתמים מתוחכמים לייצור מספרים אקראיים, אך במחשבים המוקדמים הדרך הייתה אתגר רציני. המחשבים הראשונים השתמשו באירועים חיצוניים כדי ליצור אקראיות. עם התפתחות החומרה יכולנו לחקות תופעות אקראיות באופן דיגיטלי.

אחת האלטרנטיבות למחוללים אקראיים פסבדו-אקראיים (מהשורה המנוהלת על ידי `rand` ו`Random.new`) היא שימוש באנטרופיה מחומרה כמו רעש תרמי או התנודות של ספקי כוח ליצירת מספרים אקראיים אמיתיים.

בפועל, מחוללי המספרים האקראיים מתבססים על פונקציות מתמטיות קשות להיפך, כאשר ה"זרע" (seed) משפיע על רצף התוצאות. כשמשתמשים בזרע קבוע, הרצף יהיה גם קבוע - שימושי לבדיקות תוכנה.

## קישורים רלוונטיים
- [Ruby-Doc org: Random class](https://ruby-doc.org/core-3.0.0/Random.html)
- [Ruby-Doc org: Kernel#rand](https://ruby-doc.org/core-3.0.0/Kernel.html#method-i-rand)
- [Stack Overflow: How do random number generators work?](https://stackoverflow.com/questions/4422079/how-do-random-number-generators-work)
- [Wikipedia: Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)