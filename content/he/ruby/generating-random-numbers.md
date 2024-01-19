---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# מספרים אקראיים ב-Ruby
## מהו ולמה?
חילוף מספרים אקראיים הוא תהליך שבו אתה מייצר מספרים אינם משתנים באופן צפוי. תכנתים משתמשים בכך ליצירת נתונים מבחנים, אנימציה אקראית ומשחקים.

## איך לעשות את זה:
להפקת מספר אקראי ב-Ruby, אתה יכול להשתמש בפיתח לשם מספרים אקראיים. להלן דוגמה:
```Ruby
random_number = rand(1..100)
puts random_number
```

כאן, הפונקציה `rand()` מחזירה מספר אקראי בטווח 1 עד 100. התוצאה תהיה מספר אקראי שונה בכל פעם שאתה מריץ את התוכנית.

## בחפיפה מעמיקה:
חילוף מספרים אקראיים הוא נושא שהיה עם המחשבים מאז ומתמיד. ישנם אלגוריתמים מורכבים שמשתמשים במספרים אמיתיים אקראיים, וישנם אלגוריתמים פשוטים יותר כמו `rand()` של Ruby. על-אף שמנגנונים אלה נקראים "מספרים אקראיים", הם למעשה "פסאודו-אקראיים" מאחר והם מייצרים סדרת מספרים שמשתנה בהתאם לנתונים שנקלטים בהתחלה (הזרע). על כן, עם הזרע המדויק, המנגנון יייצר את אותה הסדרה שוב ושוב.

עם התפתחות שפת התכנות Ruby, נוספו פונקציות נוספות כמו `srand` לשימוש במספרים אקראיים. `srand` מאפשר לך לקבוע את הזרע עבורה, מה שיכול להיות שימושי לדוגמה, לבדיקות אוטומטיות שצריכות מספרים "אקראיים" עקביים.

## ראה עוד:
* [Random number generation in Ruby](http://www.ruby-doc.org/core-2.7.0/Random.html)
* [Seeding in Ruby](http://ruby-doc.org/core-2.7.0/Random.html#method-c-srand)
* [Pseudorandom numbers in computing](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)