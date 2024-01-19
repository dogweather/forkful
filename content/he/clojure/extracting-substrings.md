---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
קיצור תת-מחרוזות הוא הפעולה שבה אנו מחלצים מחרוזת מתוך מחרוזת אחרת. מתכנתים בוחרים לבצע את זה כאשר רק חלק מהמידע שמטמין במחרוזת נדרש לצורך האלגוריתם. 

## כיצד ל:
קלוז'ר מספקת את הפונקציה substring בשתי הגרסאות שלה:

בחלק הראשון, קיצור מחרוזת יתבצע מהאינדקס שנמסר עד לסוף המחרוזת.
```Clojure
(subs "Hello, World!" 7)
```
פלט:
```Clojure
"World!"
```
בחלק השני, ניתן לספק גם אינדקס סופי של המחרוזת שאנו רוצים לחתוך.
```Clojure
(subs "Hello, World!" 7 12)
```
פלט:
```Clojure
"World"
```

## צלילה עמוקה
הפונקציה subs של Clojure מושטתה של הפונקציה substring ב-Java Shring, שנוצרה מתחילה ב-Java 1.0. שינויים לעומת החלפה של מחרוזות, substring לא ישנה את המחרוזת המקורית. ייתכנו שיטות אחרות לחליצת מחרוזות, כמו למשל בהשתמש ברגולר אקספרשנים או בעזרת אלגוריתמים המשלבים מאפיינים אחרים של המחרוזת.

## ראה גם
ביקור בדף [Clojure](https://clojure.org/) יגביר את המובנה שלך של התרגיל החישובי, והמאמר [Working with Strings in Clojure](https://clojure.org/guides/learn/strings) ימחיש את העוצמה של עבודה עם מחרוזות ב-Clojure.