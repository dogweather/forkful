---
title:                "חילוץ מחרוזות משניות"
html_title:           "Haskell: חילוץ מחרוזות משניות"
simple_title:         "חילוץ מחרוזות משניות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

החילוץ של תת-מחרוזת הוא כלי חיוני בכתיבת קוד בפונקציונליות כמו Haskell. באמצעות חילוץ תת-מחרוזת, ניתן לעבוד על חלקים מסוימים במחרוזת ולשנותם בקלות, מה שמביא לקוד יותר מקודם וקלות בתחזוקה.

## איך לעשות זאת

כדי לחלץ תת-מחרוזת ב-Haskell, יש להשתמש בפונקציה `take` ו-`drop` יחד עם האורך הנדרש של התת-מחרוזת. לדוגמה, אם ננתח את המחרוזת "Hello World", נוכל לחלץ תת-מחרוזת מהאות "W" והלאה באמצעות הפקודה הבאה:

```Haskell
take 6 (drop 6 "Hello World")
```

פלט: `World`

כך ניתן לחלץ חלקים שונים של המחרוזת לפי הצורך ולשנותם בקלות.

## טופס עמוק

החילוץ של תת-מחרוזת ב-Haskell יכול להיות יותר מורכב ממה שנראה במבט ראשון. ניתן להשתמש בפונקציות נוספות כמו `substring` ו-`splitOn` לחילוץ תת-מחרוזת עם יותר מהפרמטר אחד. כמו כן, הפקודה `takeWhile` מאפשרת לחלץ תת-מחרוזת עד מצב מסוים במחרוזת.

## ראה גם

- [Haskell פונקציות מובנות שימושיות למתכנתים פונקציונליים](https://medium.com/@kurtisboutelle/top-10-things-haskell-has-that-you-can-use-too-daf9872e1c9)

- [A Gentle Introduction to Haskell: 8. Lists and Tuples](http://www.cis.syr.edu/~royer/433/Lectures/IntroHaskell/lists.html)