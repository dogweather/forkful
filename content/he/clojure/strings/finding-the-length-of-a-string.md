---
date: 2024-01-20 17:47:20.372867-07:00
description: "How to: \u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05D3\
  \u05E8\u05DA \u05D4\u05E0\u05E4\u05D5\u05E6\u05D4 \u05D1\u05E7\u05DC\u05D5\u05D6\
  '\u05E8 \u05DC\u05DE\u05E6\u05D5\u05D0 \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `count`. \u05D3\u05D5\u05D2\u05DE\
  \u05D4."
lastmod: '2024-04-05T22:37:47.353543-06:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05D3\u05E8\u05DA\
  \ \u05D4\u05E0\u05E4\u05D5\u05E6\u05D4 \u05D1\u05E7\u05DC\u05D5\u05D6'\u05E8 \u05DC\
  \u05DE\u05E6\u05D5\u05D0 \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05D4\u05D9\u05D0 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `count`. \u05D3\u05D5\u05D2\u05DE\u05D4."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## How to:
איך לעשות:
הדרך הנפוצה בקלוז'ר למצוא אורך מחרוזת היא באמצעות הפונקציה `count`. דוגמה:

```Clojure
(def my-string "שלום עולם")
(count my-string) ; => 9
```

מחרוזת ריקה? קלי קלות:

```Clojure
(count "") ; => 0
```

## Deep Dive
עמק המידע:
בקלוז'ר, `count` יכולה לספור לא רק תווים במחרוזת, אלא גם פריטים בכל קולקציה. היסטורית, פונקציות ספירה היו בשימוש כבר בשפות תכנות מוקדמות. קיימות אלטרנטיבות ל-`count`, כמו `length` בשפות אחרות, אך בקלוז'ר `count` היא הדרך המקובלת. כשאתה קורא `count` על מחרוזת, הפונקציה סופרת באופן יעיל את התווים האוניקוד בה, ומחזירה את המספר.

## See Also
ראו גם:
- [Official Clojure Documentation for count](https://clojuredocs.org/clojure.core/count)
- [Clojure from the ground up: Sequences](https://aphyr.com/posts/302-clojure-from-the-ground-up-sequences)
- [ClojureScript Cheatsheet for count](http://cljs.info/cheatsheet/)
