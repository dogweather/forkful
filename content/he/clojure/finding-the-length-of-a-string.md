---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:20.372867-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
מה ולמה?
למצוא את אורך המחרוזת זה לקבוע כמה תווים יש בה. תכניתנים עושים את זה כדי לבדוק נתונים, לעבד טקסט, ולבצע ולידציות.

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
