---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרת מחרוזת לאותיות קטנות היא תהליך בו כל האותיות הגדולות במחרוזת מומרות לאותיות קטנות. תכנתים אכסנים וממשיבים מידע, אך הבדלים קטנים כמו "ABC" לעומת "abc" יכולים להיות בעיה - המרה לאותיות קטנות מבטיחה שלא תהיה שגיאה בגלל חוסר רגישות לרישיות.

## איך לעשות:
כאן הוא איך להמיר מחרוזת לאותיות קטנות ב-Clojure:

```clojure
(defn to-lower [s]
  (.toLowerCase s))
```

דוגמא לשימוש:

```clojure
(println (to-lower "HeLLo, World!"))  ;; הדפסה: "hello, world!"
```

## צלילה עמוקה:
את פונקצית "toLowerCase" הישנה מכלמת JVM, Clojure התאמה את הפונקציה וכעת משתמשת בה.
לא קיימות אלטרנטיבות מובנות אחרות לפונקציה '.toLowerCase', אך אפשר ליצור אחת באמצעות אופן הגישה לאינדיבידואלים במחרוזות והמרתם.
אבל שים לב - JVM יודעת לתמוך ב-RFCs ו-standards בשפות רבות, מה שיכול להיות מורכב ליישם באופן ידני.

## ראה גם:
- [תיעוד Clojure למתודה .toLowerCase](https://clojuredocs.org/clojure.core/lower-case)
- [הסבר עלמודל ה-string של JVM](https://stackoverflow.com/questions/4655193/java-utf-8-character-encoding)
- [תקנים ו-RFCs בנושא לוקאליזציה](https://www.ietf.org/rfc/rfc2277.txt)