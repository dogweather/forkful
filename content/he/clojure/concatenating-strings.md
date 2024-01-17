---
title:                "מחברים מחרוזות"
html_title:           "Clojure: מחברים מחרוזות"
simple_title:         "מחברים מחרוזות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?

שמירת מחרוזות היא תהליך שבו ניתן להפעיל מחרוזות כמילים אחת בתוך מחרוזת אחרת. בתור מתכנתים, נשתמש בפעולה זו כדי ליצור מחרוזות חדשות המשלבות מספר מחרוזות קיימות ותוכן נוסף.

## איך לעשות?

```clojure
(println (str "שלום " "לכולם!"))
```
פלט: "שלום לכולם!"

```clojure
(def string1 "אני לומד/ת ")
(def string2 "Clojure!")
(println (str string1 string2))
```
פלט: "אני לומד/ת Clojure!"

## חקירה מעמיקה

1. היסטוריה: הפעולה של שמירת מחרוזות הייתה קיימת מהנדס תוכנה ופרופסור רוסמורי נילסן מ-Norwegian Computing Center בשנת 1960.
2. אלטרנטיבות: י