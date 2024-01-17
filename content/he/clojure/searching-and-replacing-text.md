---
title:                "חיפוש והחלפת טקסטים"
html_title:           "Clojure: חיפוש והחלפת טקסטים"
simple_title:         "חיפוש והחלפת טקסטים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט הם שני פעולות חשובות למתכנתים. חיפוש מאפשר לנו למצוא טקסט מסוים במחרוזת או קובץ והחלפה מאפשרת לנו להחליף את הטקסט הזה בטקסט אחר. תהליך זה מאפשר לנו לשנות או לעדכן קוד קיים עם דיוק ויעילות.

## כיצד לעשות זאת:

כדי לחפש ולהחליף טקסט בשפת Clojure, ניתן להשתמש בפונקציות "replace" ו- "replace-first". ראשית, צריך לייבא את המודול "java.util.regex" על מנת לגשת לכלי הסינטקס של חיפוש והחלפה של Java. לדוגמה, הנה קוד שמחפש ומחליף את אות הראשונה "a" במחרוזת "abc" ל "x":

```clojure
(require '[java.util.regex :as rx])

(def str "abc")
(rx/replace-first str #"a" "x")
; output: "xbc"
```
כאן אנו משתמשים בפונקציה "require" כדי לייבא את המודול ובפונקציית "replace-first" כדי להחליף את הטקסט במחרוזת. ניתן גם להשתמש בפונקציה "replace" כדי להחליף את כל ההתאמות במחרוזת.

## העמקה:
החיפוש וההחלפה של טקסט הם תהליכים נפוצים וחשובים בתכנות. בעבר, חיפוש והחלפה נעשו באופן ידני, אך עם התפתחות השפות תכנות וכלים מתקדמים, הפעולות הללו נכללו באופן ישיר בתוך השפה עצמה. פונקציות חיפוש והחלפה נמצאות גם בשפות אחרות כמו Python ו-Java.

## ראה גם:
- [Clojure Docs - java.util.regex](https://clojure.org/reference/java_interop#_regular_expressions)
- [Java Regex Tutorial](https://www.javatpoint.com/java-regex)