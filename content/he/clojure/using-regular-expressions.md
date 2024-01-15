---
title:                "שימוש בביטויי רגילים"
html_title:           "Clojure: שימוש בביטויי רגילים"
simple_title:         "שימוש בביטויי רגילים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

למה נשתמש בביטויי רגלרקסיה? לפני כל דבר, נראה שהביטוי הזה נשאל מאוד במהלך תוכנית לודקש או כדי למצוא פריט של מילה מסוימת בטקסט ארוך.

## איך לעשות זאת

כדי להתחיל להשתמש בביטויי רגלרקסיה בקלות נעבוד עם הספריה של לודקש (Clojure). הספריה הזו מציעה לנו את הפונקציה re-seq כדי לחפש תבניות בטקסט ולהחזיר חלקים שלו בתור מילון, ולכן עלינו להכיר את השיטה של הפונקציה הזו.

```Clojure
(require '[clojure.string :as str])
(require '[clojure.string :refer [re-seq]])

(def txt "Hello, my name is John.")

(def pattern #"Hello, my name is (.+).")

(def matches (re-seq pattern txt))

(str/join ", " matches)

;; Output:
;; "John"
```

## לפנות יותר עמוק

בנוסף לפונקציה re-seq, הספריה של לודקש מציעה גם את הפונקציות re-find, re-matches ו-re-groups. כל אחת מהן משתמשת בדרך אחרת לחפש תבניות בטקסט ולהחזיר חלקים שלו בצורת מילון.

בנוסף, אם נרצה להימנע משימוש בפונקציות מובנות וליצור ביטוי רגלרקסיה ממשל, אפשר להשתמש במתכנתים כמו Infix שמאפשרים לנו לכתוב במקום זאת ביטוי רגלרקסיה.

# ראו גם

- [Clojure.org: Regular Expressions](https://clojure.org/guides/regular_expressions)
- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)