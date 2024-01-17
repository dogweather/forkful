---
title:                "מציאת אורך של מחרוזת"
html_title:           "Clojure: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
למצוא את אורך המחרוזת הוא פעולה פשוטה וחשובה בתכנות. אורך המחרוזת מציין את מספר התווים שבה ומאפשר למתכנתים לעבוד עם מחרוזות בצורה יעילה יותר. זה משמש לדוגמה בתחומים כמו טפסים ומסדי נתונים, כאשר נדרש לטפל בקלטים של משתמשים בעודם בצורת מחרוזת.

## איך לעשות זאת:
כדי למצוא את אורך המחרוזת, ניתן להשתמש בפונקציה ```count``` שמקבלת כפרמטר את המחרוזת ומחזירה את מספר התווים בה. לדוגמה:

```
Clojure (count "Hello World!")
```
פלט:
```
12
```

כמו כן, ניתן להשתמש גם בפונקציה ```(.length (str "Hello World!"))``` שבעזרתה ניתן להמיר את המחרוזת לאובייקט ולקבל את האורך שלה. 

## צלילת עומק:
בעבר, אורך המחרוזת היה בעיקר מוגבל על ידי אורך המילים המופרדת בתוכה. אולם, עם פתח התכנות האובייקט אוריינטד, נוצרו פונקציות כמו ```count``` שמאפשרות למתכנתים לטפל במחרוזות בצורה יעילה יותר. פונקציות אלה קיימות גם בשפות תכנות אחרות, כגון Python ו-Java.

##ראה גם:
- [פונקציית ```count``` באתר Clojure](https://clojuredocs.org/clojure.core/count)
- [כיצד למצוא אורך של מחרוזת ב-Python](https://www.w3schools.com/python/ref_func_len.asp)
- [כיצד למצוא אורך של מחרוזת ב-Java](https://www.geeksforgeeks.org/string-length-method-in-java/)