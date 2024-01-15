---
title:                "קישור מחרוזות"
html_title:           "Clojure: קישור מחרוזות"
simple_title:         "קישור מחרוזות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה?

String Concatenation היא פעולה חשובה ל-code formatting וכתיבת הקוד הברור והקריא. באמצעות חיבור תווים, מתבצעת היכולת להגדיר קבועים דינמיים וליצור תנאים הנבוכים ביותר.

## איך לבצע?

לפניכם מספר דוגמאות קוד כדי להדגים כיצד לבצע String Concatenation ב-Clojure.

```Clojure
; דוגמה 1
(def name "יניב")
(def greeting (str "שלום, " name "!" ))
(prn greeting) ;output: שלום, יניב!

; דוגמה 2
(def width 6)
(def area (str "האזור של הריבוע הוא " width " על " width " =" (* width width)))
(println area) ;output: האזור של הריבוע הוא 6 על 6 = 36

; דוגמה 3
(def nums [1 2 3])
(def joined (apply str nums))
(prn joined) ;output: 123
```

## חפירה עמוקה

String Concatenation היא פעולה שמשתמשת בקורסורים כדי לחבר תווים יחד לסדרת תווים אחת. ב-Clojure, ניתן להשתמש בפונקציות כמו str, join,  ו-apply כדי לבצע String Concatenation. בנוסף, Clojure תומך בכתיבת פורמטים מתקדמים עבור String Concatenation כדי לאפשר יצירת תבניות דינמיות יותר של סדרות תווים.

## ראו גם

- [Official Clojure Documentation for String Concatenation](https://clojuredocs.org/clojure.core/str)
- [A Beginner's Guide to Clojure Programming](https://www.freecodecamp.org/news/a-beginners-guide-to-clojure-programming-7f8f9e0a85f4/)
- [Mastering Clojure: Strings](https://vvvvalvalval.github.io/posts/clojure-strings.html)