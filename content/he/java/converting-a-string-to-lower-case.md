---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
המרת מחרוזת לאותיות קטנות היא תהליך שבו אנחנו משנים את האותיות הגדולות במחרוזת לאותיות קטנות. מתכנתים עושים את זה לשם אחידות בעבודה עם מחרוזות, טיפול בנתונים כניסה והפחתת שגיאות של מתכנית.

## איך לעשות:
כאן דוגמה של קוד ב- Java להמרת מחרודת לאותיות קטנות:
```Java
String str = "Hello, World!";
str = str.toLowerCase();
System.out.println(str);
```
הפלט של הקוד הוא:
```
hello, world!
```

## בעומק:
1. תוכן היסטורי - ב־Java, המתודה toLowerCase של המחלקה String אחראית להמרת אותיות גדולות לאותיות קטנות.
2. חלופות - אם אין לך גישה למתודה toLowerCase, אתה יכול ליצור קוד בחסר של Java שמבצע את אותו ביצוע.
3. פרטים על היישום - המתודה toLowerCase מממשת את אלגוריתם שימושי ידוע כאלגוריתם נרדף להמרת אותיות גדולות לאותיות קטנות.

## ראה גם:
תיעוד רשמי של מתודה toLowerCase: 
[Oracle's Java Documentation on toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--) 

מדריך של W3schools למתודה toLowerCase: 
[W3Schools guide on Java's toLowerCase() method](https://www.w3schools.com/java/ref_string_tolowercase.asp)