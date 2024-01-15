---
title:                "כתיבה רווחת של מחרוזת"
html_title:           "Java: כתיבה רווחת של מחרוזת"
simple_title:         "כתיבה רווחת של מחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# למה

למה שאדם ירצה לכתוב קוד כדי להפוך את האותיות במחרוזת לאותיות ראשונות גדולות.

## איך לעשות

כדי להפוך את האותיות במחרוזת לאותיות ראשונות גדולות, יש לבצע את השלבים הבאים:

```Java
// הגדרת מחרוזת המכילה את המילים "hello world"
String str = "hello world";

// שימוש בפונקציית replace כדי להפוך את האות הראשונה של המחרוזת לאות ראשונה גדולה
str = str.replace(str.charAt(0), Character.toUpperCase(str.charAt(0)));

// הדפסת המחרוזת המעודכנת
System.out.println(str);

// פלט: Hello world
```

## מעמקים

המתן טיפוס מחרוזת מייצג את מחרוזת קיימת בזיכרון. כאשר אנו מפעילים את הפונקציה `replace`, אנו מזין לה תו מסוים ותו חדש, והפונקציה מחזירה את המחרוזת עם התו המסוים מוחלף בתו החדש. כאשר אנו משתנים את ערך המשתנה `str` לתוצאת הפונקציה, אנו מעדכנים את המחרוזת בזיכרון.

# ראו גם

- [מדריך לפונקציות בשפת ג'אווה](https://www.w3schools.com/java/java_functions.asp)
- [תיעוד שפת ג'אווה](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char)