---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Java: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

למה אדם יעסוק בהמרת מחרוזת לאותיות קטנות? קיצר כדי לעבוד עם טקסט באופן יעיל יותר, במיוחד כאשר יש צורך להשוות בין שני מחרוזות.

## כיצד לעשות זאת

```Java
String str = "This is a STRING";
System.out.println(str.toLowerCase());
```

פלט:

```Java
this is a string
```

כדי להמיר מחרוזת לאותיות קטנות ב-Java, ניתן להשתמש בפונקציה `toLowerCase()`, שהיא חלק מטיפוס המחרוזת. הפונקציה תחזיר את המחרוזת המקורית עם כל האותיות הגדולות הוחלפו באותיות קטנות.

## חפירה מעמיקה

העברת מחרוזת לאותיות קטנות היא פעולה פשוטה ונפוצה בבניית תוכניות ב-Java. כאשר משווים בין שני מחרוזות, השוואה תתאפשר רק אם המחרוזת המשוית היא באותיות קטנות, אחרת היא לא תתאפשר בצורה נכונה כיוון ש-Java מגדירה אותיות גדולות וקטנות כמתאימות למחרוזת שונה.

## ראה גם

- [פונקציות מובנות בטיפוס String ב-Java](https://www.javatpoint.com/java-string-functions)
- [המרת אותיות גדולות לאותיות קטנות וההפכויות ב-Java](https://www.geeksforgeeks.org/uppercase-lowercase-string-java/)