---
title:    "Java: המרת מחרוזת לאותיות קטנות"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Why - למה: בדיקת אופי הטקסט חשובה לכל תוכנת תכנות, והמרה של מחרוזת לאותיות קטנות היא חלק חשוב מכך. ניתן להשתמש בפונקציה זו למגוון מטרות, כגון השוואת טקסטים ועיבוד מידע.

How To - איך לעשות זאת: באמצעות הפונקציה toLowerCase() של Java ניתן להמיר מחרוזת לאותיות קטנות בקלות. כדי להשתמש בפונקציה זו, השתמשו בקוד הבא:

```Java
String str = "HELLO WORLD";
String lowerCaseStr = str.toLowerCase();
System.out.println(lowerCaseStr);
```

פלט: hello world

Deep Dive - לכיוון העומק: כדי להבין מדוע חשוב להמיר מחרוזת לאותיות קטנות, ניתן להתמקד בנקודה הבאה - מחרוזות שונות עם אותיות גדולות וקטנות יכולות להיות נחשבות להבדלים ביניהן. זה יכול לגרום לתוכנית שלכם לצאת לא נכונה, לריבוי קודים ולעומס של זמן ריצה.

כשנעשה המרה לאותיות קטנות, החישובים יהיו ידידותיים יותר ונתונים יתקבלו בכבוד רב יותר. בנוסף, כאשר משווים שני טקסטים, מרמז מותאם למחרוזת זו לא תשיג שגיאות בגלל אותיות גדולות וקטנות מתועבות.

#### ראו גם

- [מסמך Java על פונקציות מרחב שם](https://docs.oracle.com/javase/tutorial/java/index.html)
- [כיצד להשתמש בפונקציות מרחב שם ב-Java](https://www.geeksforgeeks.org/java/)
- [הבדלים בין אותיות גדולות לקטנות בפונקציות מחרוזת](https://alvinalexander.com/blog/post/java/differences-between-upper-case-lower-case-strings-java/)